"""Refresh the SAS gold standard of offline conformance cases.

    python -I tools/refresh_gold.py [--force] [--dry-run] [--parallel N] [--rebuild STAMP] <case-or-area dir> [...]

--rebuild STAMP re-creates expected/ from an earlier run in the transit
directory (refresh/STAMP) without running SAS (e.g. after a sanitiser change).

A case directory holds program.sas (or meta.yaml `program_ref`, resolved against
the private `books_program_root`) and optionally data/.  A case is (re)run only
when expected/ is missing or meta.yaml `program_sha256` differs from the
program (or with --force).  Cases are staged in a transit directory visible to
the machine that runs SAS and executed by tools/sas/run_cases.sh through a
pluggable runner client (at most `max_jobs` jobs, one SAS session per job at a time unless
--parallel; every session has its own SASUSER), then sanitised into expected/
(tools/sanitize.py) and scanned for forbidden words.  A case whose expected/
contains a forbidden word is rolled back and reported.

All site-specific settings (transit paths, SAS executable, runner client,
deny list) come from the private config file named by $SASONR_CONF_CONFIG
(default ../sasonr-conformance/tools/config.json); nothing site-specific is
stored in this repository.  The service URL and API key are never printed.
"""
import datetime
import importlib.util
import os
import re
import shutil
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import sanitize  # noqa: E402


def load_client(cfg):
    spec = importlib.util.spec_from_file_location("sasjob", cfg["sasjob"])
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.Client()


def find_cases(paths):
    out = []
    for p in paths:
        p = os.path.abspath(p)
        for root, dirs, files in os.walk(p):
            dirs[:] = sorted(d for d in dirs if d not in ("expected", "data", ".git"))
            if "program.sas" in files or ("meta.yaml" in files and "program_ref" in sanitize.read_meta(os.path.join(root, "meta.yaml"))):
                out.append(root)
                dirs[:] = []
    return sorted(set(out))


def program_path(case, meta, cfg):
    if meta.get("program_ref"):
        return os.path.join(cfg["books_program_root"], meta["program_ref"], "program.sas")
    return os.path.join(case, "program.sas")


def header_info(prog):
    """tags / source / title from the leading comment of program.sas."""
    info = {}
    txt = open(prog, encoding="latin-1").read(4000)
    m = re.match(r"\s*/\*(.*?)\*/", txt, re.S)
    if not m:
        return info
    lines = [l.strip() for l in m.group(1).split("\n")]
    if lines and lines[0]:
        info["title"] = lines[0]
    for l in lines:
        k, _, v = l.partition(":")
        k = k.strip().lower()
        if k == "tags":
            info["tags"] = [t.strip() for t in re.split(r"[,\s]+", v) if t.strip()]
        elif k == "source":
            info["source"] = v.strip()
    return info


def case_name(case):
    """Case name relative to tests/conformance/ or cases/ (e.g. functions/ROUND/ROUND_001)."""
    p = case.replace(os.sep, "/")
    for marker in ("/tests/conformance/", "/cases/"):
        if marker in p:
            return p.split(marker, 1)[1]
    return os.path.basename(p)


def case_id(case, base):
    return re.sub(r"[^A-Za-z0-9_.-]", "_", os.path.relpath(case, base).replace(os.sep, "__"))


def main(argv):
    force = "--force" in argv
    dry = "--dry-run" in argv
    par = 1
    if "--parallel" in argv:
        par = int(argv[argv.index("--parallel") + 1])
    rebuild = argv[argv.index("--rebuild") + 1] if "--rebuild" in argv else None
    force = force or rebuild is not None
    paths = [a for i, a in enumerate(argv)
             if not a.startswith("--") and not (i > 0 and argv[i - 1] in ("--parallel", "--rebuild"))]
    if not paths:
        raise SystemExit(__doc__)
    cfg = sanitize.private_config()
    if not cfg:
        raise SystemExit("private config not found (set SASONR_CONF_CONFIG)")
    cases = find_cases(paths)
    todo = []
    for c in cases:
        meta = sanitize.read_meta(os.path.join(c, "meta.yaml"))
        prog = program_path(c, meta, cfg)
        sha = sanitize.sha256_file(prog)
        if force or not os.path.isdir(os.path.join(c, "expected")) or meta.get("program_sha256") != sha:
            todo.append((c, meta, prog, sha))
    print(f"cases: {len(cases)}  to run: {len(todo)}")
    if dry or not todo:
        for c, *_ in todo:
            print("  ", c)
        return 0

    stamp = rebuild or datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    base = os.path.commonpath([c for c, *_ in todo]) if len(todo) > 1 else os.path.dirname(todo[0][0])
    loc = os.path.join(cfg["transit_local"], "refresh", stamp)
    sas_root = cfg["transit_sas"] + "/refresh/" + stamp
    src_l, gold_l = os.path.join(loc, "src"), os.path.join(loc, "gold")
    ids = {case_id(c, base): (c, meta, prog, sha) for c, meta, prog, sha in todo}
    if not rebuild:
        submit(cfg, ids, loc, sas_root, src_l, gold_l, par)
    collect(cfg, ids, loc, sas_root, gold_l)


def submit(cfg, ids, loc, sas_root, src_l, gold_l, par):
    os.makedirs(src_l)
    os.makedirs(gold_l)
    for cid, (c, meta, prog, sha) in ids.items():
        d = os.path.join(src_l, cid)
        os.makedirs(d)
        shutil.copyfile(prog, os.path.join(d, "program.sas"))
        data = os.path.join(os.path.dirname(prog), "data")
        if os.path.isdir(data):
            shutil.copytree(data, os.path.join(d, "data"))
    roots = {}
    for cid, (c, meta, prog, sha) in ids.items():
        if meta.get("cwd") == "_root":
            area_root = os.path.join(os.path.dirname(c), "_root")
            if area_root not in roots:
                name = "_root_%d" % len(roots)
                shutil.copytree(area_root, os.path.join(loc, name))
                roots[area_root] = sas_root + "/" + name
            open(os.path.join(src_l, cid, "cwd"), "w").write(roots[area_root] + "\n")
    for f in ("run_cases.sh", "dump_work_sas7.sas"):
        shutil.copyfile(os.path.join(HERE, "sas", f), os.path.join(loc, f))
    os.chmod(os.path.join(loc, "run_cases.sh"), 0o777)
    names = sorted(ids)
    njobs = max(1, min(cfg.get("max_jobs", 3), len(names)))
    parts = [names[k::njobs] for k in range(njobs)]
    client = load_client(cfg)
    jobs = []
    for k, part in enumerate(parts):
        lst = os.path.join(loc, f"_list_{k}.txt")
        open(lst, "w").write("\n".join(part) + "\n")
        sh = os.path.join(loc, f"_run_{k}.sh")
        open(sh, "w").write(
            "#!/bin/bash\n"
            f"bash {sas_root}/run_cases.sh {sas_root}/src {sas_root}/gold {sas_root}/_list_{k}.txt "
            f"{cfg['sas_bin']} {sas_root}/dump_work_sas7.sas {par} 300\n")
        os.chmod(sh, 0o777)
        timeout = 900 + int(60 * len(part) / max(par, 1))
        j = client.submit("shell", {"script_path": f"{sas_root}/_run_{k}.sh"}, label=f"sasonr_refresh_{k}",
                          run_timeout_sec=timeout)
        jobs.append((j["job_id"], timeout))
        print(f"job {k + 1}/{njobs}: {len(part)} case(s)", flush=True)
    for jid, timeout in jobs:
        j = client.wait(jid, timeout=timeout + 1800)
        print("job status:", j.get("status"), flush=True)


def collect(cfg, ids, loc, sas_root, gold_l):
    today = datetime.date.today().isoformat()
    bad = []
    for cid, (c, meta, prog, sha) in sorted(ids.items()):
        run_dir = os.path.join(gold_l, cid)
        if not os.path.exists(os.path.join(run_dir, "rc")):
            bad.append((c, "no SAS output"))
            continue
        enc = meta.get("encoding", "latin-1")
        rc, ver = sanitize.build_expected(run_dir, c, "latin-1" if enc in ("latin1", "latin-1") else enc,
                                         extra_roots=[sas_root + "/gold/" + cid, sas_root, cfg["transit_sas"]]
                                         + [sas_root + "/_root_%d" % i for i in range(10)] + [
                                                      cfg["transit_local"]],
                                         inputs=(), markers=meta.get("layout") == "matrix")
        info = header_info(prog)
        new = dict(meta)
        new.setdefault("case", case_name(c))
        for k in ("title", "tags", "source"):
            if k in info and k not in new:
                new[k] = info[k]
        new.pop("hex", None)
        new.update(sas_version=ver, generated=today, sas_rc=rc, program_sha256=sha, gold_version=sanitize.GOLD_VERSION)
        new.setdefault("encoding", "latin1")
        sanitize.write_meta(os.path.join(c, "meta.yaml"), new)
        hits = sanitize.scan([os.path.join(c, "expected"), os.path.join(c, "meta.yaml")], quiet=True)
        if hits:
            shutil.rmtree(os.path.join(c, "expected"))
            bad.append((c, f"{hits} forbidden-word hit(s); expected/ removed"))
    print(f"refreshed: {len(ids) - len(bad)}  failed: {len(bad)}")
    for c, why in bad:
        print("  FAIL", c, why)
    print("run directory (transit, not in git):", os.path.relpath(loc, cfg["transit_local"]))
    if bad:
        sys.exit(1)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
