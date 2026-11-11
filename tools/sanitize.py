"""Turn raw SAS output into a sanitised conformance `expected/` directory.

Used by tools/refresh_gold.py and by the migration scripts.  Also provides the
forbidden-word scanner that must pass before anything enters git.

    python -I tools/sanitize.py scan <path> [<path> ...]       # exit 1 on a hit
    python -I tools/sanitize.py build <run_dir> <case_dir> [--encoding latin-1]

`run_dir` is the working directory of one SAS run (program.log, program.lst,
rc, ds/<mem>.sas7bdat, ds/__attr.csv and any files the program wrote).
`build` writes <case_dir>/expected/ (layout: case_format.md in the
conformance repository) and returns the SAS return code.

Sanitising rules (applied to every text that is stored):
  * the SAS log start-up block (copyright, licence, site, host/OS, analytical
    products, initialisation timings) is dropped; only key diagnostics are
    kept (log_notes.txt) together with the PUT output (put.txt);
  * absolute paths are rewritten: the run directory becomes `.`, any other
    host path (/mnt, /home, /opt, /tmp, ... prefixes) becomes `<path>`;
  * PROC CONTENTS environment fields (creation/modification time, the
    Engine/Host Dependent Information block) are masked in the listing;
  * timing lines (real time / cpu time) and the HTML body-file NOTE are dropped.
The scanner checks built-in patterns (the SAS licence line and site number) plus a private
deny list (organisation names, private domains and paths, user names) read from
the file named by $SASONR_FORBIDDEN or by the private config (see
refresh_gold.py).  The deny list itself is never stored in this repository.
"""
import csv
import hashlib
import io
import json
import os
import re
import shutil
import sys

FORMAT_VERSION = 1

# ---------------------------------------------------------------------------
# configuration (private; outside this repository)
# ---------------------------------------------------------------------------


def private_config():
    """The private settings file: $SASONR_CONF_CONFIG or ../sasonr-conformance/tools/config.json."""
    p = os.environ.get("SASONR_CONF_CONFIG")
    if not p:
        here = os.path.dirname(os.path.abspath(__file__))
        p = os.path.join(here, "..", "..", "sasonr-conformance", "tools", "config.json")
    if os.path.exists(p):
        with open(p) as f:
            return json.load(f)
    return {}


# "Site " is matched when a number follows (the SAS licence line "..., Site 70nnnnnn."),
# so ordinary data such as "Site A" passes.
BUILTIN_FORBIDDEN = [(r"Licen" + r"sed to", 0), (r"\bSite \d{3,}", 0)]


def forbidden_patterns():
    pats = [re.compile(p, f) for p, f in BUILTIN_FORBIDDEN]
    path = os.environ.get("SASONR_FORBIDDEN") or private_config().get("forbidden_patterns")
    if not path or not os.path.exists(path):
        raise SystemExit("scan: private deny list not found (set SASONR_FORBIDDEN or the private config)")
    for line in open(path, encoding="utf-8"):
        line = line.strip()
        if line and not line.startswith("#"):
            pats.append(re.compile(line, re.I))
    return pats


# ---------------------------------------------------------------------------
# reading
# ---------------------------------------------------------------------------


def read_lines(path, encoding="latin-1"):
    if not os.path.exists(path):
        return []
    with open(path, encoding=encoding, errors="replace", newline="") as f:
        txt = f.read()
    txt = txt.replace("\r\n", "\n")
    lines = txt.split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    return lines


def write_lines(path, lines):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="\n") as f:
        for l in lines:
            f.write(l + "\n")


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        h.update(f.read())
    return h.hexdigest()


# ---------------------------------------------------------------------------
# SAS log: start-up block, key messages, PUT output
# ---------------------------------------------------------------------------


def sas_log_body(lines):
    """Drop the start-up block (everything up to the SAS initialization timing)."""
    for i, l in enumerate(lines):
        if re.match(r"\s+cpu time", l) and any("SAS initialization used" in x for x in lines[max(0, i - 3):i]):
            body = lines[i + 1:]
            break
    else:
        body = lines
    # the WORK-dump epilogue starts at the runner's marker line; logs made
    # before the marker existed end with the dump's PROC EXPORT messages
    if EPILOGUE_MARK in body:
        body = body[:body.index(EPILOGUE_MARK)]
    else:
        for i, x in enumerate(body):
            if re.match(r"^\d+ records created in", x) and all(
                    _export_tail(y) for y in body[i:]):
                body = body[:i]
                break
    return [x for x in body if not x.startswith("NOTE: Writing HTML Body file:")]


EPILOGUE_MARK = "__SASONR_EPILOGUE__"
EXPORT_WORDS = {"records", "created", "in", "from"}


def _export_tail(line):
    return all(t.isdigit() or t in EXPORT_WORDS or t.startswith("/") or t.startswith("WORK.")
               for t in line.split())


KEY = [
    r"NOTE: The data set WORK\.\S+ has \d+ observations and \d+ variables\.",
    r"NOTE: There were \d+ observations read from the data set WORK\.",
    r"NOTE: Missing values were generated",
    r"NOTE: Mathematical operations could not be performed",
    r"NOTE: Variable \S+ is uninitialized\.",
    r"NOTE: (Character|Numeric) values have been converted",
    r"NOTE: Division by zero detected",
    r"NOTE: Invalid .*(argument|data)",
    r"NOTE: LOST CARD\.",
    r"NOTE: SAS went to a new line",
    r"NOTE: \d+ (observations with duplicate key values|duplicate observations) were deleted\.",
    r"NOTE: Table WORK\.\S+ created, with",
    r"NOTE: The query requires remerging",
    r"NOTE: \d+ rows? (was|were) (inserted into|updated in|deleted from)",
    r"NOTE: No rows were selected\.",
    r"NOTE: No observations in data set WORK\.",
    r"WARNING: Apparent (symbolic reference|invocation of macro)",
    r"ERROR( \d+-\d+)?:",
]
KEY_RE = re.compile("|".join(f"(?:{k})" for k in KEY))
ENV_NOISE = re.compile(r"SASUSER|parameter catalog|registry", re.I)


def norm_msg(parts):
    head = re.sub(r"\s+", " ", parts[0]).strip()
    rest = []
    for p in parts[1:]:
        p = p.strip()
        if p.startswith("WHERE "):
            p = "WHERE " + re.sub(r"[\s()]", "", p[6:]).lower()
        rest.append(re.sub(r"\s+", " ", p))
    if head.startswith("NOTE: Missing values") or head.startswith("NOTE: Mathematical") or "converted" in head:
        rest = [r for r in rest if not r.startswith("missing values.")]
        rest = [" ".join(" ".join(rest).split())]
    return " | ".join([head] + [r for r in rest if r])


def key_messages(lines):
    """Key NOTE/WARNING/ERROR messages, normalised (identical to compare4.py)."""
    out, i = [], 0
    while i < len(lines):
        l = lines[i]
        if KEY_RE.match(l) and not ENV_NOISE.search(l):
            parts = [l]
            j = i + 1
            while j < len(lines) and re.match(r" {6}\S", lines[j]) and not lines[j].startswith("      real time"):
                parts.append(lines[j])
                j += 1
            if l.startswith("ERROR"):
                parts = parts[:1]
            out.append(norm_msg(parts))
            i = j
            continue
        i += 1
    return out


DIAG_RE = re.compile(r"^(NOTE|WARNING|ERROR|INFO)( \d+-\d+)?(\(.*?\))?:")
SOURCE_RE = re.compile(r"^\d+(\s|$)")
MACRO_TRACE_RE = re.compile(r"^(MPRINT|MLOGIC|SYMBOLGEN|MACROGEN|MAUTOCOMPLOC)\b")
MARKER_RE = re.compile(r"^\s+[_\d\s]+$")


def put_lines(lines):
    """PUT / %PUT output (and data-error dumps) of a log body.

    Dropped: source echo (numbered lines), error markers under source lines,
    macro traces, NOTE/WARNING/ERROR/INFO messages and their indented
    continuation lines, form feeds and blank lines.  Trailing blanks removed.
    """
    out = []
    cont = False
    prev_src = False
    for l in lines:
        l = l.replace("\f", "")
        if not l.strip():
            # blank lines may sit inside a NOTE's indented continuation
            continue
        if DIAG_RE.match(l):
            cont, prev_src = True, False
            continue
        if cont and re.match(r" {6}", l):
            continue
        cont = False
        if SOURCE_RE.match(l) or MACRO_TRACE_RE.match(l):
            prev_src = True
            continue
        if prev_src and MARKER_RE.match(l):
            continue
        prev_src = False
        out.append(l.rstrip())
    return out


# ---------------------------------------------------------------------------
# listing
# ---------------------------------------------------------------------------

HOST_LINE = re.compile(
    r"^\s*(Engine/Host Dependent Information|Data Set Page Size|Number of Data Set Pages|First Data Page|"
    r"Max Obs per Page|Obs in First Data Page|Number of Data Set Repairs|Filename|Release Created|"
    r"Host Created|Inode Number|Access Permission|Owner Name|File Size|File Size \(bytes\)|"
    r"Index File Page Size|Number of Index File Pages)(\s{2,}|$)")
TIME_LINE = re.compile(r"^(\s*(?:Created|Last Modified)\s+)\d\d/\d\d/\d{4} \d\d:\d\d:\d\d")


def mask_listing(lines):
    out = []
    for l in lines:
        if HOST_LINE.match(l):
            continue
        out.append(TIME_LINE.sub(r"\1<datetime>", l).rstrip())
    while out and not out[-1].strip():
        out.pop()
    return out


# ---------------------------------------------------------------------------
# redaction
# ---------------------------------------------------------------------------

# Host paths only (system/user/mount prefixes, plus site prefixes from the
# private config `host_path_prefixes`), so data values such as
# "/Icons/brush.gif" are left alone.
def _abs_path_re():
    extra = [re.escape(p.strip("/")) for p in private_config().get("host_path_prefixes", [])]
    pref = "|".join(["mnt", "home", "opt", "tmp", "var", "srv", r"sas\w*", "users?", r"u\d+", r"data\d*"] + extra)
    return re.compile(r"(?<![\w.~])/(?:" + pref + r")/(?:[A-Za-z0-9_.$@+-]+/)*[A-Za-z0-9_.$@+-]*")


ABS_PATH = _abs_path_re()


def redactor(run_dirs):
    """Function that rewrites absolute paths (run directory -> '.')."""
    roots = sorted({d.rstrip("/") for d in run_dirs if d}, key=len, reverse=True)

    def f(s):
        for r in roots:
            s = s.replace(r + "/", "./").replace(r, ".")
        return ABS_PATH.sub("<path>", s)

    return f


# ---------------------------------------------------------------------------
# CSV helpers
# ---------------------------------------------------------------------------


def read_csv_rows(path, encoding="latin-1"):
    with open(path, encoding=encoding, errors="replace", newline="") as f:
        return list(csv.reader(f))


def write_csv_rows(path, rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        for r in rows:
            w.writerow(r)


# ---------------------------------------------------------------------------
# meta.yaml (flat subset: key: value, key: [a, b])
# ---------------------------------------------------------------------------

META_ORDER = ["case", "title", "sas_version", "generated", "sas_rc", "tags", "program_sha256", "program_ref",
              "source", "encoding", "hex", "cwd", "options", "known_fail", "known_reason"]


def _yaml_scalar(v):
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, (int, float)):
        return str(v)
    s = str(v)
    if s == "" or re.search(r"[:#\[\]{},&*!|>'\"%@`]|^\s|\s$", s):
        return json.dumps(s, ensure_ascii=False)
    return s


def write_meta(path, meta):
    keys = [k for k in META_ORDER if k in meta] + sorted(k for k in meta if k not in META_ORDER)
    lines = []
    for k in keys:
        v = meta[k]
        if v is None:
            continue
        if isinstance(v, (list, tuple)):
            lines.append(f"{k}: [" + ", ".join(_yaml_scalar(x) for x in v) + "]")
        else:
            lines.append(f"{k}: {_yaml_scalar(v)}")
    write_lines(path, lines)


def _parse_scalar(s):
    s = s.strip()
    if s.startswith('"'):
        return json.loads(s)
    if s in ("true", "false"):
        return s == "true"
    if re.fullmatch(r"-?\d+", s):
        return int(s)
    return s


def read_meta(path):
    meta = {}
    if not os.path.exists(path):
        return meta
    for line in open(path, encoding="utf-8"):
        line = line.rstrip("\n")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        k, _, v = line.partition(":")
        v = v.strip()
        if v.startswith("[") and v.endswith("]"):
            inner = v[1:-1].strip()
            items = []
            if inner:
                for tok in next(csv.reader([inner], skipinitialspace=True)):
                    items.append(_parse_scalar(tok if not tok.startswith('"') else json.dumps(tok)))
            meta[k.strip()] = items
        else:
            meta[k.strip()] = _parse_scalar(v)
    return meta


# ---------------------------------------------------------------------------
# build expected/ from one SAS run directory
# ---------------------------------------------------------------------------

SKIP_TOP = {"program.sas", "program.log", "program.lst", "rc", "ds", "html", "lib", ".sasuser", "data", "out"}
TEXT_EXT_SKIP = (".html", ".htm", ".rtf", ".pdf", ".xlsx", ".xls", ".sas7bdat", ".sas7bcat", ".sas7bndx",
                 ".png", ".svg", ".gif", ".jpg", ".css")


def sas_version(log_lines):
    for l in log_lines[:20]:
        m = re.search(r"Proprietary Software (\S+) \((TS\w+)", l)
        if m:
            return f"{m.group(1)} ({m.group(2)})"
    return ""


def written_files(run_dir, inputs=()):
    """Text files the program wrote in its working directory (relative paths)."""
    out = []
    for root, dirs, files in os.walk(run_dir):
        rel_root = os.path.relpath(root, run_dir)
        if rel_root == ".":
            dirs[:] = [d for d in dirs if d not in SKIP_TOP or d == "out"]
        for fn in files:
            rel = os.path.normpath(os.path.join(rel_root, fn))
            if rel_root == "." and fn in SKIP_TOP:
                continue
            if rel in inputs or fn.lower().endswith(TEXT_EXT_SKIP):
                continue
            p = os.path.join(root, fn)
            if os.path.getsize(p) > 5_000_000:
                continue
            with open(p, "rb") as f:
                if b"\0" in f.read(65536):
                    continue
            out.append(rel)
    return sorted(out)


def build_expected(run_dir, case_dir, encoding="latin-1", extra_roots=(), inputs=()):
    """Write case_dir/expected/ from a SAS run directory; return (rc, sas_version)."""
    red = redactor([run_dir] + list(extra_roots))
    exp = os.path.join(case_dir, "expected")
    if os.path.isdir(exp):
        shutil.rmtree(exp)
    os.makedirs(exp)
    log = read_lines(os.path.join(run_dir, "program.log"))
    body = sas_log_body(log)
    write_lines(os.path.join(exp, "log_notes.txt"), [red(x) for x in key_messages(body)])
    puts = put_lines(body)
    if puts:
        write_lines(os.path.join(exp, "put.txt"), [red(x) for x in puts])
    lst = mask_listing(read_lines(os.path.join(run_dir, "program.lst")))
    if any(x.strip() for x in lst):
        write_lines(os.path.join(exp, "listing.txt"), [red(x) for x in lst])
    ds = os.path.join(run_dir, "ds")
    if os.path.isdir(ds):
        for fn in sorted(os.listdir(ds)):
            if fn == "__attr.csv":
                rows = read_csv_rows(os.path.join(ds, fn))
                write_csv_rows(os.path.join(exp, "attrs.csv"), [[red(c) for c in r] for r in rows])
            elif fn.endswith(".sas7bdat"):
                # the data sets stay in SAS's own format: exact values and attributes
                os.makedirs(os.path.join(exp, "datasets"), exist_ok=True)
                shutil.copyfile(os.path.join(ds, fn), os.path.join(exp, "datasets", fn.lower()))
    for rel in written_files(run_dir, inputs):
        lines = [red(x.rstrip()) for x in read_lines(os.path.join(run_dir, rel), encoding)]
        write_lines(os.path.join(exp, "files", rel), lines)
    try:
        rc = int(open(os.path.join(run_dir, "rc")).read().strip())
    except (OSError, ValueError):
        rc = -1
    return rc, sas_version(log)


# ---------------------------------------------------------------------------
# scanner
# ---------------------------------------------------------------------------


def scan(paths, quiet=False):
    pats = forbidden_patterns()
    hits = 0
    for top in paths:
        walk = [(os.path.dirname(top), [], [os.path.basename(top)])] if os.path.isfile(top) else os.walk(top)
        for root, dirs, files in walk:
            dirs[:] = [d for d in dirs if d not in (".git",)]
            for fn in files:
                p = os.path.join(root, fn)
                try:
                    with open(p, "rb") as f:
                        data = f.read()
                except OSError:
                    continue
                if b"\0" in data[:65536]:
                    continue
                text = data.decode("utf-8", errors="replace")
                for k, line in enumerate(text.split("\n"), 1):
                    for pat in pats:
                        if pat.search(line):
                            hits += 1
                            if not quiet:
                                print(f"FORBIDDEN {p}:{k}: pattern #{pats.index(pat)}")
    return hits


def main(argv):
    if not argv:
        raise SystemExit(__doc__)
    if argv[0] == "scan":
        n = scan(argv[1:])
        print(f"scan: {n} hit(s)")
        raise SystemExit(1 if n else 0)
    if argv[0] == "build":
        enc = "latin-1"
        if "--encoding" in argv:
            enc = argv[argv.index("--encoding") + 1]
        rc, ver = build_expected(argv[1], argv[2], enc)
        print(json.dumps({"rc": rc, "sas_version": ver}))
        return
    raise SystemExit(__doc__)


if __name__ == "__main__":
    main(sys.argv[1:])
