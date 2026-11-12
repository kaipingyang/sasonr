"""Round summary of the conformance results in at most 10 lines.

    python -I tools/round_summary.py RESULTS.csv [--approx APPROX.csv] [--base BASE.csv] [--decisions FILE]

RESULTS.csv   tools/run_conformance.R --out of the round (default strict mode)
--approx      the same runner with `--status approximate --strict false`:
              how close the approximate functions are (bit-identical cases)
--base        the results of the previous round (same runner): fail-loud
              cases that are new in this round
--decisions   a text file, one open decision per line (for the user)

Lines: bit-identical N/M (pass, or SAS and sasonr both stop with an ERROR) ·
silent mismatches (L-DS differs and sasonr did not fail loud) · SAS errored
but sasonr ran (lenient) · known_fail · approximate (strict = FALSE) · new
fail-loud items vs base · decisions needed.
"""
import collections
import csv
import re
import sys


def read(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def feature(case):
    m = re.match(r"^(fn_matrix|fmt_matrix|functions|fn_examples)/([^/]+)/", case)
    return m.group(2) if m else case.split("/")[0]


def main(argv):
    if not argv or argv[0].startswith("--"):
        raise SystemExit(__doc__)
    opts = {}
    rest = argv[1:]
    for k in range(0, len(rest) - 1, 2):
        opts[rest[k]] = rest[k + 1]
    rows = read(argv[0])
    n = len(rows)
    st = collections.Counter(r["status"] for r in rows)
    ident = st["pass"] + st["both_error"]
    silent = [r for r in rows if r["status"] == "fail" and r["ds"] == "FALSE"]
    known = [r for r in rows if r.get("known_fail")]
    out = ["bit-identical %d / %d (pass %d, both stop with an ERROR %d); fail loud %d, other differences %d, sasonr error %d"
           % (ident, n, st["pass"], st["both_error"], st["unsupported"], st["fail"] - len(silent), st["error"]),
           "silent mismatches (L-DS differs, no fail loud): %d%s" % (len(silent), (" - " + ", ".join(
               r["case"] for r in silent[:4])) if silent else ""),
           "SAS errored but sasonr ran (lenient): %d" % st["lenient"],
           "known_fail: %d" % len(known)]
    if "--approx" in opts:
        a = read(opts["--approx"])
        ok = sum(r["status"] in ("pass", "both_error") for r in a)
        feats = {feature(r["case"]) for r in a}
        out.append("approximate (strict = FALSE): bit-identical %d / %d cases of %d features; L-DS last-bit differences %d"
                   % (ok, len(a), len(feats), sum(r["status"] == "fail" and r["ds"] == "FALSE" for r in a)))
    else:
        out.append("approximate (strict = FALSE): not measured (--approx)")
    if "--base" in opts:
        base = {r["case"]: r["status"] for r in read(opts["--base"])}
        new = [r for r in rows if r["status"] == "unsupported" and base.get(r["case"]) != "unsupported"]
        strict = sum("sasonr.strict = FALSE" in r["detail"] for r in new)
        top = collections.Counter(feature(r["case"]) for r in new).most_common(5)
        out.append("new fail-loud vs base: %d (strict mode %d, other %d)%s" % (
            len(new), strict, len(new) - strict, (" - " + ", ".join("%s %d" % t for t in top)) if new else ""))
        gone = sum(1 for r in rows if base.get(r["case"]) == "unsupported" and r["status"] != "unsupported")
        if gone:
            out.append("fail loud resolved vs base: %d" % gone)
    else:
        out.append("new fail-loud vs base: not compared (--base)")
    dec = []
    if "--decisions" in opts:
        dec = [l.strip() for l in open(opts["--decisions"], encoding="utf-8") if l.strip() and not l.startswith("#")]
    out.append("decisions needed: %s" % ("; ".join(dec) if dec else "none"))
    print("\n".join(out[:10]))


if __name__ == "__main__":
    main(sys.argv[1:])
