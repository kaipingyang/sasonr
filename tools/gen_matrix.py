"""Generate the function and format verification matrices (one SAS program per function / format).

    python -I tools/gen_matrix.py functions [NAME ...]   # tests/conformance/fn_matrix/<FN>/
    python -I tools/gen_matrix.py formats [NAME ...]     # tests/conformance/fmt_matrix/<FMT>/
    python -I tools/gen_matrix.py where [NAME ...]       # tests/conformance/fn_where/<FN>/ (WHERE context)

Layout (case_format.md, "matrix layout"): per function or format one directory
with program.sas (all cases), cases.csv (case id, context, SAS expression,
setup statements, note, group) and meta.yaml (`layout: matrix`); the SAS gold
standard (expected/: one WORK.MATRIX sas7bdat with one column per case, the
log NOTEs with the case markers, the PUT output and results.txt) comes from
tools/refresh_gold.py and the per-case comparison from tools/run_conformance.R.
Each case is one step between `%put @@BEGIN <case> <context>;` and
`%put @@END <case>;`, so _ERROR_, the log NOTEs (with their line numbers) and
the PUT output are attributed to the case; cases expected to stop with an
ERROR (compile-time errors, group `isolated`) come last.  Cases are local
assets (tests/conformance/ is not in git).  All data are made up.

Functions: the calls of tools/fn_examples.txt (reference-page examples) and the
extra calls of tools/fn_matrix.txt, plus generated edge cases.  For each
argument position of the base call (the call with the most arguments) the
argument is replaced, one at a time, by edge values of its type (missing and
special missing values, 0, -0, negative, non-integer and extreme numbers,
blank, padded, long character values, and a value of the other type, which
SAS converts).  The argument types come from the SAS 9.4 function inventory
(argument-type mask; minimum and maximum argument counts), which is read from
the private conformance repository when present.  Too few / too many
arguments are compile-time errors and get their own case.

Every case assigns the call to a new variable r (so the result length is the
default length SAS gives the function) and writes one record to results.txt:
  @@N <case> type=N err=<_ERROR_> hex=<hex16.> best=<best32.>
  @@C <case> type=C err=<_ERROR_> vlen=<vlength> len=<length> lenc=<lengthc>,
  then @@Q (the value with $QUOTE.) and @@H (the bytes as $HEX.)
%SYSFUNC cases write `@@M <case> [<value>]` with %PUT; PROC SQL cases create
the table with SELECT and write the record from a DATA _NULL_ step.

Formats: tools/fmt_matrix.txt lists, per format or informat, the values and
widths; each case is r = put(value, fmt.) or r = input('text', informat.).
"""
import csv
import hashlib
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
INV = os.path.join(ROOT, "..", "sasonr-conformance", "data", "inventory")
FN_OUT = os.path.join(ROOT, "tests", "conformance", "fn_matrix")
FMT_OUT = os.path.join(ROOT, "tests", "conformance", "fmt_matrix")
WHERE_OUT = os.path.join(ROOT, "tests", "conformance", "fn_where")

L200 = "length s $200; s = repeat('ab', 99);"
L32K = "length s $32767; s = repeat('x', 32766);"

# edge values: (SAS text, setup, note)
NUM_FIRST = [(".", "", "missing"), (".A", "", "special missing .A"), ("._", "", "special missing ._"),
             ("0", "", "zero"), ("-0", "", "negative zero"), ("-1", "", "negative"), ("-2.5", "", "negative non-integer"),
             ("0.5", "", "non-integer"), ("1e300", "", "huge"), ("-1e300", "", "huge negative"),
             ("1e-300", "", "tiny"), ("'3'", "", "character argument (converted)"),
             ("'x'", "", "invalid character argument")]
NUM_OTHER = [(".", "", "missing"), (".A", "", "special missing .A"), ("0", "", "zero"), ("-1", "", "negative"),
             ("2.5", "", "non-integer"), ("1e300", "", "huge"), ("'3'", "", "character argument (converted)")]
CHR_FIRST = [("' '", "", "blank"), ("'   '", "", "three blanks"), ("'  ab  '", "", "leading and trailing blanks"),
             ("'a'", "", "length 1"), ("s", L200, "length 200"), ("s", L32K, "length 32767"),
             ("123", "", "numeric argument (converted)"), (".", "", "numeric missing argument (converted)")]
CHR_OTHER = [("' '", "", "blank"), ("'  ab  '", "", "leading and trailing blanks"),
             ("12", "", "numeric argument (converted)")]
KEY_SUBS = [("' '", "", "blank"), ("'xyz'", "", "invalid value")]

TYPES = {1: "N", 2: "C", 3: "A"}
INV_TYPES = {}  # function name -> "N"/"C" (filled from the inventory)


# ---------------------------------------------------------------------------
# inventory and specs
# ---------------------------------------------------------------------------

def read_inventory():
    out = {}
    p = os.path.join(INV, "sas_functions.csv")
    if not os.path.exists(p):
        return out
    with open(p, newline="") as f:
        for r in csv.DictReader(f):
            nm = r["fncname"].upper()
            if not nm or r["fnctype"] not in ("N", "C"):
                continue
            prev = out.get(nm)
            if prev is not None and prev["source"] == "B" and r["source"] != "B":
                continue
            out[nm] = r
    return out


def arg_type(mask, k):
    """Type of argument k (1-based) from the inventory bit mask (3 bits per argument; the last repeats)."""
    m = int(mask)
    groups = []
    while m:
        groups.append(m & 7)
        m >>= 3
    if not groups:
        return "A"
    g = groups[k - 1] if k <= len(groups) else groups[-1]
    return TYPES.get(g, "A")


def parse_examples(path):
    spec = {}
    cur = None
    for ln in open(path, encoding="utf-8"):
        ln = ln.rstrip("\n")
        if ln.startswith("## "):
            cur = ln[3:].strip()
            spec[cur] = {"calls": [], "glob": [], "pre": [], "mode": "data"}
            continue
        if cur is None or not ln.strip() or ln.startswith("#"):
            continue
        m = re.match(r"^(glob|pre|mode|note|desc): (.*)$", ln)
        if m:
            if m.group(1) in ("glob", "pre"):
                spec[cur][m.group(1)].append(m.group(2))
            elif m.group(1) == "mode":
                spec[cur]["mode"] = m.group(2)
            continue
        spec[cur]["calls"].append(ln.split(" => ")[0].strip())
    return spec


def parse_matrix(path):
    """tools/fn_matrix.txt: per function a header and extra calls.

    ## NAME [key=1,3] [ret=N|C] [ctx] [env] [nogeneric] [base=<call>]
    <SAS call>                      extra DATA step case
    <SAS call> ;; <setup>           with statements before the assignment
    sysfunc: %sysfunc(...)           %SYSFUNC case
    sql: <expr>                      PROC SQL case
    ce: <SAS call>                   compile-time error expected (own session)
    glob: <SAS code>                 code before the step, for every case
    """
    spec = {}
    cur = None
    for ln in open(path, encoding="utf-8"):
        ln = ln.rstrip("\n")
        if ln.startswith("## "):
            parts = ln[3:].split()
            cur = parts[0]
            d = {"key": set(), "ret": None, "ctx": False, "env": False, "nogeneric": False, "base": None,
                 "extra": [], "glob": [], "first_n": None}
            rest = ln[3:][len(parts[0]):].strip()
            mb = re.search(r"base=(.*)$", rest)
            if mb:
                d["base"] = mb.group(1).strip()
                rest = rest[:mb.start()]
            for p in rest.split():
                if p.startswith("key="):
                    d["key"] = {int(x) for x in p[4:].split(",")}
                elif p.startswith("ret="):
                    d["ret"] = p[4:]
                elif p in ("ctx", "env", "nogeneric"):
                    d[p] = True
            spec[cur] = d
            continue
        if cur is None or not ln.strip() or ln.startswith("#"):
            continue
        m = re.match(r"^(sysfunc|sql|ce|glob): (.*)$", ln)
        if m:
            if m.group(1) == "glob":
                spec[cur]["glob"].append(m.group(2))
            else:
                spec[cur]["extra"].append((m.group(1), m.group(2).strip(), ""))
            continue
        call, _, setup = ln.partition(" ;; ")
        spec[cur]["extra"].append(("data", call.strip(), setup.strip()))
    return spec


# ---------------------------------------------------------------------------
# SAS call text
# ---------------------------------------------------------------------------

def split_call(call):
    """'f(a, b(c, d), 'x,y')' -> ('f', ['a', 'b(c, d)', "'x,y'"]) or None."""
    m = re.match(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*$", call, re.S)
    if not m:
        return None
    name, body = m.group(1), m.group(2)
    args, depth, cur, q = [], 0, "", None
    for ch in body:
        if q:
            cur += ch
            if ch == q:
                q = None
            continue
        if ch in "'\"":
            q = ch
            cur += ch
        elif ch == "(":
            depth += 1
            cur += ch
        elif ch == ")":
            depth -= 1
            cur += ch
        elif ch == "," and depth == 0:
            args.append(cur.strip())
            cur = ""
        else:
            cur += ch
    if depth != 0 or q:
        return None
    if cur.strip() or args:
        args.append(cur.strip())
    return name, args


def join_call(name, args):
    return "%s(%s)" % (name.lower(), ", ".join(args))


def sysfunc_arg(a):
    """A literal argument as %SYSFUNC text, or None."""
    if re.match(r"^-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$", a) or a == ".":
        return a
    m = re.match(r"^'([^']*)'$", a)
    if m:
        s = m.group(1)
        if not s or s != s.strip() or re.search(r"[,()%&;\"']", s):
            return None
        return s
    return None


# ---------------------------------------------------------------------------
# programs
# ---------------------------------------------------------------------------

def record_lines(cid, rtype):
    lines = ["  file 'results.txt' mod lrecl=1000000;", "  __err = _error_;"]
    if rtype == "N":
        lines.append("  put '@@N %s type=N err=' __err 'hex=' r hex16. ' best=' r best32.;" % cid)
    else:
        lines += ["  __v = vlength(r); __l = length(r); __lc = lengthc(r);",
                  "  put '@@C %s type=C err=' __err 'vlen=' __v 'len=' __l 'lenc=' __lc;" % cid,
                  "  put '@@Q %s ' r $quote32767.;" % cid,
                  "  length __h $32767; __h = putc(substr(r,1,min(__v,16383)), cats('$hex', 2*min(__v,16383), '.'));",
                  "  put '@@H %s ' __h;" % cid]
    return lines


def ds_name(cid):
    """SAS data set / variable name of a case (case ids of character formats start with $)."""
    return cid.replace("$", "_")


def case_block(cid, ctx, expr, setup, rtype):
    """Lines of one case: %put @@BEGIN <case> <context>; one step (two for PROC SQL); %put @@END <case>;"""
    ds = ds_name(cid)
    out = ["%%put @@BEGIN %s %s;" % (cid, ctx)]
    if ctx == "sysfunc":
        out += ["%%let __r=%s;" % expr, "%%put @@M %s [&__r];" % cid]
    elif ctx == "where":
        # the DATA step value of the call is the reference; the WHERE step keeps the
        # one row when the WHERE value equals it (missing values compare equal only to
        # the same missing value), so the case's member has one row (r = the value)
        # or none, and the NOTEs of the WHERE evaluation are attributed to the case
        # the arguments of the call are variables of the input row, so that SAS
        # evaluates the function per observation (a call with constant arguments is
        # folded into a constant when the WHERE clause is compiled)
        out.append("data work.w_%s;" % ds)
        if setup:
            out.append("  " + setup)
        fn = cid.rsplit("_", 1)[0]
        hoist = {}
        top = split_call(expr)
        nxt = [len(top[1]) if top else 0]

        def where_call(e, at_top=False):
            # literal and expression arguments become variables w1, w2, ...; an
            # argument that calls the function under test is rewritten the same
            # way, so that the WHERE clause (not the DATA step) evaluates it
            # (lengthc(trim('  ')) -> lengthc(trim(w1)))
            pc = split_call(e)
            if not (pc and pc[1] and pc[0].lower() not in ("put", "input") and
                    not any(re.match(r"(?i)^of\s", a) for a in pc[1])):
                return e
            vars_ = []
            for k, a in enumerate(pc[1], 1):
                if not a or re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", a):
                    vars_.append(a)
                elif re.search(r"(?i)\b%s\s*\(" % re.escape(fn), a) and split_call(a):
                    vars_.append(where_call(a))
                else:
                    # top-level arguments keep their position (w2 = argument 2)
                    if not at_top:
                        nxt[0] += 1
                    j = k if at_top else nxt[0]
                    hoist[j] = a
                    vars_.append("w%d" % j)
            return "%s(%s)" % (pc[0].lower(), ", ".join(vars_))
        wexpr = where_call(expr, True)
        out += ["  w%d = %s;" % (k, hoist[k]) for k in sorted(hoist)]
        if rtype == "C":
            # a character result is compared through a probe that shows its
            # length and trailing blanks (a plain comparison pads with blanks)
            probe = "cats(lengthc(%s)) || '|' || %s || '|'"
            ref, wexpr = probe % (expr, expr), probe % (wexpr, wexpr)
        else:
            ref = expr
        out += ["  __ref = %s;" % ref, "run;", "data work.%s;" % ds, "  set work.w_%s;" % ds,
                "  where %s = __ref;" % wexpr, "  r = __ref;", "  keep r;", "run;"]
    elif ctx == "sql":
        out += ["data work.one; x = 1; run;", "proc sql;",
                "  create table work.%s as select %s as r from work.one;" % (ds, expr), "quit;",
                "data _null_;", "  set work.%s;" % ds]
        out += record_lines(cid, rtype)
        out.append("run;")
    else:
        out.append("data work.%s;" % ds)
        if setup:
            out.append("  " + setup)
        out.append("  r = %s;" % expr)
        out += record_lines(cid, rtype)
        out.append("  drop __:;")
        if setup and re.search(r"\bs\b", setup):
            out.append("  drop s;")
        out.append("run;")
    out.append("%%put @@END %s;" % cid)
    return out


COLLECT = r"""%put @@COLLECT;
%macro __sasonr_collect(cases);
  %local i c dsid n rc;
  data work.matrix;
  %do i = 1 %to %sysfunc(countw(&cases, %str( )));
    %let c = %scan(&cases, &i, %str( ));
    %let n = 0;
    %let dsid = %sysfunc(open(work.&c));
    %if &dsid > 0 %then %do;
      %let n = %sysfunc(attrn(&dsid, nlobs));
      %let rc = %sysfunc(close(&dsid));
    %end;
    %if &n > 0 %then %do;
      set work.&c(rename=(r=&c));
    %end;
  %end;
  run;
  proc datasets lib=work nolist nowarn;
    save matrix;
  quit;
%mend __sasonr_collect;"""


def matrix_program(area, name, rows, source, tags, glob):
    """One SAS program for all cases of a function / format (layout: case_format.md, matrix layout)."""
    main = [r for r in rows if r["group"] != "isolated"]
    iso = [r for r in rows if r["group"] == "isolated"]
    out = ["/* %s/%s: verification matrix, %d cases (inputs and notes in cases.csv)" % (area, name, len(rows)),
           "   source: " + source,
           "   tags: " + ", ".join(tags),
           "   one step per case between %put @@BEGIN <case> and %put @@END <case>; cases that stop",
           "   with an ERROR come last; NOSYNTAXCHECK keeps a compile-time ERROR from putting the later",
           "   steps into syntax-check mode; after %put @@COLLECT the one-row results are joined into",
           "   WORK.MATRIX (one column per case) and the per-case data sets are deleted */",
           "options nosource2 linesize=max nosyntaxcheck;"]
    out += glob
    for r in main + iso:
        out += r["_block"]
    out += COLLECT.split("\n")
    names = [ds_name(r["case"]) for r in rows if r["context"] != "sysfunc"]
    out.append("%__sasonr_collect(")
    for k in range(0, len(names), 6):
        out.append("  " + " ".join(names[k:k + 6]))
    out.append(")")
    return "\n".join(out) + "\n"


def case_id(fn, ctx, expr, setup):
    h = hashlib.sha1(("%s|%s|%s" % (ctx, expr, setup)).encode("utf-8")).hexdigest()[:6]
    return "%s_%s" % (fn, h)


def old_group(case_dir, cid):
    """isolated when an earlier gold run of the case (one-case layout) ended with an ERROR."""
    m = os.path.join(case_dir, cid, "meta.yaml")
    if not os.path.exists(m):
        return None
    for ln in open(m, encoding="utf-8"):
        if ln.startswith("sas_rc:"):
            try:
                return "isolated" if int(ln.split(":", 1)[1]) >= 2 else "main"
            except ValueError:
                return None
    return None


def write_cases(out_root, fn, cases, rtype, source, tags, glob, area, keep_only):
    """tests/conformance/<area>/<FN>/: program.sas, cases.csv, meta.yaml (gold: expected/ via refresh_gold.py)."""
    d = os.path.join(out_root, fn)
    os.makedirs(d, exist_ok=True)
    prev = {}
    pc = os.path.join(d, "cases.csv")
    if os.path.exists(pc):
        with open(pc, newline="") as f:
            prev = {r["case"]: r.get("group") or "" for r in csv.DictReader(f)}
    rows, seen = [], {}
    for ctx, expr, setup, note in cases:
        cid = case_id(fn, ctx, expr, setup)
        if cid in seen and seen[cid] != (ctx, expr, setup):
            # two different cases with the same 6-digit id (fn_where IFN): a longer id
            h = hashlib.sha1(("%s|%s|%s" % (ctx, expr, setup)).encode("utf-8")).hexdigest()[:10]
            cid = "%s_%s" % (fn, h)
        if cid in seen:
            continue
        seen[cid] = (ctx, expr, setup)
        # an expression wrapped in another function (lengthc(strip(...))) records that function's type
        rt = rtype
        outer = split_call(expr) if ctx in ("data", "sql", "where") else None
        if outer and outer[0].upper() != fn and INV_TYPES.get(outer[0].upper()):
            rt = INV_TYPES[outer[0].upper()]
        grp = "isolated" if ctx == "ce" else (old_group(d, cid) or prev.get(cid) or "main")
        rows.append({"case": cid, "context": ctx, "expr": expr, "setup": setup, "note": note, "group": grp,
                     "_block": case_block(cid, "datastep" if ctx == "ce" else ctx, expr, setup, rt)})
    prog = matrix_program(area, fn, rows, source, tags, glob)
    p = os.path.join(d, "program.sas")
    if not os.path.exists(p) or open(p, encoding="latin-1").read() != prog:
        with open(p, "w", encoding="latin-1", newline="\n") as f:
            f.write(prog)
    mp = os.path.join(d, "meta.yaml")
    if not os.path.exists(mp):
        with open(mp, "w", newline="\n") as f:
            f.write("case: %s/%s\nlayout: matrix\n" % (area, fn))
    with open(pc, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["case", "context", "expr", "setup", "note", "group"], extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    return len(rows)


# ---------------------------------------------------------------------------
# functions
# ---------------------------------------------------------------------------

def function_cases(fn, ex, mx, inv):
    calls = [c for c in ex.get("calls", [])] if ex and ex.get("mode") == "data" else []
    extra = mx.get("extra", []) if mx else []
    cases = [("data", c, "", "reference example") for c in calls]
    for ctx, c, setup in extra:
        cases.append((ctx, c, setup, {"data": "documented usage", "sysfunc": "%SYSFUNC", "sql": "PROC SQL",
                                     "ce": "compile-time error"}[ctx]))
    info = inv.get(fn)
    minarg = int(info["minarg"]) if info else None
    maxarg = int(info["maxarg"]) if info else None
    if maxarg == 0:
        maxarg = None
    if mx and mx.get("env"):
        return cases
    data_calls = [c for c in calls] + [c for ctx, c, s in extra if ctx == "data" and not s]
    base = mx.get("base") if mx else None
    if not base:
        parsed = [(split_call(c), c) for c in data_calls]
        parsed = [(p, c) for p, c in parsed if p and p[0].upper() == fn]
        if parsed:
            base = max(parsed, key=lambda pc: len(pc[0][1]))[1]
    if base and not (mx and mx.get("nogeneric")):
        name, args = split_call(base)
        keys = mx.get("key", set()) if mx else set()
        seen_type = set()
        for k, a in enumerate(args, 1):
            t = "K" if k in keys else (arg_type(info["fncargs"], k) if info else "A")
            if t == "K":
                subs = KEY_SUBS
            elif t == "N":
                subs = NUM_FIRST if "N" not in seen_type else NUM_OTHER
            elif t == "C":
                subs = CHR_FIRST if "C" not in seen_type else CHR_OTHER
            else:
                subs = (CHR_FIRST if "A" not in seen_type else CHR_OTHER) + [("1.5", "", "number"), (".", "", "missing"),
                                                                            (".A", "", "special missing .A")]
            seen_type.add(t)
            for v, setup, note in subs:
                if v == a:
                    continue
                new = list(args)
                new[k - 1] = v
                cases.append(("data", join_call(name, new), setup, "argument %d: %s" % (k, note)))
        # argument counts
        if minarg is not None and minarg >= 1:
            cases.append(("ce", join_call(name, args[:minarg - 1]), "", "too few arguments"))
        if maxarg is not None and maxarg <= 12:
            more = list(args) + ["1"] * (maxarg + 1 - len(args))
            if len(more) == maxarg + 1:
                cases.append(("ce", join_call(name, more), "", "too many arguments"))
        # contexts
        if mx and mx.get("ctx"):
            n = 0
            for c in data_calls:
                p = split_call(c)
                if not p or p[0].upper() != fn:
                    continue
                sa = [sysfunc_arg(a) for a in p[1]]
                if all(x is not None for x in sa):
                    cases.append(("sysfunc", "%%sysfunc(%s)" % join_call(p[0], sa), "", "%SYSFUNC"))
                    n += 1
                if n >= 2:
                    break
            for c in data_calls[:2]:
                cases.append(("sql", c, "", "PROC SQL"))
    return cases


def gen_functions(names):
    inv = read_inventory()
    INV_TYPES.update({k: v["fnctype"] for k, v in inv.items()})
    ex = parse_examples(os.path.join(HERE, "fn_examples.txt"))
    mx = parse_matrix(os.path.join(HERE, "fn_matrix.txt"))
    total = 0
    for fn in sorted(mx):
        if names and fn not in names:
            continue
        info = inv.get(fn)
        rtype = mx[fn]["ret"] or (info["fnctype"] if info else "N")
        cases = function_cases(fn, ex.get(fn), mx[fn], inv)
        glob = (ex.get(fn) or {}).get("glob", []) + mx[fn]["glob"]
        n = write_cases(FN_OUT, fn, cases, rtype, "SAS 9.4 Functions and CALL Routines, %s Function" % fn,
                        ["function", fn.lower(), "fn_matrix"], glob, "fn_matrix", None)
        total += n
        print("%-10s %4d" % (fn, n))
    print("total", total)


# ---------------------------------------------------------------------------
# formats and informats
# ---------------------------------------------------------------------------

def parse_fmt_matrix(path):
    """tools/fmt_matrix.txt:

    ## NAME format|informat N|C
    values: v1 | v2 | ...          values (SAS expressions for formats, raw text for informats)
    widths: w1 w2 ...              widths (blank = default width); `w.d` entries allowed
    cases: <value> @ <spec>         explicit pairs (spec like 8.2, -l, ??)
    ce: <statement expression>      compile-time error
    """
    spec = {}
    cur = None
    for ln in open(path, encoding="utf-8"):
        ln = ln.rstrip("\n")
        if ln.startswith("## "):
            p = ln[3:].split()
            cur = p[0]
            spec[cur] = {"kind": p[1], "type": p[2], "values": [], "widths": [], "cases": [], "ce": [],
                         "base": p[3] if len(p) > 3 else p[0]}
            continue
        if cur is None or not ln.strip() or ln.startswith("#"):
            continue
        k, _, v = ln.partition(": ")
        if k == "values":
            # "\\s" stands for a blank (leading/trailing blanks of informat input)
            spec[cur]["values"] += [x.strip().replace("\\s", " ") for x in v.split(" | ")]
        elif k == "widths":
            spec[cur]["widths"] += v.split()
        elif k == "cases":
            val, _, s = v.rpartition(" @ ")
            spec[cur]["cases"].append((val.strip(), s.strip()))
        elif k == "ce":
            spec[cur]["ce"].append(v.strip())
    return spec


def fmt_text(base, char, ws):
    pre = "$" if char else ""
    return "%s%s%s%s" % (pre, base.lower(), ws, "" if "." in ws else ".")


def sas_str(s):
    return "'" + s.replace("'", "''") + "'"


def fmt_expr(kind, char, base, val, ws):
    mod = ""
    if ws.startswith("??") or ws.startswith("?"):
        mod = "?? " if ws.startswith("??") else "? "
        ws = ws.lstrip("?").strip()
    align = ""
    if ws.endswith(("-l", "-r", "-c")):
        align, ws = ws[-2:], ws[:-2]
    if ws == "_":
        ws = ""
    f = fmt_text(base, char, ws)
    if kind == "format":
        return "put(%s, %s%s)" % (val, f, align)
    return "input(%s, %s%s)" % (sas_str(val), mod, f)


def pilot_cases(fn):
    """DATA step calls of the function pilot (tests/conformance/functions/<FN>/<case>/program.sas)."""
    d = os.path.join(ROOT, "tests", "conformance", "functions", fn)
    out = []
    if not os.path.isdir(d):
        return out
    for c in sorted(os.listdir(d)):
        p = os.path.join(d, c, "program.sas")
        if not os.path.exists(p):
            continue
        lines = open(p, encoding="latin-1").read().split("\n")
        try:
            i = next(k for k, l in enumerate(lines) if re.match(r"^data work\.%s;" % re.escape(c), l, re.I))
        except StopIteration:
            continue
        setup = []
        for l in lines[i + 1:]:
            m = re.match(r"^  r = (.*);$", l)
            if m:
                out.append(("where", m.group(1).strip(), " ".join(setup), "function pilot %s" % c))
                break
            if l.startswith("  file ") or l.startswith("run;"):
                break
            setup.append(l.strip())
    return out


def gen_where(names):
    """fn_where/<FN>/: every DATA step case of the function matrix again inside a WHERE statement."""
    inv = read_inventory()
    INV_TYPES.update({k: v["fnctype"] for k, v in inv.items()})
    ex = parse_examples(os.path.join(HERE, "fn_examples.txt"))
    mx = parse_matrix(os.path.join(HERE, "fn_matrix.txt"))
    total = 0
    pilots = sorted(os.listdir(os.path.join(ROOT, "tests", "conformance", "functions")))
    for fn in sorted(set(mx) | set(pilots)):
        if names and fn not in names:
            continue
        if fn not in mx:
            mx[fn] = {"env": False, "ret": None, "glob": [], "extra": [], "key": set(), "base": None, "nogeneric": True}
        if mx[fn].get("env"):
            continue  # clock-dependent: the DATA step and WHERE values are taken at different times
        info = inv.get(fn)
        rtype = mx[fn]["ret"] or (info["fnctype"] if info else "N")
        cases = [("where", c, setup, note) for ctx, c, setup, note in function_cases(fn, ex.get(fn), mx[fn], inv)
                 if ctx == "data"]
        cases += pilot_cases(fn)
        if not cases:
            continue
        glob = (ex.get(fn) or {}).get("glob", []) + mx[fn]["glob"]
        n = write_cases(WHERE_OUT, fn, cases, rtype, "SAS 9.4 Functions and CALL Routines, %s Function (in a WHERE statement)" % fn,
                        ["function", fn.lower(), "fn_where", "where"], glob, "fn_where", None)
        total += n
        print("%-10s %4d" % (fn, n))
    print("total", total)


def gen_formats(names):
    spec = parse_fmt_matrix(os.path.join(HERE, "fmt_matrix.txt"))
    total = 0
    for nm in sorted(spec):
        if names and nm not in names:
            continue
        s = spec[nm]
        char = s["type"] == "C"
        base = "" if s["base"] == "-" else s["base"].lstrip("$")
        cases = []
        for v in s["values"]:
            for w in s["widths"] or ["_"]:
                cases.append(("data", fmt_expr(s["kind"], char, base, v, w), "", "value %s, width %s" % (v, w)))
        for v, w in s["cases"]:
            cases.append(("data", fmt_expr(s["kind"], char, base, v, w), "", "value %s, %s" % (v, w)))
        for c in s["ce"]:
            cases.append(("ce", c, "", "compile-time error"))
        if s["kind"] == "format":
            rtype = "C"
        else:
            rtype = "C" if char else "N"
        what = "Formats" if s["kind"] == "format" else "Informats"
        src = "SAS 9.4 Formats and Informats: Reference, %s%s %s" % ("$" if char else "", base.upper(),
                                                                   "Format" if s["kind"] == "format" else "Informat")
        n = write_cases(FMT_OUT, nm, cases, rtype, src, [s["kind"], nm.lower(), "fmt_matrix"], [], "fmt_matrix", None)
        total += n
        print("%-12s %4d" % (nm, n))
    print("total", total)


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in ("functions", "formats", "where"):
        raise SystemExit(__doc__)
    sel = {a.upper() for a in sys.argv[2:]}
    {"functions": gen_functions, "formats": gen_formats, "where": gen_where}[sys.argv[1]](sel)
