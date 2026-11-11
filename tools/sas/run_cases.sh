#!/bin/bash
# Run conformance cases with SAS (one fresh batch session per case).
#   run_cases.sh <src_root> <gold_root> <list_file> <sas_bin> <epilogue.sas> [parallel=1] [timeout_sec=300]
# Each line of <list_file> is a case directory relative to <src_root> holding
# program.sas (and optionally data/).  Every case runs in a fresh SAS session
# with its own SASUSER and cwd=<gold_root>/<rel>, which first receives program.sas,
# a copy of data/ and empty out/ and lib/ directories; a file `cwd` in the case
# directory names another working directory (shared support files).  Produces
# program.log, program.lst, ds/*.sas7bdat, ds/__attr.csv and rc.
SRC=$1; GOLD=$2; LIST=$3; SAS=$4; EPI=$5; P=${6:-1}; TO=${7:-300}
run_one() {
  rel=$1; d=$GOLD/$rel
  rm -rf "$d"; mkdir -p "$d/ds" "$d/html" "$d/out" "$d/lib" "$d/.sasuser"
  [ -d "$SRC/$rel/data" ] && cp -r "$SRC/$rel/data" "$d/data"
  cp "$SRC/$rel/program.sas" "$d/program.sas"
  ln -s ../out "$d/html/out"
  wd="$d"; [ -f "$SRC/$rel/cwd" ] && wd=$(cat "$SRC/$rel/cwd")
  cd "$wd"
  timeout $TO $SAS -sysin "$d/program.sas" -log "$d/program.log" -print "$d/program.lst" -nonews -noterminal \
    -sasuser "$d/.sasuser" \
    -initstmt "%let __sasonr_dsdir=$d/ds; options nocenter nodate nonumber formdlim=' ' pagesize=max linesize=132; title; ods html path='$d/html' file='program.html';" \
    -termstmt "ods html close; %put __SASONR_EPILOGUE__; %include '$EPI';" </dev/null >/dev/null 2>&1
  echo $? > "$d/rc"
  rm -rf "$d/.sasuser" "$d/html"
  chmod -R a+rwX "$d" 2>/dev/null
}
export -f run_one; export SRC GOLD SAS EPI TO
grep -v '^\s*$' "$LIST" | xargs -P "$P" -I{} bash -c 'run_one {}'
echo DONE $(grep -v '^\s*$' "$LIST" | wc -l)
