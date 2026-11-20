# sasonr

Execute SAS programs on an R engine.

sasonr is an independent implementation of SAS 9.4 program semantics in
R, written with reference to the publicly available SAS 9.4
documentation. Features it does not support fail loud instead of being
approximated. Compatibility with SAS is not guaranteed. sasonr is not
affiliated with, endorsed by, or sponsored by SAS Institute Inc. SAS is
a registered trademark of SAS Institute Inc.

sasonr parses SAS source code (DATA step, macro language, PROC SQL and a
growing set of procedures) into an abstract syntax tree with a
hand-written lexer, recursive-descent parser and Pratt expression
parser, then executes it in R: DATA steps are compiled into tidyverse
code, PROC SQL runs on DuckDB.

Three ways to use it:

1.  **SAS programs in R** - execute SAS programs in an R session.
2.  **Deterministic SAS to R migration** - `sas_translate()` produces R
    code without any language model.
3.  **R / SAS double programming** - one programmer writes R, the other
    writes SAS code and runs it with sasonr on the same platform.

Design rules:

  - **Deterministic**: no language model is involved at run time.
  - **Fail loud**: a language feature that sasonr does not support
    raises a `sasonr_unsupported` condition with the SAS line number
    instead of guessing.

Status: early development, not on CRAN.

``` r
# pak::pak("kaipingyang/sasonr")
library(sasonr)
res <- sas_run("data a; do i = 1 to 3; x = i**2; output; end; run;")
sas_log(res)                                      # the log, with the program lines
sas_translate("data b; set a; if x > 1; run;")   # show generated R code
```
