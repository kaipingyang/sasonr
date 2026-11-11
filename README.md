# sasonr

Run SAS® programs on an R engine.

sasonr parses SAS source code (DATA step, macro language, PROC SQL and a
growing set of procedures) into a faithful abstract syntax tree with a
hand-written lexer, recursive-descent parser and Pratt expression parser,
then executes it in R: DATA steps are compiled into tidyverse code, PROC SQL
runs on DuckDB. The goal is end-to-end results identical to SAS 9.4.

Three ways to use it:

1. **Run SAS on R** - execute SAS programs without a SAS licence.
2. **Deterministic SAS to R migration** - `sas_translate()` produces R code
   without any language model, and the result is verified against SAS output.
3. **R / SAS double programming** - one programmer writes R, the other writes
   SAS and runs it with sasonr on the same platform.

Design rules:

- **Deterministic**: no language model is involved at run time.
- **Fail loud**: any language feature that has not been verified against
  real SAS output raises a `sasonr_unsupported` condition with the SAS line
  number instead of guessing.
- **Verified against SAS**: features are accepted only after their test cases
  match output produced by SAS 9.4 (datasets, variable attributes, log
  diagnostics and listing).

Status: early development, not on CRAN.

```r
# pak::pak("kaipingyang/sasonr")
library(sasonr)
res <- sas_run("data a; do i = 1 to 3; x = i**2; output; end; run;")
sas_translate("data b; set a; if x > 1; run;")   # show generated R code
```

SAS is a registered trademark of SAS Institute Inc. This project is not
affiliated with or endorsed by SAS Institute Inc.
