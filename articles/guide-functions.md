# Functions, formats and rounding

``` r
library(sasonr)
```

sasonr implements SAS functions, formats and informats in R with SAS 9.4
semantics for missing values, fixed-length character results, result
lengths and `_ERROR_` diagnostics. Every function has a reference page
(see
[Reference](https://kaipingyang.github.io/sasonr/reference/index.md))
with the SAS syntax and the differences from base R.

## Calling SAS functions inside a DATA step

``` r
res <- sas_run("
data _null_;
  s = 'a,b,,c';
  w2 = scan(s, 2, ',');
  wl = scan(s, -1, ',');
  c  = compress('A-1 B-2', '-', 's');
  x  = catx('/', 'a', ' ', 'b');
  d  = intnx('month', '15MAR2026'd, 1, 'e');
  put w2= wl= c= x= d= date9.;
run;
")
sas_log(res)
#> 1          
#> 2          data _null_;
#> 3            s = 'a,b,,c';
#> 4            w2 = scan(s, 2, ',');
#> 5            wl = scan(s, -1, ',');
#> 6            c  = compress('A-1 B-2', '-', 's');
#> 7            x  = catx('/', 'a', ' ', 'b');
#> 8            d  = intnx('month', '15MAR2026'd, 1, 'e');
#> 9            put w2= wl= c= x= d= date9.;
#> 10         run;
#> 
#> w2=b wl=c c=A1B2 x=a/b d=30APR2026
```

## Calling them from R

The implementations are exported as `sas_f_<name>()`, so translated code
can run on its own and you can use them directly:

``` r
sas_f_round(2.675, 0.01)
#> [1] 2.68
sas_f_scan("a b c", 2)
#> [1] "b"
```

## Rounding

The SAS rounding rules that sasonr implements differ from base R in ways
that matter for clinical tables:

| Situation                                              | sasonr                                              | base R                            |
| ------------------------------------------------------ | --------------------------------------------------- | --------------------------------- |
| `ROUND(x, unit)` on halves                             | away from zero                                      | half to even (`round(0.5) == 0`)  |
| values just below a half in binary (e.g. 1.005, 2.675) | rounded up: `ROUND` applies a small relative fuzz   | rounded down                      |
| `w.d` formats                                          | rounding with a much smaller tolerance than `ROUND` | `sprintf()` uses the binary value |
| negative values that round to zero                     | `ROUND` then `put` shows `0.00`                     | `-0.00` is possible               |

``` r
x <- c(1.005, 2.675, 23 / 80 * 100)
round(x, c(2, 2, 1))              # base R
#> [1]  1.00  2.67 28.70
sas_f_round(x, c(0.01, 0.01, 0.1)) # SAS ROUND
#> [1]  1.01  2.68 28.80
```

Values for which sasonr cannot determine the result exactly (a few edge
windows of the fuzz, very narrow `BEST` widths) stop with
`sasonr_unsupported` instead of returning a value that may be wrong.
