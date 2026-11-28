# SAS `PROBT(x, df <, nc>)`

t cumulative probability. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value at which the t CDF is evaluated. Missing gives
    missing.

  - df:
    
    Degrees of freedom, a positive number.

  - nc:
    
    Optional noncentrality parameter, default 0 (central distribution);
    must be nonnegative.

  - .at:
    
    Internal: source position `c(line, column)` used in log NOTEs.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Arguments and parameterisation are those of the SAS function (order of
arguments, noncentrality), not those of R's d/p/q/r functions.

## See also

Other probability functions: `sas_f_cdf()`, `sas_f_cinv()`,
`sas_f_finv()`, `sas_f_logcdf()`, `sas_f_logpdf()`, `sas_f_logsdf()`,
`sas_f_pdf()`, `sas_f_poisson()`, `sas_f_probbnml()`, `sas_f_probchi()`,
`sas_f_probf()`, `sas_f_probit()`, `sas_f_probnorm()`,
`sas_f_quantile()`, `sas_f_sdf()`, `sas_f_tinv()`

## Examples

``` r
# PROBT: each call is explained above it and followed by its result

# PROBT is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(T <= 2) for a t distribution with 10 degrees of freedom.
sas_f_probt(2, 10)  # probt(2, 10) = 0.96330598261463
#> Warning: sasonr: function PROBT is approximate
#> This warning is displayed once per session.
#> [1] 0.963306

# The t distribution is symmetric about 0, so P(T <= 0) is 0.5.
sas_f_probt(0, 5)  # probt(0, 5) = 0.5
#> [1] 0.5

# A missing argument gives a missing result.
sas_f_probt(NA_real_, 5)  # probt(., 5) = .
#> [1] NA

options(old)
```
