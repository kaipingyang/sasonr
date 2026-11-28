# SAS `PROBCHI(x, df <, nc>)`

Chi-square cumulative probability. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - x:
    
    Numeric value at which the chi-square CDF is evaluated. Missing
    gives missing.

  - df:
    
    Degrees of freedom, a positive number.

  - nc:
    
    Optional noncentrality parameter, default 0 (central distribution);
    must be nonnegative.

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Arguments and parameterisation are those of the SAS function (order of
arguments, noncentrality), not those of R's d/p/q/r functions.

## See also

Other probability functions: `sas_f_cdf()`, `sas_f_cinv()`,
`sas_f_finv()`, `sas_f_logcdf()`, `sas_f_logpdf()`, `sas_f_logsdf()`,
`sas_f_pdf()`, `sas_f_poisson()`, `sas_f_probbnml()`, `sas_f_probf()`,
`sas_f_probit()`, `sas_f_probnorm()`, `sas_f_probt()`,
`sas_f_quantile()`, `sas_f_sdf()`, `sas_f_tinv()`

## Examples

``` r
# PROBCHI: each call is explained above it and followed by its result

# PROBCHI is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# A missing argument gives a missing result.
sas_f_probchi(NA_real_, 1)  # probchi(., 1) = .
#> Warning: sasonr: function PROBCHI is approximate
#> This warning is displayed once per session.
#> [1] NA

options(old)
```
