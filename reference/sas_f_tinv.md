# SAS `TINV(p, df <, nc>)`

Quantile of the t distribution. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - p:
    
    Numeric probability strictly between 0 and 1. A missing value gives
    a missing result.

  - df:
    
    Degrees of freedom, a positive number (need not be an integer).

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
`sas_f_pdf()`, `sas_f_poisson()`, `sas_f_probbnml()`, `sas_f_probchi()`,
`sas_f_probf()`, `sas_f_probit()`, `sas_f_probnorm()`, `sas_f_probt()`,
`sas_f_quantile()`, `sas_f_sdf()`

## Examples

``` r
# TINV: each call is explained above it and followed by its result

# TINV is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The t distribution is symmetric about 0, so its median is 0.
sas_f_tinv(0.5, 5)  # tinv(0.5, 5) = 0
#> Warning: sasonr: function TINV is approximate
#> This warning is displayed once per session.
#> [1] 0

# A missing probability gives a missing result.
sas_f_tinv(NA_real_, 5)  # tinv(., 5) = .
#> [1] NA

options(old)
```
