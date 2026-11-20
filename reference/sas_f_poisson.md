# SAS `POISSON(m, n)`

Poisson cumulative probability. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - m:
    
    Mean of the Poisson distribution, a nonnegative number.

  - n:
    
    Count, a nonnegative integer; the result is P(X \<= n).

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Arguments and parameterisation are those of the SAS function (order of
arguments, noncentrality), not those of R's d/p/q/r functions.

## See also

Other probability functions: `sas_f_cdf()`, `sas_f_cinv()`,
`sas_f_finv()`, `sas_f_logcdf()`, `sas_f_logpdf()`, `sas_f_logsdf()`,
`sas_f_pdf()`, `sas_f_probbnml()`, `sas_f_probchi()`, `sas_f_probf()`,
`sas_f_probit()`, `sas_f_probnorm()`, `sas_f_probt()`,
`sas_f_quantile()`, `sas_f_sdf()`, `sas_f_tinv()`

## Examples

``` r
# POISSON: each call is explained above it and followed by its result

# POISSON is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(X <= 0) with mean 0.5 is exp(-0.5).
sas_f_poisson(0.5, 0)  # poisson(0.5, 0) = 0.606530659712633
#> Warning: sasonr: function POISSON is approximate
#> This warning is displayed once per session.
#> [1] 0.6065307

# A missing mean gives a missing result.
sas_f_poisson(NA_real_, 1)  # poisson(., 1) = .
#> [1] NA

options(old)
```
