# SAS `PROBBNML(p, n, m)`

Binomial cumulative probability. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - p:
    
    Success probability of each trial, from 0 to 1.

  - n:
    
    Number of trials, a positive integer.

  - m:
    
    Number of successes, an integer from 0 to `n`; the result is P(X \<=
    m).

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Arguments and parameterisation are those of the SAS function (order of
arguments, noncentrality), not those of R's d/p/q/r functions.

## See also

Other probability functions: `sas_f_cdf()`, `sas_f_cinv()`,
`sas_f_finv()`, `sas_f_logcdf()`, `sas_f_logpdf()`, `sas_f_logsdf()`,
`sas_f_pdf()`, `sas_f_poisson()`, `sas_f_probchi()`, `sas_f_probf()`,
`sas_f_probit()`, `sas_f_probnorm()`, `sas_f_probt()`,
`sas_f_quantile()`, `sas_f_sdf()`, `sas_f_tinv()`

## Examples

``` r
# PROBBNML: each call is explained above it and followed by its result

# PROBBNML is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(X <= n) is always 1.
sas_f_probbnml(0.2, 5, 5)  # probbnml(0.2, 5, 5) = 1
#> Warning: sasonr: function PROBBNML is approximate
#> This warning is displayed once per session.
#> [1] 1

# A missing probability gives a missing result.
sas_f_probbnml(NA_real_, 5, 1)  # probbnml(., 5, 1) = .
#> [1] NA

options(old)
```
