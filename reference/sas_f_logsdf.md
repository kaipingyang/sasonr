# SAS `LOGSDF(distribution, quantile <, parameters>)`

Logarithm of the survival function. See the [SAS
documentation](https://documentation.sas.com/doc/en/lefunctionsref/9.4/titlepage.htm)
(Functions and CALL Routines).

## Arguments

  - dist:
    
    Character name of the distribution, not case sensitive: 'NORMAL' (or
    'GAUSS'), 'T', 'CHISQ', 'F', 'BINOMIAL', 'POISSON', 'UNIFORM',
    'EXPONENTIAL', 'BETA', 'GAMMA' or 'LOGNORMAL'.

  - x:
    
    Numeric value at which the function is evaluated (a count for
    discrete distributions). A missing value gives a missing result.

  - ...:
    
    Distribution parameters in the order SAS 9.4 defines: NORMAL
    location and scale (default 0, 1); T df \<, nc\>; CHISQ df \<, nc\>;
    F ndf, ddf \<, nc\>; BINOMIAL p, n; POISSON mean; UNIFORM left,
    right (default 0, 1); EXPONENTIAL scale (default 1); BETA a, b;
    GAMMA shape \<, scale\>; LOGNORMAL location, scale (default 0, 1).

## Value

A vector with one result per element (arguments are recycled), as the
SAS function returns per observation.

## Differences from R

Arguments and parameterisation are those of the SAS function (order of
arguments, noncentrality), not those of R's d/p/q/r functions.

## See also

Other probability functions: `sas_f_cdf()`, `sas_f_cinv()`,
`sas_f_finv()`, `sas_f_logcdf()`, `sas_f_logpdf()`, `sas_f_pdf()`,
`sas_f_poisson()`, `sas_f_probbnml()`, `sas_f_probchi()`,
`sas_f_probf()`, `sas_f_probit()`, `sas_f_probnorm()`, `sas_f_probt()`,
`sas_f_quantile()`, `sas_f_sdf()`, `sas_f_tinv()`

## Examples

``` r
# LOGSDF: each call is explained above it and followed by its result

# LOGSDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The log of the normal survival function P(X > 0) is log(0.5).
sas_f_logsdf("NORMAL", 0)  # logsdf('NORMAL', 0) = -0.693147180559945
#> Warning: sasonr: function LOGSDF is approximate
#> This warning is displayed once per session.
#> [1] -0.6931472

# Far in the upper tail LOGSDF stays accurate although P(X > 10) is only about
# 7.6E-24.
sas_f_logsdf("NORMAL", 10)  # logsdf('NORMAL', 10) = -53.2312851505125
#> [1] -53.23129

# A missing quantile gives a missing result.
sas_f_logsdf("NORMAL", NA_real_)  # logsdf('NORMAL', .) = .
#> [1] NA

options(old)
```
