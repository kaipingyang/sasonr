# CDF: each call is explained above it and followed by its result

# CDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(X <= 2) for a Poisson variable with mean 1.5.
sas_f_cdf("POISSON", 2, 1.5)  # cdf('POISSON', 2, 1.5) = 0.808846830538058

# A missing quantile gives a missing result.
sas_f_cdf("NORMAL", NA_real_)  # cdf('NORMAL', .) = .

options(old)
