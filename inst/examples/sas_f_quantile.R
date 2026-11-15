# QUANTILE: each call is explained above it and followed by its result

# QUANTILE is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# For a discrete distribution the quantile is the smallest x with CDF >= p: the
# binomial median is 5.
sas_f_quantile("BINOMIAL", 0.5, 0.5, 10)  # quantile('BINOMIAL', 0.5, 0.5, 10) = 5

# A missing probability gives a missing result.
sas_f_quantile("NORMAL", NA_real_)  # quantile('NORMAL', .) = .

options(old)
