# LOGPDF: each call is explained above it and followed by its result

# LOGPDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The log of the standard normal density at 0 is -log(sqrt(2*pi)).
sas_f_logpdf("NORMAL", 0)  # logpdf('NORMAL', 0) = -0.918938533204673

# Exponential distribution with scale 2: the density at 1 is exp(-1/2) / 2,
# whose log is -0.5 - log(2).
sas_f_logpdf("EXPONENTIAL", 1, 2)  # logpdf('EXPONENTIAL', 1, 2) = -1.19314718055995

# A missing quantile gives a missing result.
sas_f_logpdf("NORMAL", NA_real_)  # logpdf('NORMAL', .) = .

options(old)
