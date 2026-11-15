# SDF: each call is explained above it and followed by its result

# SDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# Exponential distribution with the default scale 1: P(X > 1) = exp(-1).
sas_f_sdf("EXPONENTIAL", 1)  # sdf('EXPONENTIAL', 1) = 0.367879441171442

# A missing quantile gives a missing result.
sas_f_sdf("NORMAL", NA_real_)  # sdf('NORMAL', .) = .

options(old)
