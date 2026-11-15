# POISSON: each call is explained above it and followed by its result

# POISSON is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(X <= 0) with mean 0.5 is exp(-0.5).
sas_f_poisson(0.5, 0)  # poisson(0.5, 0) = 0.606530659712633

# A missing mean gives a missing result.
sas_f_poisson(NA_real_, 1)  # poisson(., 1) = .

options(old)
