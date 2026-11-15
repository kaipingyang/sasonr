# BETA: each call is explained above it and followed by its result

# BETA is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The beta function B(2, 3) = Gamma(2) * Gamma(3) / Gamma(5) = 1/12.
sas_f_beta(2, 3)  # beta(2, 3) = 0.0833333333333333

# B(1, 1) equals 1.
sas_f_beta(1, 1)  # beta(1, 1) = 1

# A missing argument gives a missing result.
sas_f_beta(NA_real_, 1)  # beta(., 1) = .

options(old)
