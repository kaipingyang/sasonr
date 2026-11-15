# PROBF: each call is explained above it and followed by its result

# PROBF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(F <= 4) for an F distribution with 2 and 10 degrees of freedom.
sas_f_probf(4, 2, 10)  # probf(4, 2, 10) = 0.947077850598655

# A missing argument gives a missing result.
sas_f_probf(NA_real_, 1, 1)  # probf(., 1, 1) = .

options(old)
