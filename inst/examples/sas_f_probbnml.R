# PROBBNML: each call is explained above it and followed by its result

# PROBBNML is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(X <= n) is always 1.
sas_f_probbnml(0.2, 5, 5)  # probbnml(0.2, 5, 5) = 1

# A missing probability gives a missing result.
sas_f_probbnml(NA_real_, 5, 1)  # probbnml(., 5, 1) = .

options(old)
