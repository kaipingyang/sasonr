# FINV: each call is explained above it and followed by its result

# FINV is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# A missing probability gives a missing result.
sas_f_finv(NA_real_, 1, 1)  # finv(., 1, 1) = .

options(old)
