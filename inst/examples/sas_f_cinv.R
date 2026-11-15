# CINV: each call is explained above it and followed by its result

# CINV is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# A missing probability gives a missing result.
sas_f_cinv(NA_real_, 2)  # cinv(., 2) = .

options(old)
