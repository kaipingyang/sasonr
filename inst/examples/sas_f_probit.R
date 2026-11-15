# PROBIT: each call is explained above it and followed by its result

# PROBIT is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The probability 0.5 maps to 0.
sas_f_probit(0.5)  # probit(0.5) = 0

# A missing argument gives a missing result.
sas_f_probit(NA_real_)  # probit(.) = .

options(old)
