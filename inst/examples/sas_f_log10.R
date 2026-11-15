# LOG10: each call is explained above it and followed by its result

# LOG10 is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The base-10 logarithm of 1000 is 3.
sas_f_log10(1000)  # log10(1000) = 3

# The base-10 logarithm of 2.
sas_f_log10(2)  # log10(2) = 0.301029995663981

# The base-10 logarithm of 0.001 is -3.
sas_f_log10(0.001)  # log10(0.001) = -3

# The logarithm of 0 is undefined, so the result is missing (with a note in the
# log).
sas_f_log10(0)  # log10(0) = .

options(old)
