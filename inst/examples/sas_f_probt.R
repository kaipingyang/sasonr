# PROBT: each call is explained above it and followed by its result

# PROBT is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# P(T <= 2) for a t distribution with 10 degrees of freedom.
sas_f_probt(2, 10)  # probt(2, 10) = 0.96330598261463

# The t distribution is symmetric about 0, so P(T <= 0) is 0.5.
sas_f_probt(0, 5)  # probt(0, 5) = 0.5

# A missing argument gives a missing result.
sas_f_probt(NA_real_, 5)  # probt(., 5) = .

options(old)
