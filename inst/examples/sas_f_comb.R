# COMB: each call is explained above it and followed by its result

# COMB is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The number of ways to choose 2 items out of 5 is 10.
sas_f_comb(5, 2)  # comb(5, 2) = 10

# There is exactly one way to choose 0 items.
sas_f_comb(10, 0)  # comb(10, 0) = 1

# The number of different 5-card hands from a 52-card deck.
sas_f_comb(52, 5)  # comb(52, 5) = 2598960

# A missing argument gives a missing result.
sas_f_comb(NA_real_, 2)  # comb(., 2) = .

options(old)
