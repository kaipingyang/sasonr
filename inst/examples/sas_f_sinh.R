# SINH: each call is explained above it and followed by its result

# SINH is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The hyperbolic sine of 0 is 0.
sas_f_sinh(0)  # sinh(0) = 0

# The hyperbolic sine of 1 is (e - 1/e) / 2.
sas_f_sinh(1)  # sinh(1) = 1.1752011936438

# A missing argument gives a missing result.
sas_f_sinh(NA_real_)  # sinh(.) = .

options(old)
