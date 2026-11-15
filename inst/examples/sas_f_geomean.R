# GEOMEAN: each call is explained above it and followed by its result

# GEOMEAN is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The geometric mean of 1, 2 and 4 is the cube root of 8, which is 2.
sas_f_geomean(1, 2, 4)  # geomean(1, 2, 4) = 2

# Missing values are ignored: the geometric mean of 1 and 9 is 3.
sas_f_geomean(1, NA_real_, 9)  # geomean(1, ., 9) = 3

# If any value is zero, the geometric mean is 0.
sas_f_geomean(0, 4)  # geomean(0, 4) = 0

# All arguments are missing, so the result is missing.
sas_f_geomean(NA_real_, NA_real_)  # geomean(., .) = .

options(old)
