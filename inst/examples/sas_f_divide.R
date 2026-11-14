# DIVIDE: each call is explained above it and followed by its result

# DIVIDE returns x / y, here 0.5.
sas_f_divide(1, 2)  # divide(1, 2) = 0.5

# Dividing a positive number by 0 gives the special missing value .I (plus
# infinity) instead of an error.
sas_f_divide(1, 0)  # divide(1, 0) = .I

# Dividing a negative number by 0 gives the special missing value .M (minus
# infinity).
sas_f_divide(-1, 0)  # divide(-1, 0) = .M

# 0 / 0 is undefined, so the result is the ordinary missing value.
sas_f_divide(0, 0)  # divide(0, 0) = .

# A missing argument gives a missing result.
sas_f_divide(NA_real_, 2)  # divide(., 2) = .
