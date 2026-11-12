# DIVIDE: each call is explained above it and followed by the result of the same call in SAS 9.4

# DIVIDE returns x / y, here 0.5.
sas_f_divide(1, 2)  # SAS 9.4: divide(1, 2) = 0.5

# Dividing a positive number by 0 gives the special missing value .I (plus
# infinity) instead of an error.
sas_f_divide(1, 0)  # SAS 9.4: divide(1, 0) = .I

# Dividing a negative number by 0 gives the special missing value .M (minus
# infinity).
sas_f_divide(-1, 0)  # SAS 9.4: divide(-1, 0) = .M

# 0 / 0 is undefined, so the result is the ordinary missing value.
sas_f_divide(0, 0)  # SAS 9.4: divide(0, 0) = .

# A missing argument gives a missing result.
sas_f_divide(NA_real_, 2)  # SAS 9.4: divide(., 2) = .
