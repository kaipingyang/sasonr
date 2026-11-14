# ABS: each call is explained above it and followed by its result

# The absolute value of a negative number drops the sign.
sas_f_abs(-3.5)  # abs(-3.5) = 3.5

# The absolute value of zero is zero.
sas_f_abs(0)  # abs(0) = 0

# A missing argument (.) gives a missing result.
sas_f_abs(NA_real_)  # abs(.) = .

# A special missing value such as .A also gives the ordinary missing value (.).
sas_f_abs(sas_missing(".A"))  # abs(.A) = .
