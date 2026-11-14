# MISSING: each call is explained above it and followed by its result

# MISSING returns 1 for the missing value (.).
sas_f_missing(NA_real_)  # missing(.) = 1

# Special missing values such as .A also count as missing.
sas_f_missing(sas_missing(".A"))  # missing(.A) = 1

# 0 is a valid number, so the result is 0.
sas_f_missing(0)  # missing(0) = 0

# For a character argument a blank string counts as missing.
sas_f_missing(" ")  # missing('') = 1

# A string with any nonblank character is not missing.
sas_f_missing(" a")  # missing(' a') = 0
