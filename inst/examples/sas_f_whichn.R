# WHICHN: each call is explained above it and followed by the result of the same call in SAS 9.4

# WHICHN returns the index of the first value equal to the first argument: 2 is
# the 2nd value.
sas_f_whichn(2, 1, 2, 3)  # SAS 9.4: whichn(2, 1, 2, 3) = 2

# No value matches, so the result is 0.
sas_f_whichn(9, 1, 2)  # SAS 9.4: whichn(9, 1, 2) = 0

# A missing first argument gives a missing result, even though . appears in the
# list.
sas_f_whichn(NA_real_, 1, NA_real_, 3)  # SAS 9.4: whichn(., 1, ., 3) = .
