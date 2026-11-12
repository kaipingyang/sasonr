# INTZ: each call is explained above it and followed by the result of the same call in SAS 9.4

# INTZ returns the integer part, dropping the fraction: 2.
sas_f_intz(2.7)  # SAS 9.4: intz(2.7) = 2

# INTZ truncates towards zero, so -2.7 gives -2.
sas_f_intz(-2.7)  # SAS 9.4: intz(-2.7) = -2

# INTZ uses zero fuzzing, so a value only slightly below 3 truncates to 2.
sas_f_intz(3 - 1e-13)  # SAS 9.4: intz(3 - 1e-13) = 2

# A missing argument gives a missing result.
sas_f_intz(NA_real_)  # SAS 9.4: intz(.) = .
