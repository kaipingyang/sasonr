# CEILZ: each call is explained above it and followed by the result of the same call in SAS 9.4

# The smallest integer greater than or equal to 2.1 is 3.
sas_f_ceilz(2.1)  # SAS 9.4: ceilz(2.1) = 3

# CEILZ uses zero fuzzing, so a value only slightly above 1 is still rounded up
# to 2.
sas_f_ceilz(1 + 1e-13)  # SAS 9.4: ceilz(1 + 1e-13) = 2

# The ceiling of -2.5 is -2.
sas_f_ceilz(-2.5)  # SAS 9.4: ceilz(-2.5) = -2

# A missing argument gives a missing result.
sas_f_ceilz(NA_real_)  # SAS 9.4: ceilz(.) = .
