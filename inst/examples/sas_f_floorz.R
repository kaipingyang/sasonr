# FLOORZ: each call is explained above it and followed by the result of the same call in SAS 9.4

# The largest integer less than or equal to 2.7 is 2.
sas_f_floorz(2.7)  # SAS 9.4: floorz(2.7) = 2

# FLOORZ uses zero fuzzing, so a value only slightly below 3 gives 2.
sas_f_floorz(3 - 1e-13)  # SAS 9.4: floorz(3 - 1e-13) = 2

# The floor of -2.5 is -3.
sas_f_floorz(-2.5)  # SAS 9.4: floorz(-2.5) = -3

# A missing argument gives a missing result.
sas_f_floorz(NA_real_)  # SAS 9.4: floorz(.) = .
