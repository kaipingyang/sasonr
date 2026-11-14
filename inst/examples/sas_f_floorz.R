# FLOORZ: each call is explained above it and followed by its result

# The largest integer less than or equal to 2.7 is 2.
sas_f_floorz(2.7)  # floorz(2.7) = 2

# FLOORZ uses zero fuzzing, so a value only slightly below 3 gives 2.
sas_f_floorz(3 - 1e-13)  # floorz(3 - 1e-13) = 2

# The floor of -2.5 is -3.
sas_f_floorz(-2.5)  # floorz(-2.5) = -3

# A missing argument gives a missing result.
sas_f_floorz(NA_real_)  # floorz(.) = .
