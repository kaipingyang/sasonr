# ROUNDZ: each call is explained above it and followed by the result of the same call in SAS 9.4

# ROUNDZ rounds an exact tie to the even integer: 2.5 gives 2.
sas_f_roundz(2.5)  # SAS 9.4: roundz(2.5) = 2

# The second argument is the rounding unit; 2.45 rounded to a multiple of 0.1
# gives 2.4.
sas_f_roundz(2.45, 0.1)  # SAS 9.4: roundz(2.45, 0.1) = 2.4

# ROUNDZ uses no fuzzing, and 1.005 is stored slightly below 1.005 in binary,
# so it rounds down to 1.
sas_f_roundz(1.005, 0.01)  # SAS 9.4: roundz(1.005, 0.01) = 1

# -3.5 is halfway between -4 and -3, so the even integer -4 is chosen.
sas_f_roundz(-3.5)  # SAS 9.4: roundz(-3.5) = -4

# A missing argument gives a missing result.
sas_f_roundz(NA_real_)  # SAS 9.4: roundz(.) = .
