# MODZ: each call is explained above it and followed by the result of the same call in SAS 9.4

# The remainder of 10 divided by 3 is 1.
sas_f_modz(10, 3)  # SAS 9.4: modz(10, 3) = 1

# The remainder has the sign of the dividend: -1.
sas_f_modz(-10, 3)  # SAS 9.4: modz(-10, 3) = -1

# MODZ also works with non-integers: 5.5 - 2*2 = 1.5.
sas_f_modz(5.5, 2)  # SAS 9.4: modz(5.5, 2) = 1.5

# MODZ uses zero fuzzing: in binary 0.3 is slightly less than 3 * 0.1, so the
# remainder is (almost) 0.1.
sas_f_modz(0.3, 0.1)  # SAS 9.4: modz(0.3, 0.1) = 0.1
