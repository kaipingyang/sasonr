# ROUNDE: each call is explained above it and followed by its result

# ROUNDE rounds a value exactly halfway between two integers to the even one:
# 2.5 gives 2.
sas_f_rounde(2.5)  # rounde(2.5) = 2

# 3.5 is halfway between 3 and 4, so the even integer 4 is chosen.
sas_f_rounde(3.5)  # rounde(3.5) = 4

# -2.5 is halfway between -3 and -2, so the even integer -2 is chosen.
sas_f_rounde(-2.5)  # rounde(-2.5) = -2

# The second argument is the rounding unit: 2.45 is halfway between 2.4 and
# 2.5, so the even multiple 2.4 is chosen.
sas_f_rounde(2.45, 0.1)  # rounde(2.45, 0.1) = 2.4

# A missing argument gives a missing result.
sas_f_rounde(NA_real_)  # rounde(.) = .
