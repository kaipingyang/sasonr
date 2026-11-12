# ATAN: each call is explained above it and followed by the result of the same call in SAS 9.4

# The arctangent of 1 is pi/4 radians.
sas_f_atan(1)  # SAS 9.4: atan(1) = 0.785398163397448

# For a very large negative argument the arctangent approaches -pi/2.
sas_f_atan(-1e10)  # SAS 9.4: atan(-1e10) = -1.5707963266949

# The arctangent of 0 is 0.
sas_f_atan(0)  # SAS 9.4: atan(0) = 0

# A missing argument gives a missing result.
sas_f_atan(NA_real_)  # SAS 9.4: atan(.) = .
