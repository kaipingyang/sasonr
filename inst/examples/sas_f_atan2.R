# ATAN2: each call is explained above it and followed by the result of the same call in SAS 9.4

# ATAN2(y, x) is the angle of the point (x, y); for (1, 1) it is pi/4 radians.
sas_f_atan2(1, 1)  # SAS 9.4: atan2(1, 1) = 0.785398163397448

# The point x = -1, y = 1 lies in the second quadrant, so the angle is 3*pi/4.
sas_f_atan2(1, -1)  # SAS 9.4: atan2(1, -1) = 2.35619449019234

# With both coordinates negative the point lies in the third quadrant, so the
# angle is -3*pi/4.
sas_f_atan2(-1, -1)  # SAS 9.4: atan2(-1, -1) = -2.35619449019234

# A missing argument gives a missing result.
sas_f_atan2(NA_real_, 1)  # SAS 9.4: atan2(., 1) = .
