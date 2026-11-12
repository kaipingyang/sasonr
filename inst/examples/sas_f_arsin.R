# ARSIN: each call is explained above it and followed by the result of the same call in SAS 9.4

# The arcsine of 0.5 is pi/6 radians.
sas_f_arsin(0.5)  # SAS 9.4: arsin(0.5) = 0.523598775598299

# The arcsine of -1 is -pi/2 radians.
sas_f_arsin(-1)  # SAS 9.4: arsin(-1) = -1.5707963267949

# A missing argument gives a missing result.
sas_f_arsin(NA_real_)  # SAS 9.4: arsin(.) = .

# 1.5 is outside the domain from -1 to 1, so the result is missing (with a note
# in the log).
sas_f_arsin(1.5)  # SAS 9.4: arsin(1.5) = .
