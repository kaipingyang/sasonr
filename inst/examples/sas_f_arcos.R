# ARCOS: each call is explained above it and followed by the result of the same call in SAS 9.4

# The arccosine of 1 is 0 radians.
sas_f_arcos(1)  # SAS 9.4: arcos(1) = 0

# The arccosine of 0 is pi/2 radians.
sas_f_arcos(0)  # SAS 9.4: arcos(0) = 1.5707963267949

# The arccosine of 0.5 is pi/3 radians.
sas_f_arcos(0.5)  # SAS 9.4: arcos(0.5) = 1.0471975511966

# A missing argument gives a missing result.
sas_f_arcos(NA_real_)  # SAS 9.4: arcos(.) = .

# The argument must lie in from -1 to 1; 2 is outside this domain, so the
# result is missing (with a note in the log).
sas_f_arcos(2)  # SAS 9.4: arcos(2) = .
