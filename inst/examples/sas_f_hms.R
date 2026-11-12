# HMS: each call is explained above it and followed by the result of the same call in SAS 9.4

# HMS converts hours, minutes and seconds to a SAS time value in seconds:
# 10*3600 + 30*60 + 15.
sas_f_hms(10, 30, 15)  # SAS 9.4: hms(10, 30, 15) = 37815

# Minutes may exceed 59: 90 minutes are 5400 seconds.
sas_f_hms(0, 90, 0)  # SAS 9.4: hms(0, 90, 0) = 5400

# Fractional seconds are kept.
sas_f_hms(1, 0, 0.25)  # SAS 9.4: hms(1, 0, 0.25) = 3600.25

# A missing argument gives a missing result.
sas_f_hms(NA_real_, 1, 1)  # SAS 9.4: hms(., 1, 1) = .
