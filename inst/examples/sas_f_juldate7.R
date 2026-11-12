# JULDATE7: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# JULDATE7 always returns a 7-digit Julian date yyyyddd: day 75 of 2024 is
# 2024075.
sas_f_juldate7(23450)  # SAS 9.4: juldate7('15MAR2024'd) = 2024075

# SAS date 0 (1 January 1960) is 1960001.
sas_f_juldate7(0)  # SAS 9.4: juldate7(0) = 1960001

# 31 December 1899 is day 365 of 1899.
sas_f_juldate7(-21915)  # SAS 9.4: juldate7('31DEC1899'd) = 1899365

# A missing argument gives a missing result.
sas_f_juldate7(NA_real_)  # SAS 9.4: juldate7(.) = .
