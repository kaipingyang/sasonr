# JULDATE: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# JULDATE gives the Julian date yyddd when the year lies within the YEARCUTOFF
# window: day 75 of 2024 is 24075.
sas_f_juldate(23450)  # SAS 9.4: juldate('15MAR2024'd) = 24075

# 1 January 1960 (SAS date 0) is 60001: year 60, day 1.
sas_f_juldate(0)  # SAS 9.4: juldate('01JAN1960'd) = 60001

# For a year outside the 100-year YEARCUTOFF window the 4-digit year is used:
# 1899365.
sas_f_juldate(-21915)  # SAS 9.4: juldate('31DEC1899'd) = 1899365

# A missing argument gives a missing result.
sas_f_juldate(NA_real_)  # SAS 9.4: juldate(.) = .
