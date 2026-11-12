# DATE: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATE() returns the current date (the clock of the session), so its value
# cannot be reproduced; the examples check its type and range only.

# DATE() is an alias of TODAY(): both return the current date as a SAS date
# value.
as.numeric(sas_f_date() == sas_f_today())  # SAS 9.4: date() = today() = 1

# The current date is later than 1 January 2024.
as.numeric(sas_f_date() > 23376)  # SAS 9.4: date() > '01JAN2024'd = 1

# A SAS date value is a whole number of days since 1 January 1960.
as.numeric(sas_f_date() == sas_f_int(sas_f_date()))  # SAS 9.4: date() = int(date()) = 1
