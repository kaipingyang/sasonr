# DATETIME: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATETIME() returns the current date and time (the clock of the session), so
# its value cannot be reproduced; the examples check its type and range only.

# DATETIME() returns the current date and time in seconds since midnight, 1
# January 1960.
as.numeric(sas_f_datetime() > 2019686400)  # SAS 9.4: datetime() > '01JAN2024:00:00:00'dt = 1

# The date part of the current datetime equals today's date.
as.numeric(sas_f_datepart(sas_f_datetime()) == sas_f_today())  # SAS 9.4: datepart(datetime()) = today() = 1
