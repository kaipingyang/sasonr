# DATETIME: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATETIME() returns the current date and time (the clock of the session), so
# its value cannot be reproduced; the examples check its type and range only.
as.numeric(sas_f_datetime() > 2019686400)  # SAS: datetime() > '01JAN2024:00:00:00'dt -> 1
as.numeric(sas_f_datepart(sas_f_datetime()) == sas_f_today())  # SAS: datepart(datetime()) = today() -> 1
