# TODAY: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# TODAY() returns the current date (the clock of the session), so its value
# cannot be reproduced; the examples check its type and range only.
as.numeric(sas_f_today() == sas_f_date())  # SAS: today() = date() -> 1
as.numeric(sas_f_today() > 23376)  # SAS: today() > '01JAN2024'd -> 1
as.numeric(sas_f_weekday(sas_f_today()) <= 7)  # SAS: weekday(today()) <= 7 -> 1
