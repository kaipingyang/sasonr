# DATE: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# DATE() returns the current date (the clock of the session), so its value
# cannot be reproduced; the examples check its type and range only.
as.numeric(sas_f_date() == sas_f_today())  # SAS: date() = today() -> 1
as.numeric(sas_f_date() > 23376)  # SAS: date() > '01JAN2024'd -> 1
as.numeric(sas_f_date() == sas_f_int(sas_f_date()))  # SAS: date() = int(date()) -> 1
