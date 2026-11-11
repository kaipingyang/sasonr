# TIMEPART: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_timepart(2026117815)  # SAS: timepart('15MAR2024:10:30:15'dt) -> 37815
sas_f_timepart(-1)  # SAS: timepart(-1) -> 86399
sas_f_timepart(0)  # SAS: timepart(0) -> 0
sas_f_timepart(NA_real_)  # SAS: timepart(.) -> .
