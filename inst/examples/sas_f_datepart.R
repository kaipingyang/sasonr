# DATEPART: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_datepart(2026117800)  # SAS: datepart('15MAR2024:10:30:00'dt) -> 23450
sas_f_datepart(0)  # SAS: datepart(0) -> 0
sas_f_datepart(-1)  # SAS: datepart(-1) -> -1
sas_f_datepart(NA_real_)  # SAS: datepart(.) -> .
