# SECOND: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_second(37815)  # SAS: second('10:30:15't) -> 15
sas_f_second(2026117815.5)  # SAS: second('15MAR2024:10:30:15.5'dt) -> 15.5
sas_f_second(0)  # SAS: second(0) -> 0
sas_f_second(NA_real_)  # SAS: second(.) -> .
