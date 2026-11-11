# HOUR: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_hour(37815)  # SAS: hour('10:30:15't) -> 10
sas_f_hour(2026166399)  # SAS: hour('15MAR2024:23:59:59'dt) -> 23
sas_f_hour(0)  # SAS: hour(0) -> 0
sas_f_hour(NA_real_)  # SAS: hour(.) -> .
