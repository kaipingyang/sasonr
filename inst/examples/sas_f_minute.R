# MINUTE: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_minute(37815)  # SAS: minute('10:30:15't) -> 30
sas_f_minute(2026166399)  # SAS: minute('15MAR2024:23:59:59'dt) -> 59
sas_f_minute(0)  # SAS: minute(0) -> 0
sas_f_minute(NA_real_)  # SAS: minute(.) -> .
