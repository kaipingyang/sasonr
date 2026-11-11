# MONTH: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_month(23450)  # SAS: month('15MAR2024'd) -> 3
sas_f_month(0)  # SAS: month(0) -> 1
sas_f_month(23741)  # SAS: month('31DEC2024'd) -> 12
sas_f_month(NA_real_)  # SAS: month(.) -> .
