# DAY: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_day(23450)  # SAS: day('15MAR2024'd) -> 15
sas_f_day(0)  # SAS: day(0) -> 1
sas_f_day(23435)  # SAS: day('29FEB2024'd) -> 29
sas_f_day(NA_real_)  # SAS: day(.) -> .
