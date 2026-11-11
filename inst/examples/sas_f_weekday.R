# WEEKDAY: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_weekday(23450)  # SAS: weekday('15MAR2024'd) -> 6
sas_f_weekday(0)  # SAS: weekday(0) -> 6
sas_f_weekday(23452)  # SAS: weekday('17MAR2024'd) -> 1
sas_f_weekday(NA_real_)  # SAS: weekday(.) -> .
