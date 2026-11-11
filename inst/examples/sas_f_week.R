# WEEK: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_week(23450)  # SAS: week('15MAR2024'd) -> 10
sas_f_week(23450, "v")  # SAS: week('15MAR2024'd, 'v') -> 11
sas_f_week(23450, "w")  # SAS: week('15MAR2024'd, 'w') -> 11
sas_f_week(23011, "v")  # SAS: week('01JAN2023'd, 'v') -> 52
