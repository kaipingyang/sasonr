# YEAR: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_year(23450)  # SAS: year('15MAR2024'd) -> 2024
sas_f_year(0)  # SAS: year(0) -> 1960
sas_f_year(-1)  # SAS: year(-1) -> 1959
sas_f_year(NA_real_)  # SAS: year(.) -> .
