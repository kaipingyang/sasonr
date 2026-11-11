# DHMS: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_dhms(23450, 10, 30, 15)  # SAS: dhms('15MAR2024'd, 10, 30, 15) -> 2026117815
sas_f_dhms(0, 25, 0, 0)  # SAS: dhms(0, 25, 0, 0) -> 90000
sas_f_dhms(0, 0, 0, 0.5)  # SAS: dhms(0, 0, 0, 0.5) -> 0.5
sas_f_dhms(NA_real_, 1, 1, 1)  # SAS: dhms(., 1, 1, 1) -> .
