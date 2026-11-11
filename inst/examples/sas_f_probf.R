# PROBF: each call is followed by the result of the same call in SAS 9.4
sas_f_probf(4, 2, 10)  # SAS: probf(4, 2, 10) -> 0.947077850598655
sas_f_probf(1, 1, 1)  # SAS: probf(1, 1, 1) -> 0.5
sas_f_probf(1, 2, 10, 1)  # SAS: probf(1, 2, 10, 1) -> 0.45026969
sas_f_probf(NA_real_, 1, 1)  # SAS: probf(., 1, 1) -> .
