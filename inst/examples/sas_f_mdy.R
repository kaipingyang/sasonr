# MDY: each call is followed by the result of the same call in SAS 9.4
sas_f_mdy(3, 15, 2024)  # SAS: mdy(3, 15, 2024) -> 23450
sas_f_mdy(2, 30, 2024)  # SAS: mdy(2, 30, 2024) -> .
sas_f_mdy(1, 1, 1960)  # SAS: mdy(1, 1, 1960) -> 0
sas_f_mdy(NA_real_, 1, 2024)  # SAS: mdy(., 1, 2024) -> .
