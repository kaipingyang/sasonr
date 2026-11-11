# HMS: each call is followed by the result of the same call in SAS 9.4
sas_f_hms(10, 30, 15)  # SAS: hms(10, 30, 15) -> 37815
sas_f_hms(0, 90, 0)  # SAS: hms(0, 90, 0) -> 5400
sas_f_hms(1, 0, 0.25)  # SAS: hms(1, 0, 0.25) -> 3600.25
sas_f_hms(NA_real_, 1, 1)  # SAS: hms(., 1, 1) -> .
