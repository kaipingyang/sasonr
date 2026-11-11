# DATEJUL: each call is followed by the result of the same call in SAS 9.4
sas_f_datejul(2024075)  # SAS: datejul(2024075) -> 23450
sas_f_datejul(2024001)  # SAS: datejul(2024001) -> 23376
sas_f_datejul(1960001)  # SAS: datejul(1960001) -> 0
sas_f_datejul(NA_real_)  # SAS: datejul(.) -> .
