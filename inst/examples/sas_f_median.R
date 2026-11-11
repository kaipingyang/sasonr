# MEDIAN: each call is followed by the result of the same call in SAS 9.4
sas_f_median(1, 3, 2, 10)  # SAS: median(1, 3, 2, 10) -> 2.5
sas_f_median(5, NA_real_, 1)  # SAS: median(5, ., 1) -> 3
sas_f_median(7)  # SAS: median(7) -> 7
sas_f_median(NA_real_, NA_real_)  # SAS: median(., .) -> .
