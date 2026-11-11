# SMALLEST: each call is followed by the result of the same call in SAS 9.4
sas_f_smallest(1, 3, 9, 5)  # SAS: smallest(1, 3, 9, 5) -> 3
sas_f_smallest(2, NA_real_, 9, 5)  # SAS: smallest(2, ., 9, 5) -> 9
sas_f_smallest(3, 1, 2)  # SAS: smallest(3, 1, 2) -> .
sas_f_smallest(1, NA_real_, NA_real_)  # SAS: smallest(1, ., .) -> .
