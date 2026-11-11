# CV: each call is followed by the result of the same call in SAS 9.4
sas_f_cv(1, 2, 3, 4)  # SAS: cv(1, 2, 3, 4) -> 51.6397779494322
sas_f_cv(2, 2)  # SAS: cv(2, 2) -> 0
sas_f_cv(1, NA_real_, 3)  # SAS: cv(1, ., 3) -> 70.7106781186548
