# STD: each call is followed by the result of the same call in SAS 9.4
sas_f_std(1, 2, 3, 4)  # SAS: std(1, 2, 3, 4) -> 1.29099444873581
sas_f_std(5, NA_real_, 7)  # SAS: std(5, ., 7) -> 1.4142135623731
sas_f_std(5, NA_real_)  # SAS: std(5, .) -> .
sas_f_std(2, 4, 4, 4, 5, 5, 7, 9)  # SAS: std(2, 4, 4, 4, 5, 5, 7, 9) -> 2.1380899352994
