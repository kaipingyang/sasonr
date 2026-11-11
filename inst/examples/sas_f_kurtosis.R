# KURTOSIS: each call is followed by the result of the same call in SAS 9.4
sas_f_kurtosis(1, 2, 3, 4, 10)  # SAS: kurtosis(1, 2, 3, 4, 10) -> 3.152
sas_f_kurtosis(2, 4, 4, 4, 5, 5, 7, 9)  # SAS: kurtosis(2, 4, 4, 4, 5, 5, 7, 9) -> 0.940625
sas_f_kurtosis(1, 2, NA_real_, 3, 4, 8)  # SAS: kurtosis(1, 2, ., 3, 4, 8) -> 2.021017076375
sas_f_kurtosis(1, 1, 1, 1)  # SAS: kurtosis(1, 1, 1, 1) -> .
