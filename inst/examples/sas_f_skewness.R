# SKEWNESS: each call is followed by the result of the same call in SAS 9.4
sas_f_skewness(1, 2, 3, 10)  # SAS: skewness(1, 2, 3, 10) -> 1.7636326148039
sas_f_skewness(1, 2, NA_real_, 3)  # SAS: skewness(1, 2, ., 3) -> 0
sas_f_skewness(2, 4, 4, 4, 5, 5, 7, 9)  # SAS: skewness(2, 4, 4, 4, 5, 5, 7, 9) -> 0.8184875533568
sas_f_skewness(1, 1, 1)  # SAS: skewness(1, 1, 1) -> .
