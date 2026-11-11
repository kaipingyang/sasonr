# LARGEST: each call is followed by the result of the same call in SAS 9.4
sas_f_largest(1, 3, 9, 5)  # SAS: largest(1, 3, 9, 5) -> 9
sas_f_largest(2, 3, 9, 5)  # SAS: largest(2, 3, 9, 5) -> 5
sas_f_largest(1, NA_real_, 4, NA_real_)  # SAS: largest(1, ., 4, .) -> 4
sas_f_largest(3, 3, NA_real_)  # SAS: largest(3, 3, .) -> .
