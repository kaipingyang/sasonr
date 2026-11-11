# WHICHN: each call is followed by the result of the same call in SAS 9.4
sas_f_whichn(2, 1, 2, 3)  # SAS: whichn(2, 1, 2, 3) -> 2
sas_f_whichn(9, 1, 2)  # SAS: whichn(9, 1, 2) -> 0
sas_f_whichn(NA_real_, 1, NA_real_, 3)  # SAS: whichn(., 1, ., 3) -> .
