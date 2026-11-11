# ORDINAL: each call is followed by the result of the same call in SAS 9.4
sas_f_ordinal(2, 5, 1, 3)  # SAS: ordinal(2, 5, 1, 3) -> 3
sas_f_ordinal(1, NA_real_, 2)  # SAS: ordinal(1, ., 2) -> .
sas_f_ordinal(3, 3, 1, 2)  # SAS: ordinal(3, 3, 1, 2) -> 3
