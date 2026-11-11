# PROBCHI: each call is followed by the result of the same call in SAS 9.4
sas_f_probchi(3.84, 1)  # SAS: probchi(3.84, 1) -> 0.94995647875129
sas_f_probchi(2, 3)  # SAS: probchi(2, 3) -> 0.4275932955291
sas_f_probchi(2, 3, 1)  # SAS: probchi(2, 3, 1) -> 0.3082525076649
sas_f_probchi(NA_real_, 1)  # SAS: probchi(., 1) -> .
