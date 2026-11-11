# PROBBNML: each call is followed by the result of the same call in SAS 9.4
sas_f_probbnml(0.5, 10, 3)  # SAS: probbnml(0.5, 10, 3) -> 0.171875
sas_f_probbnml(0.2, 5, 5)  # SAS: probbnml(0.2, 5, 5) -> 1
sas_f_probbnml(0.1, 20, 0)  # SAS: probbnml(0.1, 20, 0) -> 0.1215766545906
sas_f_probbnml(NA_real_, 5, 1)  # SAS: probbnml(., 5, 1) -> .
