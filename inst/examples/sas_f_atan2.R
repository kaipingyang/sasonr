# ATAN2: each call is followed by the result of the same call in SAS 9.4
sas_f_atan2(1, 1)  # SAS: atan2(1, 1) -> 0.785398163397448
sas_f_atan2(1, -1)  # SAS: atan2(1, -1) -> 2.35619449019234
sas_f_atan2(-1, -1)  # SAS: atan2(-1, -1) -> -2.35619449019234
sas_f_atan2(NA_real_, 1)  # SAS: atan2(., 1) -> .
