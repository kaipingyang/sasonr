# QTR: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_qtr(23450)  # SAS: qtr('15MAR2024'd) -> 1
sas_f_qtr(23695)  # SAS: qtr('15NOV2024'd) -> 4
sas_f_qtr(0)  # SAS: qtr(0) -> 1
sas_f_qtr(NA_real_)  # SAS: qtr(.) -> .
