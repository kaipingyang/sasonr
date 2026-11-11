# JULDATE7: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_juldate7(23450)  # SAS: juldate7('15MAR2024'd) -> 2024075
sas_f_juldate7(0)  # SAS: juldate7(0) -> 1960001
sas_f_juldate7(-21915)  # SAS: juldate7('31DEC1899'd) -> 1899365
sas_f_juldate7(NA_real_)  # SAS: juldate7(.) -> .
