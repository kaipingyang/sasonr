# LOGSDF: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_logsdf("NORMAL", 0)  # SAS: logsdf('NORMAL', 0) -> -0.693147180559945
sas_f_logsdf("T", 1, 3)  # SAS: logsdf('T', 1, 3) -> -1.6321892244941
sas_f_logsdf("NORMAL", NA_real_)  # SAS: logsdf('NORMAL', .) -> .
