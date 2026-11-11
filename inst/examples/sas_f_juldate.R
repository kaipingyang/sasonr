# JULDATE: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_juldate(0)  # SAS: juldate('01JAN1960'd) -> 60001
sas_f_juldate(-21915)  # SAS: juldate('31DEC1899'd) -> 1899365
sas_f_juldate(NA_real_)  # SAS: juldate(.) -> .
