# LOG: each call is followed by the result of the same call in SAS 9.4
sas_f_log(10)  # SAS: log(10) -> 2.30258509299405
sas_f_log(1)  # SAS: log(1) -> 0
sas_f_log(0)  # SAS: log(0) -> .
sas_f_log(-1)  # SAS: log(-1) -> .
sas_f_log(NA_real_)  # SAS: log(.) -> .
