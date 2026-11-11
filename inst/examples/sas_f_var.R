# VAR: each call is followed by the result of the same call in SAS 9.4
sas_f_var(1, 2, 3, 4)  # SAS: var(1, 2, 3, 4) -> 1.66666666666667
sas_f_var(5, NA_real_, 7)  # SAS: var(5, ., 7) -> 2
sas_f_var(5, NA_real_)  # SAS: var(5, .) -> .
