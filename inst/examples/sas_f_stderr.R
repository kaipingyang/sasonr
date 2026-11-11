# STDERR: each call is followed by the result of the same call in SAS 9.4
sas_f_stderr(1, 2, 3, 4)  # SAS: stderr(1, 2, 3, 4) -> 0.645497224367903
sas_f_stderr(2, 4)  # SAS: stderr(2, 4) -> 1
sas_f_stderr(5, NA_real_)  # SAS: stderr(5, .) -> .
