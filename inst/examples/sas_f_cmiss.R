# CMISS: each call is followed by the result of the same call in SAS 9.4
sas_f_cmiss(1, NA_real_, "a", "")  # SAS: cmiss(1, ., 'a', '') -> 2
sas_f_cmiss(sas_missing(".A"))  # SAS: cmiss(.A) -> 1
sas_f_cmiss("x", 2)  # SAS: cmiss('x', 2) -> 0
