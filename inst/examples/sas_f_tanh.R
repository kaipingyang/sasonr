# TANH: each call is followed by the result of the same call in SAS 9.4
sas_f_tanh(0)  # SAS: tanh(0) -> 0
sas_f_tanh(1)  # SAS: tanh(1) -> 0.761594155955765
sas_f_tanh(20)  # SAS: tanh(20) -> 1
sas_f_tanh(NA_real_)  # SAS: tanh(.) -> .
