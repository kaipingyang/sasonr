# COSH: each call is followed by the result of the same call in SAS 9.4
sas_f_cosh(0)  # SAS: cosh(0) -> 1
sas_f_cosh(1)  # SAS: cosh(1) -> 1.54308063481524
sas_f_cosh(-2)  # SAS: cosh(-2) -> 3.76219569108363
sas_f_cosh(NA_real_)  # SAS: cosh(.) -> .
