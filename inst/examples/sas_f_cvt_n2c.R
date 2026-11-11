# CVT_N2C: each call is followed by the result of the same call in SAS 9.4
sas_f_cvt_n2c(3.5)  # SAS: 3.5 -> "         3.5"
sas_f_cvt_n2c(-12)  # SAS: -12 -> "         -12"
sas_f_cvt_n2c(1e15)  # SAS: 1e15 -> "        1E15"
sas_f_cvt_n2c(1/3)  # SAS: 1/3 -> "0.3333333333"
