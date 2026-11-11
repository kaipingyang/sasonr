# CVT_C2N: each call is followed by the result of the same call in SAS 9.4
sas_f_cvt_c2n(" 12 ")  # SAS: ' 12 ' + 0 -> 12
sas_f_cvt_c2n("1e3")  # SAS: '1e3' + 0 -> 1000
sas_f_cvt_c2n("-.5")  # SAS: '-.5' + 0 -> -0.5
sas_f_cvt_c2n("abc")  # SAS: 'abc' + 0 -> .
