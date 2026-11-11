# IFN: each call is followed by the result of the same call in SAS 9.4
sas_f_ifn(1, 10, 20)  # SAS: ifn(1, 10, 20) -> 10
sas_f_ifn(0, 10, 20)  # SAS: ifn(0, 10, 20) -> 20
sas_f_ifn(NA_real_, 10, 20, 99)  # SAS: ifn(., 10, 20, 99) -> 99
sas_f_ifn(-1, 10, 20)  # SAS: ifn(-1, 10, 20) -> 10
