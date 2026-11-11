# SDF: each call is followed by the result of the same call in SAS 9.4
sas_f_sdf("NORMAL", 1.96)  # SAS: sdf('NORMAL', 1.96) -> 0.02499789514822
sas_f_sdf("POISSON", 2, 1.5)  # SAS: sdf('POISSON', 2, 1.5) -> 0.19115316946194
sas_f_sdf("EXPONENTIAL", 1)  # SAS: sdf('EXPONENTIAL', 1) -> 0.367879441171442
sas_f_sdf("NORMAL", NA_real_)  # SAS: sdf('NORMAL', .) -> .
