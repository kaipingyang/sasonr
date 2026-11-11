# POISSON: each call is followed by the result of the same call in SAS 9.4
sas_f_poisson(1.5, 2)  # SAS: poisson(1.5, 2) -> 0.80884683053806
sas_f_poisson(0.5, 0)  # SAS: poisson(0.5, 0) -> 0.606530659712633
sas_f_poisson(10, 15)  # SAS: poisson(10, 15) -> 0.95125959669602
sas_f_poisson(NA_real_, 1)  # SAS: poisson(., 1) -> .
