# BAND: each call is followed by the result of the same call in SAS 9.4
sas_f_band(12, 10)  # SAS: band(12, 10) -> 8
sas_f_band(255, 15)  # SAS: band(255, 15) -> 15
sas_f_band(NA_real_, 1)  # SAS: band(., 1) -> .
