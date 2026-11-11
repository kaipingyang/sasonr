# NMISS: each call is followed by the result of the same call in SAS 9.4
sas_f_nmiss(1, NA_real_, 3)  # SAS: nmiss(1, ., 3) -> 1
sas_f_nmiss(sas_missing(".A"), sas_missing("._"), 0)  # SAS: nmiss(.A, ._, 0) -> 2
sas_f_nmiss(1, 2)  # SAS: nmiss(1, 2) -> 0
