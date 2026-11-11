# COALESCE: each call is followed by the result of the same call in SAS 9.4
sas_f_coalesce(NA_real_, 2, 3)  # SAS: coalesce(., 2, 3) -> 2
sas_f_coalesce(NA_real_, NA_real_)  # SAS: coalesce(., .) -> .
sas_f_coalesce(sas_missing(".A"), sas_missing("._"), 5)  # SAS: coalesce(.A, ._, 5) -> 5
