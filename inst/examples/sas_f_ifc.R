# IFC: each call is followed by the result of the same call in SAS 9.4
sas_f_ifc(1, "yes", "no")  # SAS: ifc(1, 'yes', 'no') -> "yes"
sas_f_ifc(0, "yes", "no")  # SAS: ifc(0, 'yes', 'no') -> "no"
sas_f_ifc(NA_real_, "yes", "no", "miss")  # SAS: ifc(., 'yes', 'no', 'miss') -> "miss"
sas_f_ifc(2 > 1, "a", "b")  # SAS: ifc(2 > 1, 'a', 'b') -> "a"
