# SCAN: each call with the result of the same SCAN call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_scan("a b c", 1)  # SAS scan('a b c',1) = "a"
sas_f_scan("a b c", -1)  # SAS scan('a b c',-1) = "c"
sas_f_scan("a b c", 4)  # SAS scan('a b c',4) = ""
sas_f_scan("a,b,,c", 3)  # SAS scan('a,b,,c',3) = "c"
