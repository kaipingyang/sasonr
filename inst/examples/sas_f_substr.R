# SUBSTR: each call with the result of the same SUBSTR call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_substr("abcdef", 2)  # SAS substr('abcdef',2) = "bcdef"
sas_f_substr("abcdef", 2, 3)  # SAS substr('abcdef',2,3) = "bcd"
sas_f_substr("abcdef", 7)  # SAS substr('abcdef',7) = ""
sas_f_substr("abcdef", 2, 10)  # SAS substr('abcdef',2,10) = "bcdef"
