# CATX: each call with the result of the same CATX call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_catx("-", "a", "b", "c")  # SAS catx('-','a','b','c') = "a-b-c"
sas_f_catx("-", "  a ", "b  ", " c")  # SAS catx('-','  a ','b  ',' c') = "a-b-c"
sas_f_catx("-", "a", " ", "c")  # SAS catx('-','a',' ','c') = "a-c"
sas_f_catx("-", "a", NA, "c")  # SAS catx('-','a',.,'c') = "a-.-c"
