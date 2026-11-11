# COMPRESS: each call with the result of the same COMPRESS call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_compress("a b c")  # SAS compress('a b c') = "abc"
sas_f_compress("a-b-c", "-")  # SAS compress('a-b-c','-') = "abc"
sas_f_compress("a1 b2", "", "d")  # SAS compress('a1 b2','','d') = "ab"
sas_f_compress("a1 b2", "", "kd")  # SAS compress('a1 b2','','kd') = "1 2"
sas_f_compress("ABC abc", "a", "i")  # SAS compress('ABC abc','a','i') = "BC bc"
