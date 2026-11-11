# INDEX: each call with the result of the same INDEX call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_index("abcabc", "bc")  # SAS index('abcabc','bc') = 2
sas_f_index("abc", "d")  # SAS index('abc','d') = 0
sas_f_index("ABC", "b")  # SAS index('ABC','b') = 0
sas_f_index("a bc", " ")  # SAS index('a bc',' ') = 2
