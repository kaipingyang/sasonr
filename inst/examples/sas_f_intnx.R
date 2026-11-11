# INTNX: each call with the result of the same INTNX call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_intnx("month", 21929, 1)  # SAS intnx('month','15JAN2020'd,1) = 21946
sas_f_intnx("month", 21929, 1, "e")  # SAS intnx('month','15JAN2020'd,1,'e') = 21974
sas_f_intnx("month", 21929, 1, "s")  # SAS intnx('month','15JAN2020'd,1,'s') = 21960
sas_f_intnx("month", 21945, 1, "s")  # SAS intnx('month','31JAN2020'd,1,'s') = 21974
sas_f_intnx("month", 21929, -1)  # SAS intnx('month','15JAN2020'd,-1) = 21884
