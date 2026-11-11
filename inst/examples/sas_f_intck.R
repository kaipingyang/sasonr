# INTCK: each call with the result of the same INTCK call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_intck("month", 21945, 21946)  # SAS intck('month','31JAN2020'd,'01FEB2020'd) = 1
sas_f_intck("month", 21945, 21946, "c")  # SAS intck('month','31JAN2020'd,'01FEB2020'd,'c') = 0
sas_f_intck("year", 21914, 21915)  # SAS intck('year','31DEC2019'd,'01JAN2020'd) = 1
sas_f_intck("month", 21975, 21915)  # SAS intck('month','01MAR2020'd,'01JAN2020'd) = -2
