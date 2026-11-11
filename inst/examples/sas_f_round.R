# ROUND: each call with the result of the same ROUND call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_round(2.5)  # SAS round(2.5) = 3
sas_f_round(-2.5)  # SAS round(-2.5) = -3
sas_f_round(2.45, 0.1)  # SAS round(2.45,0.1) = 2.5
sas_f_round(1.005, 0.01)  # SAS round(1.005,0.01) = 1.01
sas_f_round(1234.5678, 100)  # SAS round(1234.5678,100) = 1200
sas_f_round(2/3, 1/3)  # SAS round(2/3,1/3) = 0.66666666666666
