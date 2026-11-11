# CDF: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_cdf("NORMAL", 1.96)  # SAS: cdf('NORMAL', 1.96) -> 0.97500210485178
sas_f_cdf("BINOMIAL", 3, 0.5, 10)  # SAS: cdf('BINOMIAL', 3, 0.5, 10) -> 0.171875
sas_f_cdf("POISSON", 2, 1.5)  # SAS: cdf('POISSON', 2, 1.5) -> 0.808846830538058
sas_f_cdf("T", 2, 5)  # SAS: cdf('T', 2, 5) -> 0.94903026058507
sas_f_cdf("NORMAL", NA_real_)  # SAS: cdf('NORMAL', .) -> .
