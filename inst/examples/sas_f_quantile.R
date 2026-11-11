# QUANTILE: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_quantile("NORMAL", 0.975)  # SAS: quantile('NORMAL', 0.975) -> 1.9599639845401
sas_f_quantile("T", 0.975, 10)  # SAS: quantile('T', 0.975, 10) -> 2.2281388519863
sas_f_quantile("CHISQ", 0.95, 3)  # SAS: quantile('CHISQ', 0.95, 3) -> 7.8147279032512
sas_f_quantile("BINOMIAL", 0.5, 0.5, 10)  # SAS: quantile('BINOMIAL', 0.5, 0.5, 10) -> 5
sas_f_quantile("NORMAL", NA_real_)  # SAS: quantile('NORMAL', .) -> .
