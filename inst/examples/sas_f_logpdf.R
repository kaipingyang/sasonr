# LOGPDF: each call is followed by the result of the same call in SAS 9.4
sas_f_logpdf("NORMAL", 0)  # SAS: logpdf('NORMAL', 0) -> -0.91893853320467
sas_f_logpdf("EXPONENTIAL", 1, 2)  # SAS: logpdf('EXPONENTIAL', 1, 2) -> -1.19314718055995
sas_f_logpdf("BINOMIAL", 3, 0.5, 10)  # SAS: logpdf('BINOMIAL', 3, 0.5, 10) -> -2.1439800628174
sas_f_logpdf("NORMAL", NA_real_)  # SAS: logpdf('NORMAL', .) -> .
