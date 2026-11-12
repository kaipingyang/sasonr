# LOGSDF: each call is explained above it and followed by the result of the same call in SAS 9.4

# The log of the normal survival function P(X > 0) is log(0.5).
sas_f_logsdf("NORMAL", 0)  # SAS 9.4: logsdf('NORMAL', 0) = -0.693147180559945

# Far in the upper tail LOGSDF stays accurate although P(X > 10) is only about
# 7.6E-24.
sas_f_logsdf("NORMAL", 10)  # SAS 9.4: logsdf('NORMAL', 10) = -53.2312851505125

# A missing quantile gives a missing result.
sas_f_logsdf("NORMAL", NA_real_)  # SAS 9.4: logsdf('NORMAL', .) = .
