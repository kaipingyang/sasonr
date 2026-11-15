# LOGSDF: each call is explained above it and followed by its result

# LOGSDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The log of the normal survival function P(X > 0) is log(0.5).
sas_f_logsdf("NORMAL", 0)  # logsdf('NORMAL', 0) = -0.693147180559945

# Far in the upper tail LOGSDF stays accurate although P(X > 10) is only about
# 7.6E-24.
sas_f_logsdf("NORMAL", 10)  # logsdf('NORMAL', 10) = -53.2312851505125

# A missing quantile gives a missing result.
sas_f_logsdf("NORMAL", NA_real_)  # logsdf('NORMAL', .) = .

options(old)
