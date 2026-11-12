# LGAMMA: each call is explained above it and followed by the result of the same call in SAS 9.4

# LGAMMA is the natural log of the gamma function: for 10 it is log(9!).
sas_f_lgamma(10)  # SAS 9.4: lgamma(10) = 12.8018274800815

# LGAMMA(0.5) is the log of the square root of pi.
sas_f_lgamma(0.5)  # SAS 9.4: lgamma(0.5) = 0.5723649429247

# LGAMMA is convenient for large arguments: log(99!) is about 359.13.
sas_f_lgamma(100)  # SAS 9.4: lgamma(100) = 359.134205369575

# A missing argument gives a missing result.
sas_f_lgamma(NA_real_)  # SAS 9.4: lgamma(.) = .
