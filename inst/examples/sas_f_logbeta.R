# LOGBETA: each call is explained above it and followed by its result

# LOGBETA is the natural log of the beta function: log(B(2, 3)) = log(1/12).
sas_f_logbeta(2, 3)  # logbeta(2, 3) = -2.484906649788

# A missing argument gives a missing result.
sas_f_logbeta(NA_real_, 1)  # logbeta(., 1) = .
