# PDF: each call is explained above it and followed by the result of the same call in SAS 9.4

# The standard uniform density is 1 everywhere inside from 0 to 1.
sas_f_pdf("UNIFORM", 0.5)  # SAS 9.4: pdf('UNIFORM', 0.5) = 1

# A missing quantile gives a missing result.
sas_f_pdf("NORMAL", NA_real_)  # SAS 9.4: pdf('NORMAL', .) = .
