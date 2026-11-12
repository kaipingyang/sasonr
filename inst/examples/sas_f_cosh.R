# COSH: each call is explained above it and followed by the result of the same call in SAS 9.4

# The hyperbolic cosine of 0 is 1.
sas_f_cosh(0)  # SAS 9.4: cosh(0) = 1

# The hyperbolic cosine of 1 is (e + 1/e) / 2.
sas_f_cosh(1)  # SAS 9.4: cosh(1) = 1.54308063481524

# The hyperbolic cosine is even, so cosh(-2) equals cosh(2).
sas_f_cosh(-2)  # SAS 9.4: cosh(-2) = 3.76219569108363

# A missing argument gives a missing result.
sas_f_cosh(NA_real_)  # SAS 9.4: cosh(.) = .
