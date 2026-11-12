# SUBSTR: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# From position 2 to the end.
sas_f_substr("abcdef", 2)  # SAS 9.4: substr('abcdef',2) = "bcdef"

# Three characters from position 2.
sas_f_substr("abcdef", 2, 3)  # SAS 9.4: substr('abcdef',2,3) = "bcd"

# A position beyond the end is invalid: blank result, NOTE and _ERROR_ = 1 in a
# DATA step.
sas_f_substr("abcdef", 7)  # SAS 9.4: substr('abcdef',7) = ""

# A length beyond the end: the rest of the string (with a NOTE and _ERROR_ = 1
# in a DATA step).
sas_f_substr("abcdef", 2, 10)  # SAS 9.4: substr('abcdef',2,10) = "bcdef"
