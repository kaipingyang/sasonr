# SUBSTR: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# From position 2 to the end.
sas_f_substr("abcdef", 2)  # substr('abcdef',2) = "bcdef"

# Three characters from position 2.
sas_f_substr("abcdef", 2, 3)  # substr('abcdef',2,3) = "bcd"

# A position beyond the end is invalid: blank result, NOTE and _ERROR_ = 1 in a
# DATA step.
sas_f_substr("abcdef", 7)  # substr('abcdef',7) = ""

# A length beyond the end: the rest of the string (with a NOTE and _ERROR_ = 1
# in a DATA step).
sas_f_substr("abcdef", 2, 10)  # substr('abcdef',2,10) = "bcdef"

# In a UTF-8 session positions count bytes: the first three bytes of a value
# with two-byte characters can be two characters.
if (isTRUE(l10n_info()[["UTF-8"]])) {
  x <- "\u00e9t\u00e9"
  print(sas_f_substr(x, 1, 3))  # the first two characters of x
  print(sas_f_length(sas_f_substr(x, 1, 3)))  # 3
}
