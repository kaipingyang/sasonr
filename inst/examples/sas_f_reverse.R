# REVERSE: each call is explained above it and followed by the result of the same call in SAS 9.4

# REVERSE reverses the order of the characters.
sas_f_reverse("abc")  # SAS 9.4: reverse('abc') = "cba"

# Trailing blanks become leading blanks.
sas_f_reverse("abc  ")  # SAS 9.4: reverse('abc  ') = "  cba"

# A blank string stays blank.
sas_f_reverse(" ")  # SAS 9.4: reverse('') = ""
