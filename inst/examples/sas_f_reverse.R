# REVERSE: each call is followed by the result of the same call in SAS 9.4
sas_f_reverse("abc")  # SAS: reverse('abc') -> "cba"
sas_f_reverse("abc  ")  # SAS: reverse('abc  ') -> "  cba"
sas_f_reverse("")  # SAS: reverse('') -> ""
