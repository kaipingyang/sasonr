# UPCASE: each call is explained above it and followed by the result of the same call in SAS 9.4

# UPCASE converts all letters to upper case.
sas_f_upcase("abc Def")  # SAS 9.4: upcase('abc Def') = "ABC DEF"

# A blank string stays blank.
sas_f_upcase(" ")  # SAS 9.4: upcase('') = ""

# Digits and punctuation are not changed.
sas_f_upcase("a1!")  # SAS 9.4: upcase('a1!') = "A1!"
