# UPCASE: each call is explained above it and followed by its result

# UPCASE converts all letters to upper case.
sas_f_upcase("abc Def")  # upcase('abc Def') = "ABC DEF"

# A blank string stays blank.
sas_f_upcase(" ")  # upcase('') = ""

# Digits and punctuation are not changed.
sas_f_upcase("a1!")  # upcase('a1!') = "A1!"
