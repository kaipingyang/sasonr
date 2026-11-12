# LOWCASE: each call is explained above it and followed by the result of the same call in SAS 9.4

# LOWCASE converts all letters to lower case.
sas_f_lowcase("ABC def")  # SAS 9.4: lowcase('ABC def') = "abc def"

# A blank string stays blank.
sas_f_lowcase(" ")  # SAS 9.4: lowcase('') = ""

# Digits and punctuation are not changed.
sas_f_lowcase("A1!")  # SAS 9.4: lowcase('A1!') = "a1!"
