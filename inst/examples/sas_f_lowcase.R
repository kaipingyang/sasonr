# LOWCASE: each call is explained above it and followed by its result

# LOWCASE converts all letters to lower case.
sas_f_lowcase("ABC def")  # lowcase('ABC def') = "abc def"

# A blank string stays blank.
sas_f_lowcase(" ")  # lowcase('') = ""

# Digits and punctuation are not changed.
sas_f_lowcase("A1!")  # lowcase('A1!') = "a1!"
