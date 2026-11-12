# ANYLOWER: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first lowercase letter is 'c' at position 3.
sas_f_anylower("ABc")  # SAS 9.4: anylower('ABc') = 3

# All letters are uppercase, so the result is 0.
sas_f_anylower("ABC")  # SAS 9.4: anylower('ABC') = 0

# Negative start -2 searches leftwards from position 2 ('B') and finds the
# lowercase 'a' at position 1.
sas_f_anylower("aBc", -2)  # SAS 9.4: anylower('aBc', -2) = 1
