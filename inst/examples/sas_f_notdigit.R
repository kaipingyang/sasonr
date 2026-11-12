# NOTDIGIT: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first character that is not a digit is 'a' at position 3.
sas_f_notdigit("12a")  # SAS 9.4: notdigit('12a') = 3

# All characters are digits, so the result is 0.
sas_f_notdigit("123")  # SAS 9.4: notdigit('123') = 0

# Negative start -3 searches leftwards from position 3 ('2') and finds 'a' at
# position 2.
sas_f_notdigit("1a2b", -3)  # SAS 9.4: notdigit('1a2b', -3) = 2
