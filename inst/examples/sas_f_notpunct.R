# NOTPUNCT: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first character that is not punctuation is 'a' at position 3.
sas_f_notpunct("!!a")  # SAS 9.4: notpunct('!!a') = 3

# All characters are punctuation, so the result is 0.
sas_f_notpunct("!?")  # SAS 9.4: notpunct('!?') = 0

# Start 2 searches rightwards from position 2, skips '!', and finds 'b' at
# position 3.
sas_f_notpunct("a!b", 2)  # SAS 9.4: notpunct('a!b', 2) = 3
