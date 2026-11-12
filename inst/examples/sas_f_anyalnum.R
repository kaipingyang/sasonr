# ANYALNUM: each call is explained above it and followed by the result of the same call in SAS 9.4

# The search starts at position 1; the first letter or digit is 'a' at position
# 3, after two blanks.
sas_f_anyalnum("  ab1")  # SAS 9.4: anyalnum('  ab1') = 3

# No letter or digit occurs in the string, so the result is 0.
sas_f_anyalnum("!!!")  # SAS 9.4: anyalnum('!!!') = 0

# Negative start -3 searches leftwards from position 3: the first alphanumeric
# character found is '1' at position 2.
sas_f_anyalnum("a1!", -3)  # SAS 9.4: anyalnum('a1!', -3) = 2

# Start 3 searches rightwards from position 3, which holds only '!', so nothing
# is found and the result is 0.
sas_f_anyalnum("!a!", 3)  # SAS 9.4: anyalnum('!a!', 3) = 0
