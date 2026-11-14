# ANYALPHA: each call is explained above it and followed by its result

# The first alphabetic character is 'a' at position 3.
sas_f_anyalpha("12ab")  # anyalpha('12ab') = 3

# The string contains only digits, so the result is 0.
sas_f_anyalpha("123")  # anyalpha('123') = 0

# Start 3 searches rightwards from position 3 and finds only digits, so the
# result is 0.
sas_f_anyalpha("ab12", 3)  # anyalpha('ab12', 3) = 0

# Negative start -4 searches leftwards from position 4: the first letter found
# is 'b' at position 2.
sas_f_anyalpha("ab12", -4)  # anyalpha('ab12', -4) = 2
