# ANYDIGIT: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first digit is '1' at position 3.
sas_f_anydigit("ab12")  # SAS 9.4: anydigit('ab12') = 3

# No digit occurs, so the result is 0.
sas_f_anydigit("abc")  # SAS 9.4: anydigit('abc') = 0

# Start 2 searches rightwards from position 2, skipping the '1' at position 1,
# and finds '2' at position 3.
sas_f_anydigit("1a2", 2)  # SAS 9.4: anydigit('1a2', 2) = 3

# Negative start -2 searches leftwards from position 2 ('a') and finds the
# digit '1' at position 1.
sas_f_anydigit("1a2", -2)  # SAS 9.4: anydigit('1a2', -2) = 1
