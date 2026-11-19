# ANYPRINT: each call is explained above it and followed by its result

# The first printable character is 'a' at position 1.
sas_f_anyprint("abc")  # anyprint('abc') = 1

# Start 2 searches rightwards from position 2 and finds 'b' at position 2.
sas_f_anyprint("abc", 2)  # anyprint('abc', 2) = 2

# Negative start -2 searches leftwards from position 2 and finds 'b' at
# position 2.
sas_f_anyprint("abc", -2)  # anyprint('abc', -2) = 2
