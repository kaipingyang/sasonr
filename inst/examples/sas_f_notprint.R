# NOTPRINT: each call is explained above it and followed by its result

# Every character of 'abc' is printable, so the result is 0.
sas_f_notprint("abc")  # notprint('abc') = 0

# Start 2 searches rightwards from position 2 and finds no character that is
# not printable.
sas_f_notprint("abc", 2)  # notprint('abc', 2) = 0
