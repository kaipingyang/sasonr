# NOTSPACE: each call is explained above it and followed by its result

# The first nonblank character is at position 3.
sas_f_notspace("  ab")  # notspace('  ab') = 3

# The string is all blanks, so the result is 0.
sas_f_notspace("   ")  # notspace('   ') = 0

# Start 2 searches rightwards from position 2, skips the blanks at 2 and 3, and
# finds 'b' at position 4.
sas_f_notspace("a  b", 2)  # notspace('a  b', 2) = 4
