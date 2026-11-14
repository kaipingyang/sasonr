# REPEAT: each call is explained above it and followed by its result

# REPEAT(s, n) returns s followed by n further copies, so 'ab' appears 3 times.
sas_f_repeat("ab", 2)  # repeat('ab', 2) = "ababab"

# With n = 0 the string is returned once.
sas_f_repeat("x", 0)  # repeat('x', 0) = "x"

# '-' followed by 4 further copies gives 5 dashes.
sas_f_repeat("-", 4)  # repeat('-', 4) = "-----"
