# MOD: each call is explained above it and followed by its result

# The remainder of 10 divided by 3 is 1.
sas_f_mod(10, 3)  # mod(10, 3) = 1

# The remainder has the sign of the dividend: -1.
sas_f_mod(-10, 3)  # mod(-10, 3) = -1

# With a negative divisor and a positive dividend the remainder is positive: 1.
sas_f_mod(10, -3)  # mod(10, -3) = 1

# MOD also works with non-integers: 5.5 - 2*2 = 1.5.
sas_f_mod(5.5, 2)  # mod(5.5, 2) = 1.5

# MOD is fuzzed: 0.3 / 0.1 is treated as exactly 3, so the remainder is 0.
sas_f_mod(0.3, 0.1)  # mod(0.3, 0.1) = 0

# Division by 0 gives a missing result (with a note in the log).
sas_f_mod(10, 0)  # mod(10, 0) = .
