# VERIFY: each call is explained above it and followed by its result

# VERIFY returns the position of the first character that is not in the
# excerpt: '1' at position 4.
sas_f_verify("abc123", "abc")  # verify('abc123', 'abc') = 4

# Every character is in the excerpt, so the result is 0.
sas_f_verify("aaa", "a")  # verify('aaa', 'a') = 0

# The trailing blank is not in the excerpt 'ab', so VERIFY returns 3.
sas_f_verify("ab ", "ab")  # verify('ab ', 'ab') = 3

# Characters of all excerpts are allowed; 'd' at position 4 is in none of them.
sas_f_verify("abcd", "ab", "c")  # verify('abcd', 'ab', 'c') = 4
