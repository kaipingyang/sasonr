# VERIFY: each call is explained above it and followed by the result of the same call in SAS 9.4

# VERIFY returns the position of the first character that is not in the
# excerpt: '1' at position 4.
sas_f_verify("abc123", "abc")  # SAS 9.4: verify('abc123', 'abc') = 4

# Every character is in the excerpt, so the result is 0.
sas_f_verify("aaa", "a")  # SAS 9.4: verify('aaa', 'a') = 0

# The trailing blank is not in the excerpt 'ab', so VERIFY returns 3.
sas_f_verify("ab ", "ab")  # SAS 9.4: verify('ab ', 'ab') = 3

# Characters of all excerpts are allowed; 'd' at position 4 is in none of them.
sas_f_verify("abcd", "ab", "c")  # SAS 9.4: verify('abcd', 'ab', 'c') = 4
