# VERIFY: each call is followed by the result of the same call in SAS 9.4
sas_f_verify("abc123", "abc")  # SAS: verify('abc123', 'abc') -> 4
sas_f_verify("aaa", "a")  # SAS: verify('aaa', 'a') -> 0
sas_f_verify("ab ", "ab")  # SAS: verify('ab ', 'ab') -> 3
sas_f_verify("abcd", "ab", "c")  # SAS: verify('abcd', 'ab', 'c') -> 4
