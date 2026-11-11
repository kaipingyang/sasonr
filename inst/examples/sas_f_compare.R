# COMPARE: each call is followed by the result of the same call in SAS 9.4
sas_f_compare("abc", "abc")  # SAS: compare('abc', 'abc') -> 0
sas_f_compare("abc", "abd")  # SAS: compare('abc', 'abd') -> -3
sas_f_compare("abd", "abc")  # SAS: compare('abd', 'abc') -> 3
sas_f_compare("ABC", "abc", "i")  # SAS: compare('ABC', 'abc', 'i') -> 0
sas_f_compare("abc", "ab")  # SAS: compare('abc', 'ab') -> 3
sas_f_compare("ab", "abc", ":")  # SAS: compare('ab', 'abc', ':') -> 0
