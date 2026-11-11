# COUNT: each call is followed by the result of the same call in SAS 9.4
sas_f_count("banana", "an")  # SAS: count('banana', 'an') -> 2
sas_f_count("Banana", "b", "i")  # SAS: count('Banana', 'b', 'i') -> 1
sas_f_count("aaa", "aa")  # SAS: count('aaa', 'aa') -> 1
sas_f_count("abc", "x")  # SAS: count('abc', 'x') -> 0
