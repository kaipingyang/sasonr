# INPUTC: each call is followed by the result of the same call in SAS 9.4
sas_f_inputc("abc", "$3.")  # SAS: inputc('abc', '$3.') -> "abc"
sas_f_inputc("abc", "$upcase3.")  # SAS: inputc('abc', '$upcase3.') -> "ABC"
sas_f_inputc("abcdef", "$3.")  # SAS: inputc('abcdef', '$3.') -> "abc"
sas_f_inputc("abcdef", "$char.", 2)  # SAS: inputc('abcdef', '$char.', 2) -> "ab"
