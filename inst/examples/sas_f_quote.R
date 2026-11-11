# QUOTE: each call is followed by the result of the same call in SAS 9.4
sas_f_quote("abc")  # SAS: quote('abc') -> "\"abc\""
sas_f_quote("a\"b")  # SAS: quote('a"b') -> "\"a\"\"b\""
sas_f_quote("abc", "'")  # SAS: quote('abc', "'") -> "'abc'"
