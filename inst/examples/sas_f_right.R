# RIGHT: each call is followed by the result of the same call in SAS 9.4
sas_f_right("abc  ")  # SAS: right('abc  ') -> "  abc"
sas_f_right("  abc")  # SAS: right('  abc') -> "  abc"
sas_f_right("")  # SAS: right('') -> ""
