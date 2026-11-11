# PUTC: each call is followed by the result of the same call in SAS 9.4
sas_f_putc("abc", "$5.")  # SAS: putc('abc', '$5.') -> "abc"
sas_f_putc("abc", "$upcase3.")  # SAS: putc('abc', '$upcase3.') -> "ABC"
sas_f_putc("abcdef", "$3.")  # SAS: putc('abcdef', '$3.') -> "abc"
