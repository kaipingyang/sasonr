# PROPCASE: each call is followed by the result of the same call in SAS 9.4
sas_f_propcase("hello WORLD")  # SAS: propcase('hello WORLD') -> "Hello World"
sas_f_propcase("a-b c", "-")  # SAS: propcase('a-b c', '-') -> "A-B c"
sas_f_propcase("jean-luc")  # SAS: propcase('jean-luc') -> "Jean-Luc"
sas_f_propcase("")  # SAS: propcase('') -> ""
