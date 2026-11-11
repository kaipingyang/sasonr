# PRXMATCH: each call is followed by the result of the same call in SAS 9.4
sas_f_prxmatch("/b+/", "abbc")  # SAS: prxmatch('/b+/', 'abbc') -> 2
sas_f_prxmatch("/z/", "abc")  # SAS: prxmatch('/z/', 'abc') -> 0
sas_f_prxmatch("/B/i", "abc")  # SAS: prxmatch('/B/i', 'abc') -> 2
sas_f_prxmatch("/^a/", " abc")  # SAS: prxmatch('/^a/', ' abc') -> 0
