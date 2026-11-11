# PRXPARSE: each call is followed by the result of the same call in SAS 9.4
sas_f_prxparse("/a+/")  # SAS: prxparse('/a+/') -> 1
sas_f_prxparse("/b/i")  # SAS: prxparse('/b/i') -> 2
