# TRANWRD: each call is followed by the result of the same call in SAS 9.4
sas_f_tranwrd("banana", "an", "AN")  # SAS: tranwrd('banana', 'an', 'AN') -> "bANANa"
sas_f_tranwrd("a b c", " ", "-")  # SAS: tranwrd('a b c', ' ', '-') -> "a-b-c"
