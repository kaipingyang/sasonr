# CHOOSEN: each call is followed by the result of the same call in SAS 9.4
sas_f_choosen(2, 10, 20, 30)  # SAS: choosen(2, 10, 20, 30) -> 20
sas_f_choosen(-1, 10, 20, 30)  # SAS: choosen(-1, 10, 20, 30) -> 30
sas_f_choosen(5, 1, 2)  # SAS: choosen(5, 1, 2) -> .
