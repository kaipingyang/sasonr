# FINDW: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_findw("the cat sat", "cat")  # SAS: findw('the cat sat', 'cat') -> 5
sas_f_findw("the cat sat", "at")  # SAS: findw('the cat sat', 'at') -> 0
sas_f_findw("a,b,c", "b", ",")  # SAS: findw('a,b,c', 'b', ',') -> 3
sas_f_findw("The Cat", "cat", " ", "i")  # SAS: findw('The Cat', 'cat', ' ', 'i') -> 5
sas_f_findw("the cat sat", "sat", " ", "e")  # SAS: findw('the cat sat', 'sat', ' ', 'e') -> 3
