# INDEXW: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_indexw("the cat sat", "cat")  # SAS: indexw('the cat sat', 'cat') -> 5
sas_f_indexw("the cat sat", "at")  # SAS: indexw('the cat sat', 'at') -> 0
sas_f_indexw("a;b;c", "b", ";")  # SAS: indexw('a;b;c', 'b', ';') -> 3
sas_f_indexw("cat", "cat")  # SAS: indexw('cat', 'cat') -> 1
