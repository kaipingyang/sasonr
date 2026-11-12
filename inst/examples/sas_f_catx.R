# CATX: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# Join the items with the delimiter '-'.
sas_f_catx("-", "a", "b", "c")  # SAS 9.4: catx('-','a','b','c') = "a-b-c"

# Leading and trailing blanks of every item are removed before joining.
sas_f_catx("-", "  a ", "b  ", " c")  # SAS 9.4: catx('-','  a ','b  ',' c') = "a-b-c"

# A blank item is skipped (no doubled delimiter).
sas_f_catx("-", "a", " ", "c")  # SAS 9.4: catx('-','a',' ','c') = "a-c"

# A numeric missing item is not skipped: it is written as '.'.
sas_f_catx("-", "a", NA, "c")  # SAS 9.4: catx('-','a',.,'c') = "a-.-c"
