# SYMGET: each call is explained above it and followed by the result of the same call in SAS 9.4

# SYMGET returns the value of the macro variable mv, which was set with a LET
# statement.
sas_f_symget(list2env(list(mv = "hello")), "mv")  # SAS 9.4: symget('mv') = "hello"

# Macro variable names are not case sensitive.
sas_f_symget(list2env(list(mv = "hello")), "MV")  # SAS 9.4: symget('MV') = "hello"

# A name with a leading blank is invalid, so the result is blank (with a note
# in the log).
sas_f_symget(list2env(list(mv = "hello")), " mv ")  # SAS 9.4: symget(' mv ') = ""
