# SYMGET: each call is explained above it and followed by its result

# SYMGET returns the value of the macro variable mv, which was set with a LET
# statement.
sas_f_symget(list2env(list(mv = "hello")), "mv")  # symget('mv') = "hello"

# Macro variable names are not case sensitive.
sas_f_symget(list2env(list(mv = "hello")), "MV")  # symget('MV') = "hello"

# A name with a leading blank is invalid, so the result is blank (with a note
# in the log).
sas_f_symget(list2env(list(mv = "hello")), " mv ")  # symget(' mv ') = ""
