# NVALID: each call is explained above it and followed by the result of the same call in SAS 9.4

# NVALID returns 1 if the string is a valid SAS variable name; under the
# default V7 rules 'abc' is valid.
sas_f_nvalid("abc")  # SAS 9.4: nvalid('abc') = 1

# A V7 name cannot start with a digit, so the result is 0.
sas_f_nvalid("1abc")  # SAS 9.4: nvalid('1abc') = 0

# V7 names cannot contain blanks, so 'my var' is not valid.
sas_f_nvalid("my var", "v7")  # SAS 9.4: nvalid('my var', 'v7') = 0

# Option 'any' accepts any characters (1 to 32 bytes), including blanks.
sas_f_nvalid("my var", "any")  # SAS 9.4: nvalid('my var', 'any') = 1

# Option 'nliteral' accepts V7 names as well as name literals such as 'my
# var'n; '_x1' is a V7 name.
sas_f_nvalid("_x1", "nliteral")  # SAS 9.4: nvalid('_x1', 'nliteral') = 1
