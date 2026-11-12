# INPUTC: each call is explained above it and followed by the result of the same call in SAS 9.4

# INPUTC reads a character value with a character informat that is given as a
# string.
sas_f_inputc("abc", "$3.")  # SAS 9.4: inputc('abc', '$3.') = "abc"

# The $UPCASE informat converts the value to upper case.
sas_f_inputc("abc", "$upcase3.")  # SAS 9.4: inputc('abc', '$upcase3.') = "ABC"

# The width 3 in $3. reads only the first 3 characters.
sas_f_inputc("abcdef", "$3.")  # SAS 9.4: inputc('abcdef', '$3.') = "abc"

# The optional third argument overrides the informat width: $CHAR. with width 2
# reads 'ab'.
sas_f_inputc("abcdef", "$char.", 2)  # SAS 9.4: inputc('abcdef', '$char.', 2) = "ab"
