# INPUTC: each call is explained above it and followed by its result

# INPUTC reads a character value with a character informat that is given as a
# string.
sas_f_inputc("abc", "$3.")  # inputc('abc', '$3.') = "abc"

# The $UPCASE informat converts the value to upper case.
sas_f_inputc("abc", "$upcase3.")  # inputc('abc', '$upcase3.') = "ABC"

# The width 3 in $3. reads only the first 3 characters.
sas_f_inputc("abcdef", "$3.")  # inputc('abcdef', '$3.') = "abc"

# The optional third argument overrides the informat width: $CHAR. with width 2
# reads 'ab'.
sas_f_inputc("abcdef", "$char.", 2)  # inputc('abcdef', '$char.', 2) = "ab"
