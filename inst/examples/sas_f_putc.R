# PUTC: each call is explained above it and followed by the result of the same call in SAS 9.4

# PUTC applies a character format given as a string; $5. pads 'abc' to width 5.
sas_f_putc("abc", "$5.")  # SAS 9.4: putc('abc', '$5.') = "abc"

# The $UPCASE3. format writes the value in upper case.
sas_f_putc("abc", "$upcase3.")  # SAS 9.4: putc('abc', '$upcase3.') = "ABC"

# $HEX6. writes 616263 (two hex digits per character); a new variable gets the
# length of the first argument (3), so SAS stores 616.
substr(sas_f_putc("abc", "$hex6."), 1, 3)  # SAS 9.4: putc('abc', '$hex6.') = "616"

# The format $3. writes only the first 3 characters.
sas_f_putc("abcdef", "$3.")  # SAS 9.4: putc('abcdef', '$3.') = "abc"
