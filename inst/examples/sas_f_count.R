# COUNT: each call is explained above it and followed by its result

# COUNT counts the occurrences of the substring 'an' in 'banana': 2.
sas_f_count("banana", "an")  # count('banana', 'an') = 2

# Modifier i ignores case, so 'b' matches the 'B' once.
sas_f_count("Banana", "b", "i")  # count('Banana', 'b', 'i') = 1

# Occurrences do not overlap: after 'aa' at position 1 the search resumes at
# position 3, so the count is 1.
sas_f_count("aaa", "aa")  # count('aaa', 'aa') = 1

# The substring does not occur, so the result is 0.
sas_f_count("abc", "x")  # count('abc', 'x') = 0
