# Call a SAS function with SAS's handling of missing and invalid arguments

Used by translated code for the functions in the internal policy table:
counts the log NOTEs ("Missing values were generated", "Invalid argument
to function", "Mathematical operations could not be performed"), sets
`_ERROR_` and gives the missing results that the policy table defines.

## Usage

``` r
sas_fcall(.name, .f, ..., .at = NULL, .exact = TRUE)
```

## Arguments

  - .name:
    
    Function name.

  - .f:
    
    Implementation.

  - ...:
    
    Arguments.

  - .at:
    
    `c(line, column)` of the call.

  - .exact:
    
    `FALSE` in vectorised code (counts are redone row by row).
