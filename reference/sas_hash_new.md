# Create a hash object (DECLARE HASH)

`...`: the constructor arguments by tag (`dataset`, `ordered`,
`hashexp`); `.work`: WORK (for DATASET:); `.lengths`: the lengths of the
character variables of that data set; `.line`: the program line.

## Usage

``` r
sas_hash_new(..., .work = NULL, .lengths = NULL, .line = NULL)
```
