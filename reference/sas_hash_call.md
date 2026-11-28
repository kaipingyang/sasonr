# Call a method of a hash object: the PDV after the call (the return code in `h$rc`, sas\_hash\_rc())

`...`: the arguments, named by their tags (` key =  `); `.stmt`: the
call is a statement (a failing call is an ERROR there).

## Usage

``` r
sas_hash_call(h, method, pdv, ..., .stmt = FALSE)
```
