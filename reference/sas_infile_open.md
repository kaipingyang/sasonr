# Open an input stream

Open an input stream

## Usage

``` r
sas_infile_open(
  lines,
  line_no = seq_along(lines),
  opts = list(),
  instream = TRUE
)
```

## Arguments

  - lines:
    
    Records.

  - line\_no:
    
    Log line numbers of the records (in-stream data) or record numbers
    (external files).

  - opts:
    
    list(dlm, dsd, mode ("flowover", "missover", "truncover",
    "stopover"), firstobs, obs, pad, lines (N), end\_text).

  - instream:
    
    `TRUE` for DATALINES/CARDS (records are padded to 80).
