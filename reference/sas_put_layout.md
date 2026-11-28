# PUT with column/line pointer controls (`@n`, `+n`, `/`)

Pieces are written into a line buffer at the current column
(overwriting); list and named output leave one blank after the value.
`/` starts a new line. `.ls`: the line size (the log's LINESIZE, a
FILE's LRECL; `.file`); `.guard`: the rows where the PUT executes
(vectorised steps): the result then has one element per row, NA where no
line is written.

## Usage

``` r
sas_put_layout(
  pieces,
  .ls = rt_linesize(),
  .line = NULL,
  .file = FALSE,
  .guard = NULL
)
```
