# Start the state of a row-by-row DATA step

`pdv`: the initial program data vector; `reset`: the variables set to
their initial value at the start of each iteration; `outputs`: the
variables written to each output data set; `dump`: the error dump spec
(`sas_dump_spec()`); `readers`: the reader objects of its SET, MERGE and
INPUT statements.

## Usage

``` r
sas_step_state(
  pdv,
  reset = character(),
  outputs = list(),
  dump = NULL,
  readers = list()
)
```
