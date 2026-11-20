# Fetch a WORK data set and apply KEEP=/DROP=/RENAME=/FIRSTOBS=/OBS=

`work` is the WORK library (a list) or the data set itself (a data
frame, as in the tidy form of a script; `name` is then not used).

## Usage

``` r
sas_ds(
  work,
  name,
  keep = NULL,
  drop = NULL,
  rename = NULL,
  firstobs = NULL,
  obs = NULL
)
```
