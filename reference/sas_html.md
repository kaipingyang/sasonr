# The HTML document of a result

The procedure output of a result as an HTML document: one table per ODS
table, under the titles, the procedure title and the BY lines. Tables go
to the HTML destination while it is open: it is open when a program
starts, `ODS HTML CLOSE;` closes it and `ODS HTML;` opens it again (in a
session the state carries over to the next submission). `ODS HTML FILE=`
and the other ODS HTML options are not supported; write the document
with `file`.

## Usage

``` r
sas_html(x, file = NULL, log = FALSE)
```

## Arguments

  - x:
    
    A result of `sas_run()`, or of a data-level PROC function
    (`sas_means_df()`, `sas_tabulate_df()`).

  - file:
    
    `NULL`, or a path: the document is written there.

  - log:
    
    `TRUE` adds the log after the tables (`sas_log(x, "html")`).

## Value

The document as one string (invisibly when `file` is given).

## Details

The style sheet is `system.file("css", "sasonr.css", package =
"sasonr")`; the document embeds it.

## Examples

``` r
res <- sas_run("
data a;
  input grp $ x;
  datalines;
a 1
a 2
b 4
;
run;
proc means data=a;
  class grp;
  var x;
run;
")
doc <- sas_html(res)
substr(doc, 1, 60)
#> [1] "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>"
```
