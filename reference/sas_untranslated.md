# Stop: a step that has no R translation

Translated code of a step that sasonr runs itself (PROC SQL, PROC
DATASETS) consists of this call: evaluating it outside `sas_run()` stops
with a condition of class `sasonr_unsupported`.

## Usage

``` r
sas_untranslated(what, line)
```

## Arguments

  - what:
    
    The step ("PROC SQL CREATE", "PROC DATASETS").

  - line:
    
    Its SAS line.
