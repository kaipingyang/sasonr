# Evaluate a macro expression

Evaluate a macro expression

## Usage

``` r
mac_eval(text, float = FALSE, what = "%EVAL")
```

## Arguments

  - text:
    
    Resolved expression text.

  - float:
    
    `FALSE` for %EVAL (integer arithmetic), `TRUE` for %SYSEVALF.

## Value

A list with `kind` ("num" or "str") and `value`.
