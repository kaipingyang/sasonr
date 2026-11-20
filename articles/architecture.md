# Architecture

sasonr treats SAS as the only source language and R as the execution
engine. Nothing is translated “once and handed over”: the SAS program is
parsed and executed every time, and the generated R code can be
inspected.

    SAS program text
       │
       ▼
    Macro processor ──────────────── macro symbol tables, macro definitions (session)
       │  expands %LET, %IF, %DO, &x into SAS text; yields one complete step at
       │  a time. After the step runs, CALL SYMPUT(X) and SQL INTO: values are
       │  written back before expansion continues (expansion and execution
       │  interleave).
       ▼
    Lexer: text → token table (line, column, kind; comments and blanks kept,
       │   so the tokens give the source back exactly)
       ▼
    Parser: recursive descent for statements, Pratt parser for expressions
       │   (SAS operator precedence: comparison chains, IN, MIN/MAX, ...)
       ▼
    AST: one tree per step (DATA, PROC SQL, PROC MEANS, ...), kept as written
       ├──► sas_check(): rules that list every unsupported feature before
       │    anything runs
       ▼
    Analysis: types and lengths, readers (SET / MERGE / INPUT), BY, arrays
       ▼
    Intermediate representation: declarations in slots (KEEP, LENGTH, RETAIN,
       │   BY, ...), executable statements as a control-flow graph
       ▼
    Data-flow facts: which level can run this step?
       ├─ L1 / L2: dplyr pipelines (whole columns, BY groups, accumulators, LAG)
       └─ L3     : an explicit row loop (PDV, implicit OUTPUT, RETAIN)
       ▼
    Code generation: R calls built with rlang; SAS functions become sas_f_*()
       │   PROC SQL → own SQL parser, DuckDB for joins and grouping
       │   other procedures → R implementations
       ▼
    Engine: evaluates the step in its environment with the run-time helpers
       │   (fixed-length character storage, missing values, NOTEs, _ERROR_)
       ▼
    Session: WORK data sets, options, titles, formats (carry over between steps)
       ▼
    Output: log events      → the log (plain, coloured, HTML)
            ODS tables      → the listing, the HTML document, ODS OUTPUT data sets

## DATA step compilation levels

What matters is the result of each step, not a replica of the program
data vector, so each step is compiled to the fastest form that is
provably equivalent to row-by-row execution:

| Level  | Used when                                                                                                                | Generated code                                                  |
| ------ | ------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------- |
| **L1** | no cross-row state                                                                                                       | `dplyr::mutate()`, `filter()`, `select()`                       |
| **L2** | recognised patterns: BY groups with `FIRST.`/`LAST.`, `RETAIN` accumulators, baseline carry-forward, unconditional `LAG` | grouped vectorised code                                         |
| **L3** | anything else (conditional `LAG`, `OUTPUT` to several data sets, `DO` loops creating rows, …)                            | an explicit row loop that follows the DATA step execution rules |

The level is decided by data-flow facts on the control-flow graph. If a
vectorised step meets a condition that needs row-by-row diagnostics (for
example invalid data that sets `_ERROR_`), it is re-run as L3.

## The session

Everything a SAS program can change lives in one session object
(`SasSession`, an R6 class), never in package-level variables:

| Component        | Contents                                                                                                                                                                                                                                                  |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| WORK             | data sets (tibbles), their metadata (types, lengths, formats, labels), PROC SQL views, `_LAST_`                                                                                                                                                           |
| Format catalog   | user formats and informats from PROC FORMAT                                                                                                                                                                                                               |
| Macro            | global symbol table, local frames, macro definitions, automatic macro variables (`SYSCC`, `SYSERR`, `SQLOBS`, …)                                                                                                                                          |
| Options          | `LINESIZE`, `NOTES`, `SYNTAXCHECK`, the listing layout (`CENTER`, `DATE`, `NUMBER`, `PAGESIZE`, `FORMDLIM`); options whose only implemented value is the SAS default (`YEARCUTOFF=1940`, `ERRORS=20`, `MISSING=.`) are not stored, other values fail loud |
| Titles, filerefs | `TITLE` lines, `FILENAME` assignments                                                                                                                                                                                                                     |
| SQL back end     | the DuckDB connection, opened on first use and closed with the session                                                                                                                                                                                    |
| Sinks            | the cumulative log and listing of all submissions                                                                                                                                                                                                         |
| Diagnostics      | compile-time NOTEs of the step being translated; run-time state of the step being run (`_ERROR_` flag, data-error NOTEs, missing-value and math-error counters, observations read, PRXPARSE ids)                                                          |

`sas_run(code)` creates a throw-away session; `sas_session()` creates
one that several `sas_run(code, session = s)` calls share, one
submission after another (each submission still starts outside
syntax-check mode). `sas_work()`, `sas_meta()`, `sas_macro_vars()`,
`sas_log()` and `sas_listing()` read a session.

The generated step code does not take the session as an argument. The
engine binds the session for the duration of a submission and restores
the previous binding on exit; the run-time helpers that generated code
calls (`sas_err_sync()`, `sas_put_line()`, the functions and formats)
read the bound session. Two sessions used alternately therefore never
see each other’s state, and a `sas_run()` nested inside a running step
gets its own session without disturbing the outer one; a nested
submission to the *same* session is refused (it would rebind the running
submission’s log and macro executor).

The listing has the layout of `NOCENTER NODATE NONUMBER FORMDLIM=' '
PAGESIZE=MAX`. A procedure that writes output while `CENTER`, `DATE` or
`NUMBER` is set, while `FORMDLIM=` is not a blank (from the second page
on), or with more lines than fit on one page of a finite `PAGESIZE=`
fails loud.

### Semantics across submissions

This state does not carry over from one submission to the next:

| State                                     | Behaviour                             |
| ----------------------------------------- | ------------------------------------- |
| Syntax-check mode after an ERROR          | reset at the start of each submission |
| PRXPARSE ids                              | numbered from 1 in each submission    |
| Log line numbers                          | start at 1 in each submission         |
| PROC SQL without `QUIT;`                  | closed at the end of the submission   |
| PROC SQL options (`NUMBER`, `NOPRINT`, …) | reset with each submission            |

Function-layer calls made outside any run (`sas_f_round()` in your own
code) have no hidden state: there is no default session. Their
data-error NOTEs are discarded, they see no user formats and no
filerefs, use `LINESIZE=132` (or their `.ls` argument), and PRXPARSE ids
need an explicit session (`sas_f_prxparse(re, session = s)`).

Parsed programs, translations and results are plain S3 values; the
function, format and procedure-option registries are built once at load
time and are never changed by a run.

## Why not keyword mapping or regular expressions

A simpler design would tokenise the program and map each keyword to an R
function. sasonr does map SAS functions to `sas_f_*()` at the end, but
the layers in between are needed because the meaning of SAS code is
rarely local to one keyword:

  - **Context decides the statement.** `IF x;` is a subsetting IF and
    `IF x THEN` a condition; `=` assigns or compares; `PUT` is a
    statement or a function; `set`, `if` and `end` can be variable
    names; `a <= b in (0, 5)` is `a <= (b in (0, 5))`. This needs a
    grammar, not a keyword table.
  - **A DATA step is compiled before it runs.** Types and lengths come
    from the first appearance of a variable, LENGTH, RETAIN and the data
    sets read; the program data vector exists before the first
    observation. A statement-by- statement mapping does not know the
    length of a variable when it meets it.
  - **Many rules are implicit.** Variables read by SET are retained,
    every iteration ends with an implicit OUTPUT, and in a match-merge
    the variables of a data set that has run out of rows in a BY group
    keep their last value. None of this is written in any statement; the
    intermediate representation holds it for the whole step.
  - **Vectorisation needs the whole step.** Only after the data-flow
    analysis shows that no value crosses rows can a step run as
    whole-column dplyr code; a mapping per statement can only produce a
    row loop.
  - **Macros rewrite the program.** What `%IF &n > 0 %THEN ...`
    generates is known only after an earlier step has set `n` with CALL
    SYMPUT, so macro expansion and execution have to interleave.
  - **The log needs positions and structure.** NOTEs give
    `(Line):(Column)` places taken from the tokens; line wrapping and
    syntax-check mode after an ERROR depend on the step structure.
  - **Unsupported features are found before anything runs**, and the
    same intermediate representation feeds the executed code,
    `sas_translate()` and the scripts of `sas_script()`.

SAS syntax is not a regular language either (nested `DO`/`END` blocks,
nested parentheses, operator precedence, macro expansion), so
regex-based translation reaches a ceiling quickly; sasonr is fully
deterministic.

## Fail loud

Every statement, option, function and procedure is either implemented or
rejected with a `sasonr_unsupported` condition that names the feature
and the SAS line. Silent approximation is treated as a bug.
