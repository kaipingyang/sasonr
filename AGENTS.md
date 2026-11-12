# AGENTS.md

Guidance for AI agents working in this repository. `CONTRIBUTING.md` covers
the human-facing rules; this file records the environment facts and hard
constraints that are not visible from the code.

## What this project is

`sasonr` runs SAS 9.4 programs on an R engine and translates SAS to R
deterministically. It has a hand-written lexer, recursive-descent statement
parser and Pratt expression parser — not a regex translator. Every behaviour is
judged against output produced by a real SAS 9.4 installation.

## Golden rules

1. **SAS output is the only source of truth.** Never write expected values by
   hand, from documentation examples, or from another interpreter. If you
   cannot run SAS for a behaviour, the feature fails loud instead.
2. **Fail loud.** Unverified behaviour must stop with `sasonr_unsupported`
   (or `sasonr_not_verified` under strict mode), naming the feature and the
   SAS line. Silent approximation is a bug, and a half-correct result is worse
   than a refusal.
3. **Deterministic.** No LLM, no network service at run time.
4. **Never commit anything SAS produced.** Logs carry the licensee's site
   number, and SAS data sets belong to the installation. `.gitignore` covers
   `*.log`, `*.lst`, `*.sas7bdat`, `*.sas7bcat`, `.sasuser/`, `results.txt`,
   `inst/sashelp/` and `.local/`. Do not add exceptions.
5. **Never push, force-push, rewrite history, or overwrite stored gold
   output.** Work on `dev`; the remote is not to be touched without an
   explicit instruction.
6. **Never add SAS-owned data or third-party text.** SASHELP members may be
   exported into `inst/sashelp/` for local testing, but that directory is
   git-ignored and must stay out of the repository. Write your own programs
   with invented `datalines` instead of copying documentation examples.

## Environment (this machine)

| Tool | Path / fact |
|---|---|
| R | `D:\R\R-4.4.2\bin\Rscript.exe`, Rtools44 available (`pkgbuild::has_build_tools()` is TRUE) |
| Python | `D:\Python\313` |
| Local SAS | SAS 9.4 **TS1M2 DBCS3170**, `sas.exe` under `HKLM\SOFTWARE\SAS Institute Inc.\Common Data\Shared Files` → `SASFoundation\9.4\`. English locale via `-config "...\nls\en\sasv9.cfg"` (the Chinese default gives EUC-CN and Chinese log text) |
| Conformance repository | `E:\dsh\sasonr-conformance`, branch `ts1m2` (private; gold generated on TS1M2) |
| Local SAS helper | `.local/run-case.ps1` (git-ignored): runs a program under local SAS with the English config and dumps every WORK data set |

The conformance repository stores `cases/own/**` with **TS1M7** provenance and
`cases/ts1m2/**` with **TS1M2** gold produced on this machine. Local SAS is
TS1M2, so it can only produce the latter. Never overwrite `cases/own`; new
local gold goes under `cases/ts1m2` with the same case id.

## Commands

```powershell
# load and test (one R process at a time)
& 'D:\R\R-4.4.2\bin\Rscript.exe' -e "pkgload::load_all('E:/dsh/sasonr'); testthat::test_dir('E:/dsh/sasonr/tests/testthat')"

# single test file
& 'D:\R\R-4.4.2\bin\Rscript.exe' -e "pkgload::load_all('E:/dsh/sasonr'); testthat::test_file('E:/dsh/sasonr/tests/testthat/test-proc.R')"

# run one conformance case (harness is in tests/testthat/helper-conformance.R)
& 'D:\R\R-4.4.2\bin\Rscript.exe' -e "pkgload::load_all('E:/dsh/sasonr'); e <- new.env(); sys.source('E:/dsh/sasonr/tests/testthat/helper-conformance.R', e); print(e\$conf_run_case('E:/dsh/sasonr-conformance/cases/own/semantics/sort_collation'))"

# run a program under local SAS (English locale, WORK data sets dumped)
& powershell.exe -NoProfile -ExecutionPolicy Bypass -File 'E:\dsh\sasonr\.local\run-case.ps1' `
    -Program 'E:\dsh\_dsh_tmp\probe.sas' -RunDir 'E:\dsh\_dsh_tmp\probe_run' `
    -Epilogue 'E:\dsh\sasonr\tools\sas\dump_work_sas7.sas'

# rebuild the local SASHELP store (needs an export from SAS first)
& 'D:\R\R-4.4.2\bin\Rscript.exe' 'E:\dsh\sasonr\tools\build_sashelp.R' 'inst/sashelp'
```

Set `OPENBLAS_NUM_THREADS=1` when running anything that touches linear algebra
so a stray threaded BLAS cannot perturb last-bit results.

## Layout

| Path | Contents |
|---|---|
| `R/` | engine: `lex.R`, `parse_*.R`, `analyze.R`, `run.R`, `functions.R`, `formats.R`, `proc_*.R`, `sql_*.R` |
| `inst/registry/` | `functions.csv` and `formats.csv` are **generated** by `tools/update_registry.R`; `semantics.csv` is hand-maintained |
| `tests/testthat/` | unit tests plus `helper-conformance.R` (case runner: ds/attr/log/lst/put/files layers) |
| `tools/` | case generators, registry builder, `sanitize.py` (removes site/licence text from SAS output), `sas/dump_work_sas7.sas` |
| `vignettes/articles/` | developer articles, including the semantics and feature registries |
| `.local/` | local-only SAS helpers and scratch runs (git-ignored) |

## Strict mode and the registries

`sasonr.strict` (default `TRUE`) makes a program stop when it uses a function,
format or informat whose registry status is `approximate`, `unverified` or
`mismatch`. `sas_feature_registry(kind)` exposes the status from the CSVs; the
matching for character formats uses the `$NAME` spelling (`$HEX`).

Adding a feature therefore takes three things: the implementation, a passing
conformance case, and a registry row reflecting that case. `functions.csv` and
`formats.csv` are regenerated from the matrix assets with
`tools/update_registry.R`; if the assets are unavailable, hand-adding a row is
acceptable **only** when a real SAS-verified case backs it — say so in the
`note` column.

## Adding a conformance case

1. Write `program.sas` with invented data and an oracle that prints the values
   (`put ... hex16.` for exact bits).
2. Run it under SAS with `tools/sas/run_cases.sh` (or `.local/run-case.ps1`).
3. Sanitise the log output with `tools/sanitize.py` — SAS logs name the site
   and licence and must never be committed raw.
4. Store `program.sas`, `meta.yaml` (`sas_version`, `program_sha256`,
   `encoding: latin1`) and `expected/` (datasets, `attrs.csv`, `log_notes.txt`,
   `put.txt`, `files/`).
5. Compare bit for bit: data sets, column attributes, log notes, listing,
   `put` text and written files. There is no tolerance.

`program_sha256` is taken over the program bytes as stored. Set
`git config core.autocrlf false` before checking cases out — with autocrlf on,
every hash mismatches and a refresh run would silently re-gold every case.

## Working with other agents

* One R process at a time; do not run tests while another agent is mid-edit in
  `R/`, and do not edit a file another agent owns without saying so.
* Measurement beats reasoning: when the SAS rule is unclear, run the local SAS
  over a generated grid (vary `LINESIZE`, option values, data shapes) and parse
  the output mechanically rather than inferring the rule from one example.
* Report numbers, not intentions: "65/84 pass, was 43" is useful; "should work
  now" is not.
* If a change cannot be finished, keep the verified part and leave the
  unverified part failing loud — never ship a half-correct behaviour, and never
  roll back a commit that already carries verified work.
