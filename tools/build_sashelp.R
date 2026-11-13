# Build the local SASHELP store used at run time (optional: sasonr also reads
# <member>.sas7bdat + <member>_meta.csv directly, R/sashelp.R).
#
# The SAS data sets are exported from the local SAS installation into
# inst/sashelp/ (that directory is in .gitignore: the members belong to the SAS
# installation and are not redistributed).  This script turns each member plus
# its <member>_meta.csv into an RDS file that keeps what SAS programs observe:
# the declared character lengths (SAS pads to them, R does not) and the column
# formats.
#
# Usage (after exporting with SAS):
#   Rscript tools/build_sashelp.R [inst/sashelp]
#
# The export side is two SAS statements:
#   libname __out "<dir>"; proc copy in=sashelp out=__out memtype=data; select <member>; run;
#   proc sql; create table work.meta as select memname, varnum, name, type,
#     length, format, informat, label from dictionary.columns
#     where libname='SASHELP' and memname='<MEMBER>'; quit;
#   proc export data=work.meta outfile="<dir>/<member>_meta.csv" dbms=csv replace; run;

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1] else file.path("inst", "sashelp")
suppressMessages(devtools::load_all(".", quiet = TRUE, helpers = FALSE))
read_member <- get("sashelp_read_sas7bdat", asNamespace("sasonr"))

for (ds in list.files(dir, pattern = "\\.sas7bdat$", full.names = TRUE)) {
  member <- tolower(sub("\\.sas7bdat$", "", basename(ds)))
  meta_file <- file.path(dir, paste0(member, "_meta.csv"))
  # without the column attributes the declared lengths are unknown: skip
  if (!file.exists(meta_file)) {
    cat(sprintf("%-14s skipped (no %s)\n", member, basename(meta_file)))
    next
  }
  d <- read_member(ds, meta_file, member)
  out <- file.path(dir, paste0(member, ".rds"))
  saveRDS(d, out, version = 2)
  cat(sprintf("%-14s %4d rows  -> %s\n", member, nrow(d), basename(out)))
}
