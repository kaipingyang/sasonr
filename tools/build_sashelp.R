# Build the local SASHELP store used at run time.
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
if (!requireNamespace("haven", quietly = TRUE)) stop("build_sashelp needs haven")

read_meta <- function(path) {
  m <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  m$length <- as.integer(m$length)
  m
}

for (ds in list.files(dir, pattern = "\\.sas7bdat$", full.names = TRUE)) {
  member <- tolower(sub("\\.sas7bdat$", "", basename(ds)))
  meta_file <- file.path(dir, paste0(member, "_meta.csv"))
  d <- haven::read_sas(ds)
  names(d) <- tolower(names(d)) # SAS names are case insensitive
  if (file.exists(meta_file)) {
    m <- read_meta(meta_file)
    m <- m[tolower(m$name) %in% tolower(names(d)), , drop = FALSE]
    # SAS pads character values to the declared length; restore the padding so
    # that LENGTH()/SUBSTR()/comparisons behave as in SAS
    for (i in seq_len(nrow(m))) {
      v <- tolower(m$name[i])
      if (m$type[i] != "char") next
      x <- d[[v]]
      pad <- pmax(0L, m$length[i] - nchar(x))
      d[[v]] <- ifelse(is.na(x), NA_character_,
                       paste0(x, vapply(pad, function(k) strrep(" ", k), "")))
    }
    names(d) <- tolower(names(d))
    attr(d, "sashelp_meta") <- m
  }
  out <- file.path(dir, paste0(member, ".rds"))
  saveRDS(d, out, version = 2)
  cat(sprintf("%-14s %4d rows  -> %s\n", member, nrow(d), basename(out)))
}
