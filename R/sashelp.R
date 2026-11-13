# SASHELP data sets used by conformance cases.
#
# The members are exported from the local SAS 9.4 installation into
# inst/sashelp/ (that directory is in .gitignore: the data belong to the SAS
# installation and are not redistributed).  Each member is stored as an RDS
# file so that the package needs no reader dependency at run time.
#
# When a member is missing, the program stops loudly instead of guessing.

sashelp_cache <- new.env(parent = emptyenv())

sashelp_dir <- function() {
  d <- system.file("sashelp", package = "sasonr")
  if (!nzchar(d)) d <- file.path("inst", "sashelp")
  d
}

sashelp_member <- function(name, line = NULL) {
  key <- tolower(name)
  cached <- sashelp_cache[[key]]
  if (!is.null(cached)) return(cached)
  path <- file.path(sashelp_dir(), paste0(key, ".rds"))
  if (!file.exists(path)) {
    abort_unsupported(
      paste0("SASHELP.", toupper(name), " (export it from SAS into inst/sashelp/)"), line)
  }
  d <- readRDS(path)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  # SAS names are case insensitive; sasonr works with the lower-case form
  names(d) <- tolower(names(d))
  sashelp_cache[[key]] <- d
  d
}
