# SASHELP data sets.
#
# sasonr does not ship SASHELP members.  They are read from the directory set
# with options(sasonr.sashelp_dir = DIR) or the environment variable
# SASONR_SASHELP_DIR (the option wins); without one, every SASHELP member is
# missing.  A member is either
#   <member>.rds          a data frame that keeps the declared character
#                         lengths and the column formats, or
#   <member>.sas7bdat     the data set copied from SASHELP by PROC COPY, read
#                         with haven (Suggests), together with
#   <member>_meta.csv     its DICTIONARY.COLUMNS rows (name, type, length,
#                         format, informat, label), which give the declared
#                         character lengths that values are padded to.
# When a member, its column attributes or haven are missing, the program stops
# loudly instead of guessing.

sashelp_cache <- new.env(parent = emptyenv())

sashelp_dir <- function() {
  d <- getOption("sasonr.sashelp_dir", Sys.getenv("SASONR_SASHELP_DIR"))
  if (length(d) == 1L && !is.na(d) && nzchar(d)) d else ""
}

# The files of a member in the local store (NULL when it is not there).
sashelp_files <- function(name) {
  dir <- sashelp_dir()
  if (!nzchar(dir) || !dir.exists(dir)) return(NULL)
  key <- tolower(name)
  rds <- file.path(dir, paste0(key, ".rds"))
  if (file.exists(rds)) return(list(rds = rds))
  ds <- file.path(dir, paste0(key, ".sas7bdat"))
  if (file.exists(ds)) return(list(sas7bdat = ds, meta = file.path(dir, paste0(key, "_meta.csv"))))
  NULL
}

sashelp_has <- function(name) !is.null(sashelp_files(name))

sashelp_member <- function(name, line = NULL) {
  key <- tolower(name)
  cached <- sashelp_cache[[key]]
  if (!is.null(cached)) return(cached)
  f <- sashelp_files(name)
  if (is.null(f)) {
    abort_unsupported(paste0("SASHELP.", toupper(name), " (not in the SASHELP directory: set ",
                             "options(sasonr.sashelp_dir) or SASONR_SASHELP_DIR)"), line)
  }
  d <- if (!is.null(f$rds)) readRDS(f$rds) else sashelp_read_sas7bdat(f$sas7bdat, f$meta, name, line)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  # SAS names are case insensitive; sasonr works with the lower-case form
  names(d) <- tolower(names(d))
  sashelp_cache[[key]] <- d
  d
}

# A SASHELP data set copied by PROC COPY, with the declared character lengths
# of its DICTIONARY.COLUMNS rows (character values are padded to them, which
# LENGTHC()/SUBSTR()/comparisons depend on).
sashelp_read_sas7bdat <- function(path, meta_path, name = basename(path), line = NULL) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    abort_unsupported(paste0("SASHELP.", toupper(name), " (reading the local .sas7bdat copy needs the haven package)"), line)
  }
  if (!file.exists(meta_path)) {
    abort_unsupported(paste0("SASHELP.", toupper(name), " (the column attributes ", basename(meta_path),
                             " are missing next to the .sas7bdat copy; the declared lengths are not known)"), line)
  }
  m <- utils::read.csv(meta_path, stringsAsFactors = FALSE, colClasses = "character")
  m$length <- as.integer(m$length)
  d <- as.data.frame(haven::read_sas(path))
  d[] <- lapply(d, function(x) {
    attributes(x) <- NULL
    x
  })
  names(d) <- tolower(names(d))
  if (!setequal(tolower(m$name), names(d))) {
    abort_unsupported(paste0("SASHELP.", toupper(name), " (", basename(meta_path), " does not list the columns of the data set)"), line)
  }
  for (i in seq_len(nrow(m))) {
    if (m$type[i] != "char") next
    v <- tolower(m$name[i])
    x <- d[[v]]
    if (any(nchar(x[!is.na(x)]) > m$length[i])) {
      abort_unsupported(paste0("SASHELP.", toupper(name), " (values of ", m$name[i], " longer than its declared length)"), line)
    }
    pad <- pmax(0L, m$length[i] - nchar(x))
    d[[v]] <- ifelse(is.na(x), NA_character_, paste0(x, strrep(" ", pad)))
  }
  attr(d, "sashelp_meta") <- m
  d
}
