# Read-only libraries (LIBNAME, spec 01 P0-9). A member `ref.member` of a
# library is read when a step names it: the values with haven (Suggests), the
# attributes of its variables (type, length, format, informat, label) from the
# file -- the NAMESTR records of a transport (XPORT) file (xpt_meta()); a
# .sas7bdat member fails loud until its column attributes are read. The members stay in the
# session (`lib_members`), not in WORK; a step's analysis sees their metadata
# (lib_step_meta()) and its code reads them with sas_rt_ds() (sas_rt_lib_member()).
# What the file does not give fails loud: a .sas7bdat member, a transport
# file with several members, a member that is not there (an ERROR in the
# documented behaviour), writing to a library.

# The data set references of a parsed step that name a library member.
lib_step_refs <- function(x) {
  if (!is.list(x)) return(character())
  out <- if (is.character(x$libref) && length(x$libref) == 1L && is.character(x$member)) paste0(x$libref, ".", x$member)
  unique(c(out, unlist(lapply(x, lib_step_refs), use.names = FALSE)))
}

# The metadata a step is analysed with: WORK's and that of the library members
# it reads (read now if needed).
lib_step_meta <- function(session, step) {
  refs <- lib_step_refs(step)
  if (!length(refs)) return(session$meta)
  meta <- session$meta
  for (r in refs) meta[[r]] <- lib_member(session, r, step$line)$meta
  meta
}

lib_drop_members <- function(session, ref) {
  if (!length(session$lib_members)) return(invisible())
  keep <- !startsWith(names(session$lib_members), paste0(ref, "."))
  session$lib_members <- session$lib_members[keep]
  invisible()
}

# A library member: list(data, meta), read once per assignment of its libref.
lib_member <- function(session, key, line = NULL) {
  got <- session$lib_members[[key]]
  if (!is.null(got)) return(got)
  ref <- sub("[.].*$", "", key)
  mem <- sub("^[^.]*[.]", "", key)
  lib <- session$libs[[ref]]
  if (is.null(lib)) abort_unsupported(paste0("library ", toupper(ref), " (no LIBNAME assigns it)"), line)
  if (!requireNamespace("haven", quietly = TRUE)) abort_unsupported(paste0(toupper(key), " (reading a library needs the haven package)"), line)
  got <- if (lib$engine == "XPORT") lib_read_xpt(lib$path, ref, mem, line) else lib_read_sas7bdat(lib$path, ref, mem, line)
  session$lib_members[[key]] <- got
  got
}

lib_missing_member <- function(ref, mem, line) {
  abort_unsupported(paste0("File ", toupper(ref), ".", toupper(mem), ".DATA does not exist (an ERROR)"), line)
}

lib_read_xpt <- function(path, ref, mem, line) {
  m <- xpt_meta(path)
  names <- vapply(m$members, function(x) tolower(x$name), "")
  k <- match(mem, names)
  if (is.na(k)) lib_missing_member(ref, mem, line)
  if (length(names) > 1L) abort_unsupported(paste0("a transport file with several members (", toupper(ref), ")"), line)
  v <- m$members[[k]]$vars
  d <- as.data.frame(haven::read_xpt(path))
  # special missing values (.A-.Z, ._) arrive as tagged NA
  for (j in seq_along(d)) {
    x <- d[[j]]
    if (!is.double(x)) next
    tag <- haven::na_tag(x)
    if (any(!is.na(tag))) {
      at <- which(!is.na(tag))
      x[at] <- vapply(tag[at], function(t) sas_missing(paste0(".", t)), 1)
      d[[j]] <- x
    }
  }
  lib_finish(d, v$name, v$type, v$len, v$format, v$informat, v$label, m$members[[k]]$label, ref, mem, line)
}

lib_read_sas7bdat <- function(path, ref, mem, line) {
  f <- file.path(path, paste0(mem, ".sas7bdat"))
  if (!file.exists(f)) lib_missing_member(ref, mem, line)
  # haven gives the values but not the declared character lengths or the
  # numeric storage lengths, which the step's attributes depend on
  abort_unsupported(paste0(toupper(ref), ".", toupper(mem), " (the variable lengths of a .sas7bdat member are not read yet)"), line)
}

# The data frame (lower-case names, character values padded to their length)
# and its metadata in the form of WORK's (out_meta()).
lib_finish <- function(df, vars, type, len, fmt, informat, label, memlabel, ref, mem, line) {
  df[] <- lapply(df, function(x) {
    attributes(x) <- NULL
    x
  })
  disp <- vars
  vars <- tolower(vars)
  names(df) <- tolower(names(df))
  if (!identical(names(df), vars)) abort_unsupported(paste0("the variables of ", toupper(ref), ".", toupper(mem), " (the file's attributes do not match its columns)"), line)
  for (j in seq_along(vars)) {
    if (type[j] == "char") {
      x <- df[[j]]
      if (any(nchar(x[!is.na(x)], "bytes") > len[j])) abort_unsupported(paste0("values of ", disp[j], " longer than its length"), line)
      df[[j]] <- sas_rt_char(ifelse(is.na(x), "", x), as.integer(len[j]))
    } else if (len[j] != 8L) {
      # a shorter numeric length: the stored precision is not modelled here
      abort_unsupported(paste0("the numeric length ", len[j], " of ", disp[j], " in ", toupper(ref), ".", toupper(mem)), line)
    }
  }
  pf <- function(t) if (nzchar(t)) tryCatch(mac_parse_format(t), error = function(e) abort_unsupported(paste0("the format ", t, " of a library member"), line))
  fmts <- stats::setNames(lapply(fmt, pf), vars)
  infs <- stats::setNames(lapply(informat, pf), vars)
  labs <- stats::setNames(lapply(label, function(l) if (nzchar(l)) l), vars)
  meta <- list(vars = vars, type = stats::setNames(as.list(type), vars), len = stats::setNames(as.list(as.integer(ifelse(type == "char", len, NA))), vars),
               formats = Filter(Negate(is.null), fmts), disp = stats::setNames(as.list(disp), vars),
               labels = Filter(Negate(is.null), labs), informats = Filter(Negate(is.null), infs))
  if (nzchar(memlabel)) meta$memlabel <- memlabel
  list(data = tibble::as_tibble(df), meta = meta)
}
