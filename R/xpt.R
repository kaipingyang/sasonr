# SAS transport files (XPORT, version 5): the members and the attributes of
# their variables, read from the file's header records (the values are read
# with haven). Layout of the format (documented by SAS, "The Record Layout of
# a Data Set in SAS Transport (XPORT) Format"): 80-byte records; a library
# header, then per member a member header, a descriptor header and two
# records (member name in bytes 9-16 of the first, label in 33-72 of the
# second), the NAMESTR header
# (number of variables in bytes 55-58) and one 140-byte NAMESTR per variable,
# then the OBS header and the observations.

xpt_header_re <- "^HEADER RECORD\\*{7}([A-Z ]{8})HEADER RECORD!{7}"

# list(members = list(list(name, label, vars = data.frame(name, type, len,
# label, format, informat, varnum))))
xpt_meta <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  raw <- readBin(con, "raw", file.info(path)$size)
  rec <- function(i) {
    b <- raw[(i - 1L) * 80L + seq_len(80L)]
    b[b == as.raw(0)] <- as.raw(32)
    iconv(rawToChar(b), "latin1", "UTF-8")
  }
  n_rec <- length(raw) %/% 80L
  if (n_rec < 3L || !grepl("LIBRARY", rec(1L), fixed = TRUE)) stop("not a transport (XPORT) file: ", path)
  if (grepl("LIBV8", rec(1L), fixed = TRUE)) stop("a version 8 transport file (LIBV8): ", path)
  be_int <- function(b) {
    v <- sum(as.integer(b) * 256^rev(seq_along(b) - 1L))
    if (length(b) == 2L && v >= 32768) v - 65536 else v
  }
  txt <- function(b) sub(" +$", "", iconv(rawToChar(b[b != as.raw(0)]), "latin1", "UTF-8"))
  members <- list()
  i <- 1L
  while (i <= n_rec) {
    r <- rec(i)
    m <- regmatches(r, regexec(xpt_header_re, r))[[1]]
    if (length(m) && m[2] == "MEMBER  ") {
      # (record i + 1 is the DSCRPTR header)
      d1 <- raw[(i + 1L) * 80L + seq_len(80L)]
      d2 <- raw[(i + 2L) * 80L + seq_len(80L)]
      name <- txt(d1[9:16])
      label <- txt(d2[33:72])
      if (!identical(substr(r, 76L, 78L), "140")) stop("a NAMESTR length other than 140 in member header: ", path)
      nh <- rec(i + 4L)
      if (!grepl("NAMESTR", nh, fixed = TRUE)) stop("no NAMESTR header after member ", name)
      nvar <- as.integer(substr(nh, 55L, 58L))
      start <- (i + 4L) * 80L
      vars <- lapply(seq_len(nvar), function(k) {
        b <- raw[start + (k - 1L) * 140L + seq_len(140L)]
        fmt <- txt(b[57:64])
        fl <- be_int(b[65:66])
        fd <- be_int(b[67:68])
        ifmt <- txt(b[73:80])
        ifl <- be_int(b[81:82])
        ifd <- be_int(b[83:84])
        fmt_text <- function(n, w, d) if (!nzchar(n) && w == 0) "" else paste0(n, if (w > 0) w, ".", if (d > 0) d)
        data.frame(name = txt(b[9:16]), type = if (be_int(b[1:2]) == 2) "char" else "num", len = be_int(b[5:6]),
                   varnum = be_int(b[7:8]), label = txt(b[17:56]), format = fmt_text(fmt, fl, fd),
                   informat = fmt_text(ifmt, ifl, ifd), stringsAsFactors = FALSE)
      })
      members[[length(members) + 1L]] <- list(name = name, label = label, vars = do.call(rbind, vars))
      i <- (start + nvar * 140L) %/% 80L + 1L
      next
    }
    i <- i + 1L
  }
  list(members = members)
}
