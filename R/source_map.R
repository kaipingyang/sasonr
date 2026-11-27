# The source map of a submission (spec 09 L0): %INCLUDE files inlined into the
# program text, each line with the level it comes from (0: the program).
#
# An included file is read as if its lines followed the %INCLUDE statement:
# they take the next line numbers and the program's later lines are numbered
# after them; under NOSOURCE2 (the default) they are not echoed (measured).  Supported: `%INCLUDE 'path';` (or "path") alone on
# its line, in open code or in a step, with a file that exists, nested in
# included files.  Fail loud: %INCLUDE inside a macro definition or with
# %IF/%DO in open code (the macro processor decides there whether it runs),
# filerefs, several files or options, a missing file, OPTIONS SOURCE2 (the
# echo of the included lines).

src_include_expand <- function(text, level = 0L) {
  if (!grepl("%inc", text, ignore.case = TRUE)) return(list(text = text, run = text, level = rep(level, src_n_lines(text))))
  tok <- tryCatch(sas_lex(text, lossless = TRUE), error = function(e) NULL)
  if (is.null(tok)) abort_unsupported("%INCLUDE in a program whose tokens could not be read before macro processing")
  low <- tolower(tok$value)
  macro <- tok$kind == "macro"
  inc <- which(macro & low %in% c("%include", "%inc"))
  if (!length(inc)) return(list(text = text, run = text, level = rep(level, src_n_lines(text))))
  if (any(macro & low %in% c("%macro", "%if", "%do"))) {
    abort_unsupported("%INCLUDE in a program with macro definitions or open-code %IF/%DO", tok$line[inc[1]])
  }
  if (any(tok$kind == "name" & low == "source2")) abort_unsupported("%INCLUDE under OPTIONS SOURCE2", tok$line[inc[1]])
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (endsWith(text, "\n")) lines <- c(lines, "")
  out <- list()
  lev <- list()
  incl <- integer()
  done <- 0L
  for (k in inc) {
    ln <- tok$line[k]
    on_line <- tok[tok$line == ln & !tok$kind %in% c("ws", "newline"), ]
    if (nrow(on_line) != 3L || !identical(on_line$id, tok$id[k] + c(0L, 2L, 3L)) || on_line$kind[2] != "str" ||
        on_line$kind[3] != "semi") {
      abort_unsupported("a %INCLUDE statement other than %INCLUDE 'path'; alone on its line", ln)
    }
    path <- on_line$value[2]
    if (!file.exists(path)) abort_unsupported(paste0("%INCLUDE of a file that does not exist (", path, ")"), ln)
    sub <- src_include_expand(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), level + 1L)
    out <- c(out, list(lines[(done + 1L):ln]), list(strsplit(sub$run, "\n", fixed = TRUE)[[1]]))
    lev <- c(lev, list(rep(level, ln - done)), list(sub$level))
    incl <- c(incl, sum(lengths(out)) - length(sub$level))
    done <- ln
  }
  if (done < length(lines)) {
    out <- c(out, list(lines[(done + 1L):length(lines)]))
    lev <- c(lev, list(rep(level, length(lines) - done)))
  }
  lines <- unlist(out)
  # the text the macro processor reads: the %INCLUDE lines blank (the echo
  # keeps them, `text`)
  run <- lines
  run[incl] <- ""
  list(text = paste(lines, collapse = "\n"), run = paste(run, collapse = "\n"), level = unlist(lev))
}

src_n_lines <- function(text) lengths(regmatches(text, gregexpr("\n", text, fixed = TRUE))) + 1L
