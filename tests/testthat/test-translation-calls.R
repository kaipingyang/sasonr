# Translated code calls only exported sasonr helpers, functions of the base
# packages and functions with a package prefix (dplyr::, tibble::).

called_names <- function(code) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      f <- e[[1]]
      if (is.call(f) && identical(f[[1]], as.name("::"))) {
        out <<- c(out, paste0(as.character(f[[2]]), "::", as.character(f[[3]])))
      } else if (is.name(f)) {
        out <<- c(out, as.character(f))
      }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
      if (!is.name(f)) walk(f)
    } else if (is.function(e)) {
      walk(body(e))
    }
  }
  for (x in as.list(parse(text = code, keep.source = FALSE))) walk(x)
  unique(out)
}

test_that("translated steps call only exported, base or package-prefixed functions", {
  prog <- "
    data a; input g $ x y; datalines;
  a 1 2
  a 3 4
  b 5 6
  ;
    run;
    data b; set a; by g; retain s 0; if first.g then s = 0; s + x; z = put(x, 8.2); if last.g; run;
    proc sort data=a out=c; by descending x; run;
    proc means data=a; class g; var x y; run;
    proc freq data=a; tables g; run;
    proc contents data=a; run;
    proc print data=a; run;
    proc transpose data=a out=t; var x y; run;
    proc format; value lo low-2 = 'low' other = 'high'; run;
    proc sql; create table d as select g, sum(x) as sx from a group by g; quit;
    proc datasets lib=work nolist; delete c; quit;
  "
  tr <- sas_translate(prog)
  exports <- getNamespaceExports("sasonr")
  base_pkgs <- c("base", "stats", "utils", "methods", "graphics", "grDevices")
  base_fns <- unlist(lapply(base_pkgs, function(p) ls(asNamespace(p), all.names = TRUE)))
  for (st in tr$steps) {
    nm <- called_names(st$code)
    bad <- nm[!grepl("::", nm, fixed = TRUE) & !nm %in% c(exports, base_fns)]
    # names bound in the code itself (local helper functions)
    bound <- unique(unlist(regmatches(st$code, gregexpr("[.A-Za-z_][.A-Za-z0-9_]*(?=\\s*<-\\s*function)", st$code, perl = TRUE))))
    expect_identical(setdiff(bad, bound), character(), info = st$code)
  }
  levels <- vapply(tr$steps, `[[`, "", "level")
  expect_true(all(c("SQL", "PROC") %in% levels))
})

test_that("steps without an R translation fail loud when their code is evaluated", {
  tr <- sas_translate("data a; x = 1; run; proc sql; create table b as select * from a; quit;
                       proc datasets lib=work nolist; delete b; quit;")
  stub <- Filter(function(s) grepl("sas_rt_untranslated", s$code, fixed = TRUE), tr$steps)
  expect_length(stub, 2L)
  for (s in stub) {
    expect_error(eval(parse(text = s$code), envir = new.env(parent = globalenv())), class = "sasonr_unsupported")
  }
})
