# PROC TABULATE: parser, table IR and the checks of a step (no computation yet).

tab_step <- function(code) sas_parse(code)$steps[[1]]

tab_irs <- function(code) {
  step <- tab_step(code)
  decl <- sasonr:::tab_declared(step)
  tables <- Filter(function(s) identical(s$stmt, "table"), step$stmts)
  lapply(tables, sasonr:::tab_table_ir, decl = decl)
}

# A tree as text: name[kind](children), siblings blank-separated.
tree_text <- function(tree) {
  if (is.null(tree)) return(NULL)
  paste(vapply(tree, function(n) {
    s <- paste0(n$name, "[", n$kind, "]")
    if (!is.na(n$label)) s <- paste0(s, "='", n$label, "'")
    if (!is.null(n$format)) s <- paste0(s, "{", n$format$text, "}")
    if (length(n$children)) s <- paste0(s, "(", tree_text(n$children), ")")
    s
  }, ""), collapse = " ")
}

ir_text <- function(ir) lapply(ir$dims, tree_text)

have <- "
data have;
  length grp $1 sub $1;
  input grp $ sub $ trt v w;
  datalines;
a x 1 10 1.5
b y 2 . 2.5
. y 1 90 .
;
run;
"
kw <- "
data kw;
  length all $1 n $1;
  input all $ n $ mean;
  datalines;
p m 1.5
q f 2.5
;
run;
"

run_log <- function(code) {
  r <- sas_run(code, on_error = "sas")
  lines <- as.character(sas_log(r))
  lines[grepl("^(ERROR|WARNING|NOTE: sasonr stopped)", lines)]
}

test_that("the TABLE statement gives one tree per dimension", {
  ir <- tab_irs("proc tabulate data=a; class g; var v; table v*mean;")[[1]]
  expect_null(ir$dims$page)
  expect_null(ir$dims$row)
  expect_identical(tree_text(ir$dims$col), "v[var](mean[stat])")
  ir <- tab_irs("proc tabulate data=a; class g h; var v; table g, h, v*n;")[[1]]
  expect_identical(ir_text(ir), list(page = "g[class]", row = "h[class]", col = "v[var](n[stat])"))
})

test_that("crossing and concatenation follow the grammar", {
  ir <- tab_irs("proc tabulate data=a; class g h; var v w; table (g h)*v, w*(n mean) all;")[[1]]
  # (A B)*C distributes, A*(B C) keeps one node with two children
  expect_identical(tree_text(ir$dims$row), "g[class](v[var]) h[class](v[var])")
  expect_identical(tree_text(ir$dims$col), "w[var](n[stat] mean[stat]) all[all]")
  # equal siblings stay apart
  ir <- tab_irs("proc tabulate data=a; class grp; var v w; table grp, v*sum v*mean (v w)*n;")[[1]]
  expect_identical(tree_text(ir$dims$col), "v[var](sum[stat]) v[var](mean[stat]) v[var](n[stat]) w[var](n[stat])")
})

test_that("labels and F= are kept on the IR", {
  ir <- tab_irs("proc tabulate data=a; class grp; var v; table grp='' all='Total', v=''*mean='' / box='B' misstext='none';")[[1]]
  expect_identical(tree_text(ir$dims$row), "grp[class]='' all[all]='Total'")
  expect_identical(tree_text(ir$dims$col), "v[var]=''(mean[stat]='')")
  expect_identical(ir$box, list(kind = "text", value = "B"))
  expect_identical(ir$misstext, "none")
  ir <- tab_irs("proc tabulate data=a; class grp; var v; table grp*f=5.1, v*mean*f=8.3;")[[1]]
  expect_identical(tree_text(ir$dims$row), "grp[class]{5.1}")
  expect_identical(tree_text(ir$dims$col), "v[var](mean[stat]{8.3})")
  # an inner F= wins over an outer one
  ir <- tab_irs("proc tabulate data=a; class g; var p l; table g, mean*(p*format=dollar7.2 l*format=2.0)*f=6.1;")[[1]]
  expect_identical(tree_text(ir$dims$col), "mean[stat](p[var]{dollar7.2} l[var]{2.0})")
})

test_that("typical TABLE expressions give the expected IR", {
  ir <- tab_irs("PROC TABULATE DATA = giant; CLASS Color; VAR Days Weight; TABLE Color ALL, (Days Weight) * MEAN; RUN;")[[1]]
  expect_identical(ir_text(ir), list(page = NULL, row = "color[class] all[all]",
                                     col = "days[var](mean[stat]) weight[var](mean[stat])"))
  ir <- tab_irs("PROC TABULATE DATA = boats; CLASS Port Locomotion Type; TABLE Port, Locomotion, Type; RUN;")[[1]]
  expect_identical(ir_text(ir), list(page = "port[class]", row = "locomotion[class]", col = "type[class]"))
  ir <- tab_irs("PROC TABULATE DATA = boats FORMAT=DOLLAR9.2; CLASS Locomotion Type; VAR Price;
    TABLE Locomotion ALL, MEAN*Price*(Type ALL) /BOX='Full Day Excursions' MISSTEXT='none'; RUN;")[[1]]
  expect_identical(tree_text(ir$dims$col), "mean[stat](price[var](type[class] all[all]))")
  expect_identical(ir$box$value, "Full Day Excursions")
  ir <- tab_irs("PROC TABULATE DATA = boats; CLASS Locomotion Type; VAR Price Length;
    TABLE Locomotion ALL, MEAN * (Price*FORMAT=DOLLAR7.2 Length*FORMAT=2.0) * (Type ALL); RUN;")[[1]]
  expect_identical(tree_text(ir$dims$col),
                   "mean[stat](price[var](type[class]{dollar7.2} all[all]{dollar7.2}) length[var](type[class]{2.0} all[all]{2.0}))")
  ir <- tab_irs("PROC TABULATE DATA = skating; CLASS Year Record; TABLE Year = '',Record*N = ''; RUN;")[[1]]
  expect_identical(ir_text(ir), list(page = NULL, row = "year[class]=''", col = "record[class](n[stat]='')"))
  ir <- tab_irs("proc tabulate data = adsl missing; class trtpn sexn racen; var age;
    table age = 'Age' * (n = 'n' * f = 8. mean = 'Mean' * f = 5.1)
          sexn = 'Sex' * (n = 'n' * f = 3.),
    (trtpn = '  ') (all = 'Overall'); run;")[[1]]
  expect_identical(tree_text(ir$dims$row), "age[var]='Age'(n[stat]='n'{8.} mean[stat]='Mean'{5.1}) sexn[class]='Sex'(n[stat]='n'{3.})")
  expect_identical(tree_text(ir$dims$col), "trtpn[class]='  ' all[all]='Overall'")
})

test_that("a CLASS or VAR variable named like a keyword is the variable, in any case", {
  decl <- sasonr:::tab_declared(tab_step("proc tabulate data=kw; class all n; var mean; table ALL N, MEAN*Mean;"))
  ir <- sasonr:::tab_table_ir(Filter(function(s) identical(s$stmt, "table"), tab_step(
    "proc tabulate data=kw; class all n; var mean; table ALL N, MEAN*Mean;")$stmts)[[1]], decl)
  expect_identical(ir_text(ir), list(page = NULL, row = "all[class] n[class]", col = "mean[var](mean[var])"))
  ir <- tab_irs("proc tabulate data=a; class grp; var v; table ALL, v*sum;")[[1]]
  expect_identical(tree_text(ir$dims$row), "all[all]")
})

test_that("the cell ERRORs are written with their nesting path and the step stops", {
  base <- paste0(have, "proc tabulate data=have; class grp; ")
  expect_identical(run_log(paste0(base, "var v trt; table grp, v*trt; run;")), c(
    "ERROR: There are multiple analysis variables associated with a single table cell in the following nesting : grp * v * trt.",
    "NOTE: sasonr stopped processing this step because of errors."))
  expect_identical(run_log(paste0(base, "var v; table grp, v*mean*sum; run;"))[1],
    "ERROR: There are multiple statistics associated with a single table cell in the following nesting : grp * v * Mean * Sum.")
  expect_identical(run_log(paste0(base, "var v; table grp*mean, v*sum; run;"))[1],
    "ERROR: There are multiple statistics associated with a single table cell in the following nesting : grp * Mean * v * Sum.")
  expect_identical(run_log(paste0(base, "table grp, mean; run;"))[1],
    "ERROR: Statistic other than N was requested without analysis variable in the following nesting : grp * Mean.")
})

test_that("the ERROR follows the source of the step and a blank line", {
  r <- sas_run(paste0(have, "proc tabulate data=have; class grp; var v trt; table grp, v*trt; run;"), on_error = "sas")
  lines <- as.character(sas_log(r))
  k <- grep("^ERROR", lines)
  expect_identical(lines[k - 1L], "")
  expect_match(lines[k - 2L], "run;$")
})

test_that("keyword-named variables give one WARNING each before the ERRORs", {
  expect_identical(run_log(paste0(kw, "proc tabulate data=kw; class all n; var mean; table all n, mean*mean; run;")), c(
    "WARNING: The variable all conflicts with the keyword of the same name.",
    "WARNING: The variable n conflicts with the keyword of the same name.",
    "WARNING: The variable mean conflicts with the keyword of the same name.",
    "ERROR: There are multiple analysis variables associated with a single table cell in the following nesting : all * mean * mean.",
    "ERROR: There are multiple analysis variables associated with a single table cell in the following nesting : n * mean * mean.",
    "NOTE: sasonr stopped processing this step because of errors."))
})

test_that("an unknown name in a TABLE statement is an ERROR", {
  expect_identical(run_log(paste0(have, "proc tabulate data=have; class grp; table grp, v*sum; run;")), c(
    "ERROR: The type of name (v) is unknown.",
    "NOTE: sasonr stopped processing this step because of errors."))
})

test_that("a character analysis variable is an ERROR without a nesting path", {
  expect_identical(run_log(paste0(have, "proc tabulate data=have; class trt; var sub; table trt, sub; run;"))[1],
    "ERROR: Variable sub in list does not match type prescribed for this list.")
})

test_that("a step that passes the checks fails loud: no output yet", {
  expect_error(sas_run(paste0(have, "proc tabulate data=have; class grp; var v; table grp, v*mean; run;")),
               "PROC TABULATE output is not implemented yet", class = "sasonr_unsupported")
  # grp*grp is a WARNING in the step that runs; still no output
  expect_error(sas_run(paste0(have, "proc tabulate data=have; class grp; table grp*grp; run;"), on_error = "sas"),
               "not implemented yet", class = "sasonr_unsupported")
  expect_error(sas_run(paste0(kw, "proc tabulate data=kw; class all n; var mean; table all, mean; run;"), on_error = "sas"),
               "not implemented yet", class = "sasonr_unsupported")
})

test_that("a log that cannot be written exactly fails loud", {
  # the nesting path with ALL, and a statistic spelling not measured
  expect_error(sas_run(paste0(have, "proc tabulate data=have; class grp; var v trt; table grp all, v*trt; run;"), on_error = "sas"),
               "nesting path", class = "sasonr_unsupported")
  expect_error(sas_run(paste0(have, "proc tabulate data=have; class grp; var v; table grp, v*min*max; run;"), on_error = "sas"),
               "nesting path", class = "sasonr_unsupported")
  # an ERROR after the WARNING of a self-crossing
  expect_error(sas_run(paste0(have, "proc tabulate data=have; class grp; var v; table grp*grp, v*mean*sum; run;"), on_error = "sas"),
               "WARNING", class = "sasonr_unsupported")
})

test_that("sas_check() lists every unsupported feature of a TABULATE step", {
  chk <- sas_check(paste0(have, "
proc tabulate data=have out=o noseps;
  by grp;
  class sub / mlf;
  var v / weight=w;
  table sub, v*(n colpctn pctn<sub> cv) / printmiss rts=20;
run;
proc tabulate data=have style={background=gray};
  class grp;
  table grp*{style=[foreground=red]}, n;
run;
"))
  expect_identical(chk$feature, c(
    "PROC TABULATE option OUT=", "PROC TABULATE option NOSEPS", "PROC TABULATE statement BY",
    "CLASS option MLF", "VAR statement options", "TABLE option PRINTMISS", "TABLE option RTS=",
    "PROC TABULATE statistic COLPCTN", "a PCTN denominator in a TABLE statement", "PROC TABULATE statistic CV",
    "PROC TABULATE option STYLE=", "STYLE= on a TABLE element"))
  expect_identical(chk$step, c(rep(2L, 10), 3L, 3L))
})

test_that("sas_check() reports only the missing output for a TABULATE step inside the checked scope", {
  chk <- sas_check(paste0(have, "proc tabulate data=have format=8.2 order=data missing;
    class grp sub / descending; var v; keylabel n='Count' all='Total'; label v='Value';
    table grp all, sub*v*(n mean) / box='B' misstext='-'; run;"))
  expect_identical(chk$feature, "PROC TABULATE output is not implemented yet")
})
