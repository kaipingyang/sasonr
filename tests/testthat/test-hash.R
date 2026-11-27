# DATA step hash objects (DECLARE HASH): the methods, their return codes and
# their effect on the PDV.

hash_put <- function(body) {
  r <- sas_run(paste0("data _null_; length k $4 v 8;\n if _n_ = 1 then do;\n declare hash h();\n h.definekey('k');\n h.definedata('v');\n h.definedone();\n end;\n", body, "\nrun;"))
  as.character(sas_log(r, echo = FALSE))
}

test_that("ADD, FIND, CHECK, REPLACE, REMOVE and their return codes", {
  expect_true("find=0  v=1" %in% hash_put("k = 'one'; v = 1; rc = h.add(); k = 'two'; v = 2; rc = h.add(); k = 'one'; rc = h.find(); put 'find=' rc ' v=' v;"))
  expect_true("rc=160038 v=." %in% hash_put("k = 'one'; v = 1; rc = h.add(); v = .; k = 'missing'; rc = h.find(); put 'rc=' rc 'v=' v;"))
  expect_true("check_a=0  v=10" %in% hash_put("k = 'a'; v = 10; rc = h.add(); v = 10; rc = h.check(); put 'check_a=' rc ' v=' v;"))
  expect_true("add=0  replace=0  find=0  v=2" %in% hash_put("k = 'a'; v = 1; rc1 = h.add(); v = 2; rc2 = h.replace(); v = .; rc3 = h.find(); put 'add=' rc1 ' replace=' rc2 ' find=' rc3 ' v=' v;"))
  expect_true("remove=0  find_after=160038" %in% hash_put("k = 'a'; v = 1; rc = h.add(); rc_rm = h.remove(); rc_f = h.find(); put 'remove=' rc_rm ' find_after=' rc_f;"))
  expect_true("add1=0  add2=160036  v=1" %in% hash_put("k = 'a'; v = 1; rc1 = h.add(); v = 2; rc2 = h.add(); rc3 = h.find(); put 'add1=' rc1 ' add2=' rc2 ' v=' v;"))
  # the return code in a condition; NUM_ITEMS
  expect_true("n=1 found" %in% hash_put("k = 'a'; v = 1; rc = h.add(); n = h.num_items; if h.find() = 0 then put 'n=' n 'found';"))
})

test_that("DATASET: loads a lookup table; a FIND per observation", {
  withr::local_options(sasonr.step_notes = TRUE)
  r <- sas_run("data lookup; input id $ grp $; datalines;
a g1
b g2
;
data have; input id $ x; datalines;
a 1
b 2
z 3
;
data out;
  length id $4 grp $2 x 8;
  if _n_ = 1 then do;
    declare hash h(dataset: 'lookup');
    h.definekey('id');
    h.definedata('grp');
    h.definedone();
  end;
  set have;
  rc = h.find();
  if rc ne 0 then grp = 'NA';
run;")
  expect_identical(r$work$out$grp, c("g1", "g2", "NA"))
  expect_identical(r$work$out$rc, c(0, 0, 160038))
  m <- as.character(sas_log(r, echo = FALSE))
  expect_false(any(grepl("^WARNING: Multiple lengths", m)))
  expect_lt(match("NOTE: There were 2 observations read from the data set WORK.LOOKUP.", m),
            match("NOTE: There were 3 observations read from the data set WORK.HAVE.", m))
})

test_that("what is not measured fails loud", {
  base <- "data _null_; length k $4 v 8; declare hash h(); h.definekey('k'); h.definedata('v'); h.definedone(); k = 'a'; v = 1;"
  expect_error(sas_run(paste(base, "h.find(); run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(base, "rc = h.replace(); run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(base, "rc = h.remove(); run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(base, "rc = h.add(); h.output(dataset: 'o'); run;")), class = "sasonr_unsupported")
  expect_error(sas_run(paste(base, "x = 1; if x = 1 and h.find() = 0 then y = 1; run;")), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; declare hiter it('h'); run;"), class = "sasonr_unsupported")
  expect_error(sas_run("data _null_; rc = h.find(); run;"), class = "sasonr_unsupported")
})

test_that("ADD and REPLACE with KEY: and DATA: values", {
  p <- hash_put("rc = h.add(key: 'x', data: 5); rc2 = h.replace(key: 'x', data: 6); k = 'x'; v = .; rc3 = h.find(); put rc= rc2= rc3= v=;")
  expect_true("rc=0 rc2=0 rc3=0 v=6" %in% p)
  expect_error(sas_run("data _null_; length k $4 v 8; declare hash h(); h.definekey('k'); h.definedata('v'); h.definedone(); rc = h.add(key: 'x'); run;"),
               class = "sasonr_unsupported")
})
