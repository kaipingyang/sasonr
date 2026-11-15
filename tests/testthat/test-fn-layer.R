# Direct calls of the function layer (R/zzz_fn_layer.R) pass the same gates as
# the functions in a SAS program: the feature registry (strict mode), the
# format registry for PUT()/INPUT(), the argument domain of the policy table,
# and data errors as warnings of class sasonr_data_error.

fn_layer_exports <- function() {
  ex <- grep("^sas_f_", getNamespaceExports("sasonr"), value = TRUE)
  ex[!sub("^sas_f_", "", ex) %in% sasonr:::fn_layer_internal]
}

test_that("every exported SAS function goes through the direct-call gate", {
  for (fn in fn_layer_exports()) {
    f <- getExportedValue("sasonr", fn)
    expect_identical(as.character(body(f)[[1]]), "fn_layer_call", info = fn)
    expect_identical(formals(f), formals(sasonr:::fn_layer_impl[[sub("^sas_f_", "", fn)]]), info = fn)
  }
})

test_that("approximate and unsupported functions fail loud in strict mode and run otherwise", {
  reg <- sas_feature_registry("functions")
  for (fn in fn_layer_exports()) {
    st <- sasonr:::feature_status("function", sub("^sas_f_", "", fn))
    if (!sasonr:::feature_gated(st)) next
    # the gate comes before the arguments are used
    expect_error(getExportedValue("sasonr", fn)(), class = "sasonr_strict_error", info = fn)
  }
  expect_error(sas_f_probt(1.5, 10), class = "sasonr_strict_error")
  expect_error(sas_f_exp(1), class = "sasonr_strict_error")
  withr::local_options(sasonr.strict = FALSE)
  expect_warning(v <- sas_f_probt(1.5, 10), class = "sasonr_strict_warning")
  expect_equal(v, 0.9177463367772799, tolerance = 1e-12)
  expect_warning(v <- sas_f_exp(0), class = "sasonr_strict_warning")
  expect_identical(v, 1)
})

test_that("supported functions run without a condition", {
  expect_identical(sas_f_round(2.5), 3)
  expect_identical(sas_f_cats("a ", " b"), "ab")
  expect_identical(sas_f_sqrt(c(4, 9)), c(2, 3))
})

test_that("arguments outside the supported domain fail loud in direct calls", {
  dom_points <- list(
    anyalnum = list("a", 1e+15),
    anyalpha = list("a", 1e+15),
    anydigit = list("a", 1e+15),
    anylower = list("a", 1e+15),
    anypunct = list("a", 1e+15),
    anyspace = list("a", 1e+15),
    anyupper = list("a", 1e+15),
    byte = list(0.5),
    cdf = list("NORMAL", 1e+300),
    cinv = list(1e+300, 0.5),
    collate = list(0.5),
    comb = list(1152921504606846976, 0.5),
    cos = list(1e+15),
    datdif = list(1e+15, 0.5, "a"),
    datejul = list(171.5),
    datepart = list(1e+14),
    day = list(1e+15),
    dhms = list(1e+15, 0.5, 0.5, 0.5),
    findw = list("a", "a", 0.5),
    finv = list(1e+300, 0.5, 0.5),
    gamma = list(171.5),
    gcd = list(1e+300, 0.5),
    geomean = list(1e+300, 0.5),
    harmean = list(1e+300, 0.5),
    hms = list(1e+300, 0.5, 0.5),
    hour = list(1e+15),
    indexw = list("a", "a", "a"),
    inputn = list("a", "a", -0.5),
    juldate = list(1e+15),
    juldate7 = list(1e+15),
    lcm = list(1e+300, 0.5),
    logcdf = list("NORMAL", 1e+300),
    logpdf = list("NORMAL", 1e+300),
    logsdf = list("NORMAL", 1e+300),
    mdy = list(1152921504606846976, 0.5, 0.5),
    minute = list(1e+15),
    mod = list(1152921504606846976, 0.5),
    modz = list(1152921504606846976, 0.5),
    month = list(1e+15),
    notalnum = list("a", 1e+15),
    notalpha = list("a", 1e+15),
    notdigit = list("a", 1e+15),
    notlower = list("a", 1e+15),
    notpunct = list("a", 1e+15),
    notspace = list("a", 1e+15),
    notupper = list("a", 1e+15),
    pdf = list("NORMAL", 1e+300),
    perm = list(1e+300),
    poisson = list(1e+300, 0.5),
    probbnml = list(1e+300, 0.5, 0.5),
    probchi = list(1e+300, 0.5),
    probf = list(1e+300, 0.5, 0.5),
    probt = list(1e+300, 0.5),
    putn = list(0.5, "a", -0.5),
    qtr = list(1e+15),
    quantile = list("NORMAL", 1e+300),
    `repeat` = list("a", 1152921504606846976),
    sdf = list("NORMAL", 1e+300),
    second = list(1e+15),
    sin = list(1e+15),
    substrn = list(0.5, 1e+15),
    tan = list(1e+15),
    timepart = list(1e+14),
    tinv = list(1e+300, 0.5),
    week = list(1e+15),
    weekday = list(1e+15),
    year = list(1e+15),
    yrdif = list(1e+15, 0.5)
  )
  for (nm in c("byte", "collate", "datejul", "dhms", "gcd", "harmean", "hms", "lcm", "weekday",
               "anyalnum", "cos", "datdif", "datepart", "day", "gamma", "hour", "juldate", "mdy",
               "sin", "tan", "timepart", "week", "year")) {
    expect_true(nm %in% names(dom_points), info = nm)
  }
  withr::local_options(sasonr.strict = FALSE)
  for (nm in names(dom_points)) {
    f <- getExportedValue("sasonr", paste0("sas_f_", nm))
    expect_error(withCallingHandlers(do.call(f, dom_points[[nm]]),
                                     sasonr_strict_warning = function(w) invokeRestart("muffleWarning")),
                 class = "sasonr_unsupported", info = nm)
  }
})

test_that("data errors of direct calls are warnings of class sasonr_data_error", {
  expect_warning(v <- sas_f_sqrt(-1), "Invalid argument to function SQRT", class = "sasonr_data_error")
  expect_true(is.na(v))
  expect_warning(v <- sas_f_log(0), class = "sasonr_data_error")
  expect_true(is.na(v))
  # a step exception has no step to stop outside a submission
  expect_error(sas_f_constant("EXACTINT", 1e300), class = "sasonr_unsupported")
})

test_that("PUT() and INPUT() take a format as text and check it", {
  expect_identical(sas_f_put(3.14159, "8.2"), "    3.14")
  expect_identical(sas_f_input("12", "3."), 12)
  expect_error(sas_f_put(21915, "ddmmyyb10."), class = "sasonr_strict_error")
  expect_error(sas_f_input("12", "bestd10."), class = "sasonr_strict_error")
  expect_error(sas_f_putn(21915, "ddmmyyb10."), class = "sasonr_strict_error")
})

test_that("calls inside a submission are not checked twice", {
  res <- sas_run("data a; y = sqrt(-1); z = put(3.14159, 8.2); w = round(2.5); run;")
  expect_true(is.na(res$work$a$y))
  expect_identical(res$work$a$z, "    3.14")
  expect_true(any(grepl("Invalid argument to function SQRT", res$log)))
})

test_that("a direct call passes the arguments as written to the implementation", {
  impl <- function(nm) sasonr:::fn_layer_impl[[nm]]
  # an empty positional argument and the same argument by name
  expect_identical(sas_f_collate(48, , 10), "0123456789")
  expect_identical(sas_f_collate(48, len = 10), "0123456789")
  expect_identical(sas_f_collate(len = 10, start = 48), "0123456789")
  expect_identical(sas_f_collate(48, , 10), impl("collate")(48, , 10))
  # named and positional arguments mixed, in any order
  expect_identical(sas_f_substr(pos = 2, "abcdef"), impl("substr")(pos = 2, "abcdef"))
  expect_identical(sas_f_substr("abcdef", len = 2, pos = 3), "cd")
  expect_identical(sas_f_round(unit = 0.1, 2.45), impl("round")(unit = 0.1, 2.45))
  # defaults of omitted arguments
  expect_identical(sas_f_round(2.5), impl("round")(2.5))
  expect_identical(sas_f_scan("a b c", 2), impl("scan")("a b c", 2))
  # `...`: given directly, forwarded by another function, and with an empty
  # element (an error, as for the implementation)
  expect_identical(sas_f_cats("a ", " b", "c"), impl("cats")("a ", " b", "c"))
  fwd <- function(...) sas_f_cats(...)
  expect_identical(fwd("x ", " y"), "xy")
  expect_error(sas_f_cats("a", , "b"))
  expect_error(impl("cats")("a", , "b"))
  expect_identical(sas_f_sum(1, NA, 3), impl("sum")(1, NA, 3))
  # each argument is evaluated once
  k <- 0
  arg <- function() { k <<- k + 1; 4 }
  expect_identical(sas_f_sqrt(arg()), 2)
  expect_identical(k, 1)
})
