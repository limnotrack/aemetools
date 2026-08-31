# Tests for the parameter/variable declaration (`param_var_matrix`) and the
# `pestpp-ies` localizer built from it. Neither needs a PEST++ binary.

loc_param <- function() {
  data.frame(
    model = "glm_aed",
    file = c("glm3.nml", "glm3.nml", "glm3.nml", "met"),
    group = c("light", "light", "sediment", NA),
    name = c("Kw", "ce", "Fsed_oxy", "MET_tmpair"),
    index = c(1, NA, NA, NA),
    value = c(0.5, 0.0013, -20, 1.0),
    min = c(0.1, -0.001, -40, 0.8),
    max = c(1.5, 0.005, -5, 1.2),
    log = FALSE,
    stringsAsFactors = FALSE
  )
}

# Two observation groups, so a localizer has something to sever.
loc_obs_tbl <- function() {
  ot <- data.frame(
    obsnme = sprintf("o%06d", 1:4),
    obsval = c(12.1, 13.4, 9.8, 8.2),
    weight = 0.4,
    obgnme = c("hyd_temp", "hyd_temp", "chm_oxy", "chm_oxy"),
    stringsAsFactors = FALSE
  )
  attr(ot, "map") <- data.frame(
    obsnme = ot$obsnme,
    var_aeme = c("HYD_temp", "HYD_temp", "CHM_oxy", "CHM_oxy"),
    Date = as.Date("2020-01-01"),
    depth = c(0.5, 5.0, 0.5, 5.0),
    stringsAsFactors = FALSE
  )
  ot
}

loc_vars <- c("HYD_temp", "CHM_oxy")

# as_param_var_matrix ----------------------------------------------------

test_that("a list keyed by variable expands to the canonical dataframe", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = c("light", "MET_tmpair"),
                             CHM_oxy = "Fsed_oxy"),
                        param = p, vars_sim = loc_vars)
  )

  expect_named(pvm, c("model", "file", "name_full", loc_vars))
  expect_equal(pvm$name_full, encode_param(p$group, p$name, p$index))
  # "light" selects both light parameters; MET_tmpair selects by bare name.
  expect_equal(pvm$HYD_temp, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(pvm$CHM_oxy, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("selectors match a full name, a group or a bare name", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = c("light/Kw[1]", "sediment", "ce"),
                             CHM_oxy = "all"),
                        param = p, vars_sim = loc_vars)
  )
  expect_equal(pvm$HYD_temp, c(TRUE, TRUE, TRUE, FALSE))
  expect_true(all(pvm$CHM_oxy))
})

test_that("a variable the list does not mention is left unrestricted", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(CHM_oxy = "sediment"), param = p,
                        vars_sim = loc_vars)
  )
  expect_true(all(pvm$HYD_temp))
  expect_equal(pvm$CHM_oxy, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("an empty selector links a variable to nothing", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "all", CHM_oxy = character(0)),
                        param = p, vars_sim = loc_vars)
  )
  expect_false(any(pvm$CHM_oxy))
})

test_that("a selector that matches nothing is an error", {
  expect_error(
    as_param_var_matrix(list(HYD_temp = "lite"), param = loc_param(),
                        vars_sim = loc_vars),
    "no parameter matches"
  )
})

test_that("a variable that is not being calibrated is an error", {
  expect_error(
    as_param_var_matrix(list(PHY_tchla = "light"), param = loc_param(),
                        vars_sim = loc_vars),
    "not being calibrated"
  )
})

test_that("an unnamed list is rejected", {
  expect_error(
    as_param_var_matrix(list("light"), param = loc_param(),
                        vars_sim = loc_vars),
    "named by variable"
  )
})

test_that("normalising the canonical dataframe is idempotent", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "light", CHM_oxy = "sediment"),
                        param = p, vars_sim = loc_vars)
  )
  again <- suppressMessages(
    as_param_var_matrix(pvm, param = p, vars_sim = loc_vars)
  )
  expect_equal(again, pvm)
})

test_that("create_param_var_matrix output round-trips unchanged", {
  p <- loc_param()
  p$var_sim <- c("HYD_temp", "HYD_temp", "CHM_oxy", "HYD_temp|CHM_oxy")
  pvm <- create_param_var_matrix(p, loc_vars)
  again <- suppressMessages(
    as_param_var_matrix(pvm, param = p, vars_sim = loc_vars)
  )
  expect_equal(again[, loc_vars], pvm[, loc_vars])
})

test_that("rows for uncalibrated parameters are dropped with a plural-safe warning", {
  p <- loc_param()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "light", CHM_oxy = "sediment"),
                        param = p, vars_sim = loc_vars))
  # Canonical dataframe carrying an extra row for a parameter that is not in
  # `param` - `.pvm_from_df()` must drop it and say so. The message uses cli
  # plural markup (`row{?s}`); the quantity must come through or cli aborts
  # with "Cannot pluralize without a quantity".
  stray <- pvm[1, ]
  stray$name_full <- "sediment/not_calibrated"
  pvm2 <- rbind(pvm, stray)

  # setup.R sets AEME.inform = FALSE, which makes AEME::cli_safe() a no-op and
  # hides the bug - force the message on.
  # AEME::cli_safe(FUN = cli::cli_alert_warning) is a styled *message*, not an
  # R warning; setup.R's AEME.inform = FALSE would otherwise suppress it.
  withr::with_options(list(AEME.inform = TRUE), {
    expect_message(
      out1 <- as_param_var_matrix(pvm2, param = p, vars_sim = loc_vars),
      "Dropping .*1.* row\\b")
    stray$name_full <- "sediment/also_not_calibrated"
    expect_message(
      as_param_var_matrix(rbind(pvm2, stray), param = p, vars_sim = loc_vars),
      "Dropping .*2.* rows\\b")
  })
  expect_equal(nrow(out1), nrow(pvm))
})

test_that("a logical matrix is accepted", {
  p <- loc_param()
  m <- matrix(FALSE, nrow = 4, ncol = 2, dimnames = list(NULL, loc_vars))
  m[1:2, "HYD_temp"] <- TRUE
  m[3, "CHM_oxy"] <- TRUE
  m[4, ] <- TRUE
  pvm <- suppressMessages(
    as_param_var_matrix(m, param = p, vars_sim = loc_vars)
  )
  expect_equal(pvm$HYD_temp, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(pvm$CHM_oxy, c(FALSE, FALSE, TRUE, TRUE))
})

# pest_localizer ---------------------------------------------------------

test_that("pest_localizer maps observation groups onto parameters", {
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = c("light", "MET_tmpair"),
                             CHM_oxy = c("sediment", "Kw")),
                        param = p, vars_sim = loc_vars)
  )

  loc <- suppressMessages(pest_localizer(pvm, pt, ot))

  # Dense: every observation group by every parameter.
  expect_equal(dim(loc), c(2L, nrow(pt)))
  expect_equal(rownames(loc), c("hyd_temp", "chm_oxy"))
  expect_equal(colnames(loc), pt$parnme)
  # Entries are binary, because pestpp-ies localization is binary in effect.
  expect_true(all(loc %in% c(0, 1)))

  expect_equal(unname(loc["hyd_temp", ]), c(1, 1, 0, 1))
  expect_equal(unname(loc["chm_oxy", ]), c(1, 0, 1, 0))
})

test_that("a variable absent from the declaration is left unrestricted", {
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  # Declare only HYD_temp, then hand the localizer both groups.
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "light"), param = p,
                        vars_sim = "HYD_temp")
  )
  loc <- suppressMessages(pest_localizer(pvm, pt, ot))
  expect_true(all(loc["chm_oxy", ] == 1))
})

test_that("pest_localizer refuses a parameter linked to no variable", {
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "light", CHM_oxy = "sediment"),
                        param = p, vars_sim = loc_vars)
  )
  # MET_tmpair is linked to nothing, so pestpp-ies would silently fix it.
  expect_error(suppressMessages(pest_localizer(pvm, pt, ot)),
               "treats an unlinked parameter as fixed|not linked to any")
})

test_that("pest_localizer refuses an observation group linked to nothing", {
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "all", CHM_oxy = character(0)),
                        param = p, vars_sim = loc_vars)
  )
  expect_error(suppressMessages(pest_localizer(pvm, pt, ot)),
               "not linked to\\s+any parameter|not linked to any parameter")
})

test_that("pest_localizer refuses a parameter it has no row for", {
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = "all"), param = p,
                        vars_sim = loc_vars)
  )
  expect_error(suppressMessages(pest_localizer(pvm[-1, ], pt, ot)),
               "no row for")
})

test_that("the written localizer round-trips through the PEST matrix format", {
  d <- withr::local_tempdir()
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  pvm <- suppressMessages(
    as_param_var_matrix(list(HYD_temp = c("light", "MET_tmpair"),
                             CHM_oxy = c("sediment", "Kw")),
                        param = p, vars_sim = loc_vars)
  )
  f <- file.path(d, "loc.mat")
  loc <- suppressMessages(pest_localizer(pvm, pt, ot, file = f))

  expect_true(file.exists(f))
  l <- readLines(f)
  # Non-square, so the header carries both dimensions and icode 2.
  expect_equal(l[1], "2 4 2")
  expect_true(any(grepl("^\\*\\s*row", l)))

  back <- aemetools:::.pest_read_cov(f)
  expect_equal(back, loc, ignore_attr = TRUE)
  expect_equal(rownames(back), rownames(loc))
  expect_equal(colnames(back), colnames(loc))
})

# Control wiring ---------------------------------------------------------

test_that("create_pest_control stores a localizer and rejects it for glm", {
  ctrl <- create_pest_control(ncore = 1,
                              localizer = list(HYD_temp = "light"))
  expect_equal(ctrl$localizer, list(HYD_temp = "light"))

  expect_error(
    create_pest_control(exe = "pestpp-glm", ncore = 1,
                        localizer = list(HYD_temp = "light")),
    "pestpp-ies"
  )
})

test_that(".pest_setup_localizer writes the file and sets ++ies_localizer", {
  d <- withr::local_tempdir()
  p <- loc_param()
  pt <- pest_param_table(p)
  ot <- loc_obs_tbl()
  ctrl <- create_pest_control(
    pest_dir = d, case = "aeme", ncore = 1,
    localizer = list(HYD_temp = c("light", "MET_tmpair"),
                     CHM_oxy = c("sediment", "Kw"))
  )

  ctrl <- suppressMessages(
    aemetools:::.pest_setup_localizer(ctrl, par_tbl = pt, obs_tbl = ot,
                                      param = p, vars_sim = loc_vars)
  )

  expect_equal(ctrl$pestpp_options$ies_localizer, "aeme_localizer.mat")
  expect_true(file.exists(file.path(d, "aeme_localizer.mat")))

  # And the option reaches the control file.
  l <- aemetools:::.pest_plusplus_lines(ctrl)
  expect_true(any(grepl("^\\+\\+ies_localizer\\(aeme_localizer.mat\\)$", l)))
})

test_that(".pest_setup_localizer accepts a ready-made file path", {
  d <- withr::local_tempdir()
  src <- file.path(d, "mine.mat")
  writeLines(c("1 1 2", "1.0", "* row names", "hyd_temp",
               "* column names", "p001"), src)

  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              localizer = src)
  ctrl <- suppressMessages(
    aemetools:::.pest_setup_localizer(ctrl, par_tbl = pest_param_table(loc_param()),
                                      obs_tbl = loc_obs_tbl(),
                                      param = loc_param(),
                                      vars_sim = loc_vars)
  )
  expect_equal(ctrl$pestpp_options$ies_localizer, "aeme_localizer.mat")
  expect_equal(readLines(file.path(d, "aeme_localizer.mat"))[1], "1 1 2")
})

test_that("no localizer means no ies_localizer option", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, ncore = 1)
  ctrl2 <- aemetools:::.pest_setup_localizer(
    ctrl, par_tbl = pest_param_table(loc_param()), obs_tbl = loc_obs_tbl(),
    param = loc_param(), vars_sim = loc_vars)
  expect_null(ctrl2$pestpp_options$ies_localizer)
  expect_false(any(grepl("ies_localizer", aemetools:::.pest_plusplus_lines(ctrl2))))
})
