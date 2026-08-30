# Tests for the pestpp-sen (Method of Morris) sensitivity engine behind
# sa_aeme(). Mirrors test-calib_aeme-pest.R: the interface tests need
# neither a PEST++ binary nor AEME, and the one end-to-end test is skipped
# unless have_pest() finds a binary.

# ---- control object ------------------------------------------------------

test_that("create_sen_control builds a pestpp-sen sensitivity control", {
  ctrl <- create_sen_control(morris_r = 8, ncore = 1,
                             vars_sim = make_sen_vars_sim())

  expect_s3_class(ctrl, "calib_sa_control")
  expect_identical(ctrl$method, "sa")
  expect_identical(ctrl$engine, "pest")
  expect_identical(ctrl$exe, "pestpp-sen")
  expect_identical(ctrl$obj_mode, "sa")
  expect_identical(ctrl$sen_method, "morris")
  expect_equal(ctrl$morris_r, 8)
  expect_equal(ctrl$noptmax, 1L)
  # The stall-salvage shortcut in .pest_wait() must stay disabled: a
  # truncated Morris design is invalid, not merely smaller.
  expect_true(is.na(ctrl$itermax))
})

test_that("create_sen_control validates its inputs", {
  expect_error(create_sen_control(vars_sim = list()), "named list")
  expect_error(create_sen_control(vars_sim = list(list(var = "HYD_temp"))),
               "named list")
  expect_error(
    create_sen_control(sen_method = "sobol", vars_sim = make_sen_vars_sim()),
    "morris"
  )
  expect_error(
    create_sen_control(pestpp_options = list(1), vars_sim = make_sen_vars_sim()),
    "named list"
  )
  expect_error(
    create_sen_control(vars_sim = make_sen_vars_sim(), nonsense = 1)
  )
})

test_that("the sensitivity control prints with its own heading", {
  ctrl <- create_sen_control(ncore = 1, vars_sim = make_sen_vars_sim())
  out <- cli::cli_fmt(print(ctrl))
  expect_true(any(grepl("sensitivity analysis control", out)))
})

# ---- run-count estimate ------------------------------------------------

test_that("pest_expected_runs knows the Method of Morris budget", {
  ctrl <- create_sen_control(morris_r = 5, ncore = 1,
                             vars_sim = make_sen_vars_sim())
  expect_equal(pest_expected_runs(ctrl, n_par = 6), (6 + 1) * 5)
  # n_par is required - it is not held in the control.
  expect_true(is.na(pest_expected_runs(ctrl)))

  # Also reachable through a raw pestpp-sen control (defaults: r = 4).
  raw <- create_pest_control(ncore = 1, exe = "pestpp-sen")
  expect_equal(pest_expected_runs(raw, n_par = 4), (4 + 1) * 4)
})

# ---- parameter / observation tables ----------------------------------

test_that("pest_param_table(transform = FALSE) forces linear sampling", {
  p <- make_sen_param()
  expect_true("log" %in% pest_param_table(p, transform = TRUE)$partrans)
  expect_true(all(pest_param_table(p, transform = FALSE)$partrans == "none"))
})

test_that("pest_sa_obs_table makes one observation per sub-region", {
  ctrl <- create_sen_control(
    ncore = 1,
    vars_sim = list(
      surf_temp = list(var = "HYD_temp", month = 1:3, depth_range = c(0, 2)),
      bot_temp  = list(var = "HYD_temp", month = 1:3, depth_range = c(8, 10)),
      thermo_cline = list(var = "HYD_thmcln", month = 1:3, depth_range = c(0, 10))
    )
  )
  ot <- pest_sa_obs_table(ctrl, weights = set_weights(c("HYD_temp",
                                                       "HYD_thmcln")))

  expect_equal(nrow(ot), 3L)
  expect_setequal(ot$obsnme, c("surf_temp", "bot_temp", "thermo_cline"))
  expect_true(all(ot$obsval == 0))
  expect_equal(attr(ot, "map")$var_aeme,
               c("HYD_temp", "HYD_temp", "HYD_thmcln"))
  expect_equal(attr(ot, "map")$name, c("surf_temp", "bot_temp", "thermo_cline"))
})

# ---- control file --------------------------------------------------------

test_that("write_pst emits the Method of Morris ++ options", {
  d <- withr::local_tempdir()
  ctrl <- create_sen_control(pest_dir = d, case = "aeme", ncore = 1,
                             morris_r = 6, morris_p = 8,
                             vars_sim = make_sen_vars_sim())
  param <- make_sen_param()
  param$name_full <- encode_param(param$group, param$name, param$index)

  pt <- pest_param_table(param, transform = FALSE)
  ot <- pest_sa_obs_table(ctrl, weights = set_weights("HYD_temp"))
  pst <- write_pst(pt, ot, ctrl, write_pest_tpl(pt, ctrl),
                   write_pest_ins(ot, ctrl), "Rscript forward_run.R")

  txt <- readLines(pst)
  expect_true(any(grepl("^\\+\\+gsa_method\\(morris\\)$", txt)))
  expect_true(any(grepl("^\\+\\+gsa_morris_r\\(6\\)$", txt)))
  expect_true(any(grepl("^\\+\\+gsa_morris_p\\(8\\)$", txt)))
  expect_true(any(grepl("gsa_morris_obs_sen\\(true\\)", txt)))
  # one observation group per sub-region
  expect_true(any(grepl("^surf_temp$", txt)) && any(grepl("^bot_temp$", txt)))
})

# ---- forward run in sensitivity mode ----------------------------------

test_that("the sa forward run logs one column per sub-region", {
  d <- withr::local_tempdir()
  ctrl <- create_sen_control(pest_dir = d, case = "aeme", ncore = 1,
                             vars_sim = make_sen_vars_sim())
  param <- make_sen_param()
  param$name_full <- encode_param(param$group, param$name, param$index)
  pt <- pest_param_table(param, transform = FALSE)
  ot <- pest_sa_obs_table(ctrl, weights = set_weights("HYD_temp"))
  write_pest_tpl(pt, ctrl)

  # No `aeme` in the payload: run_and_fit() errors, the run is logged as
  # failed. That is enough to exercise the sa-mode plumbing without a model.
  saveRDS(list(case = "aeme", obsnme = ot$obsnme, na_value = 999,
               par_map = attr(pt, "map"), obs_map = attr(ot, "map"),
               param = param, obj_mode = "sa",
               sa_ctrl = ctrl["vars_sim"], fit_names = names(ctrl$vars_sim),
               vars_sim = "HYD_temp", log_dir = file.path(d, "runlog")),
          file.path(d, "aeme_fwd.rds"))

  for (v in list(list(p001 = 0.7, p002 = 0.9, p003 = 1.1),
                 list(p001 = 0.4, p002 = 0.5, p003 = 0.9))) {
    fake_pest_write_pars(file.path(d, "aeme_pars.csv.tpl"),
                         file.path(d, "aeme_pars.csv"), v)
    withr::with_dir(d, suppressMessages(pest_forward_run("aeme_fwd.rds")))
  }

  res <- read_pest_results(ctrl = ctrl, param = param,
                           vars_sim = names(ctrl$vars_sim))

  expect_equal(nrow(res), 2L)
  expect_equal(names(res),
               c(param$name_full, "surf_temp", "bot_temp", "fit", "gen"))
  expect_equal(res[[param$name_full[1]]], c(0.7, 0.4))
  expect_true(all(res$fit == 999))
})

# ---- index parsing -----------------------------------------------------

test_that("read_pest_sen_indices parses a Morris summary", {
  d <- withr::local_tempdir()
  param <- make_sen_param()
  param$name_full <- encode_param(param$group, param$name, param$index)
  pt <- pest_param_table(param, transform = FALSE)

  ctrl <- create_sen_control(pest_dir = d, case = "aeme", ncore = 1,
                             vars_sim = make_sen_vars_sim())
  ot <- pest_sa_obs_table(ctrl, weights = set_weights("HYD_temp"))

  write_fake_maps(d, "aeme", pt, ot)
  write_fake_msn(d, "aeme", groups = c("surf_temp", "bot_temp"),
                 parnmes = pt$parnme)

  ctrl$pest_dir <- d
  idx <- read_pest_sen_indices(ctrl, param, model = "glm_aed")

  expect_setequal(unique(idx$index_type), c("mu_star", "mu", "sigma"))
  expect_setequal(unique(idx$variable), c("surf_temp", "bot_temp"))
  expect_setequal(unique(idx$parameter), param$name_full)
  expect_true(all(is.finite(idx$value)))
  expect_equal(names(idx), c("model", "variable", "parameter", "label",
                             "index_type", "value", "low_ci", "high_ci"))
})

test_that("read_pest_sen_indices returns an empty frame when no output present", {
  d <- withr::local_tempdir()
  param <- make_sen_param()
  ctrl <- create_sen_control(pest_dir = d, case = "aeme", ncore = 1,
                             vars_sim = make_sen_vars_sim())
  ctrl$pest_dir <- d
  idx <- suppressWarnings(read_pest_sen_indices(ctrl, param))
  expect_s3_class(idx, "data.frame")
  expect_equal(nrow(idx), 0L)
  expect_equal(names(idx), c("model", "variable", "parameter", "label",
                             "index_type", "value", "low_ci", "high_ci"))
})

# ---- plotting --------------------------------------------------------

test_that("plot_sen draws a mu* vs sigma scatter", {
  idx <- data.frame(
    sim_id = "L_glmaed_S_001", model = "glm_aed",
    variable = rep(c("surf", "bot"), each = 6),
    parameter = rep(c("a", "b", "c"), 4),
    label = rep(c("a", "b", "c"), 4),
    index_type = rep(rep(c("mu_star", "sigma"), each = 3), 2),
    value = seq_len(12) / 12, low_ci = NA, high_ci = NA
  )
  expect_s3_class(plot_sen(idx), "ggplot")
  expect_s3_class(plot_sen(idx, label = FALSE), "ggplot")
  expect_error(plot_sen(idx[idx$index_type == "mu_star", ]), "Morris")
})

# ---- read_sa guard -------------------------------------------------------

test_that("read_sa sends a pest-engine run to read_sen", {
  d <- withr::local_tempdir()
  sid <- "L_glmaed_S_001"

  write.csv(data.frame(sim_id = sid, id = "L", model = "glm_aed",
                       method = "sa", engine = "pest"),
            file.path(d, "simulation_metadata.csv"), row.names = FALSE)
  write.csv(data.frame(sim_id = sid, id = "L"),
            file.path(d, "lake_metadata.csv"), row.names = FALSE)
  write.csv(data.frame(sim_id = sid, gen = 1, run = 1,
                       parameter_name = "light/Kw", parameter_value = 1,
                       fit_type = "surf", fit_value = 0.5),
            file.path(d, "simulation_data.csv"), row.names = FALSE)
  for (t in c("function_metadata", "parameter_metadata",
              "sensitivity_metadata")) {
    write.csv(data.frame(sim_id = sid), file.path(d, paste0(t, ".csv")),
              row.names = FALSE)
  }

  expect_error(
    read_sa(file_name = "simulation_metadata.csv", file_dir = d, sim_id = sid),
    "read_sen"
  )
})

# ---- end to end --------------------------------------------------------

test_that("sa_aeme dispatches to pestpp-sen and stores Morris indices", {
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")
  install_pest()

  cached <- get_cached_aeme_run(model = "glm_aed", vars_sim = "HYD_temp")
  aeme <- cached$aeme
  path <- cached$path
  inp <- AEME::input(aeme)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model == "glm_aed", !duplicated(name)) |>
    head(4) |>
    as.data.frame()

  d <- withr::local_tempdir()
  ctrl <- create_sen_control(
    file_dir = d, morris_r = 2,
    ncore = max(1L, parallel::detectCores() - 1L),
    vars_sim = list(
      surf_temp = list(var = "HYD_temp", month = c(12, 1, 2),
                       depth_range = c(0, 2)),
      bot_temp  = list(var = "HYD_temp", month = c(12, 1, 2),
                       depth_range = c(inp$init_depth - 2, inp$init_depth))
    )
  )

  fit <- function(df) mean(df$model, na.rm = TRUE)
  sim_id <- sa_aeme(aeme = aeme, model = "glm_aed", param = param,
                    FUN_list = list(HYD_temp = fit), path = path, ctrl = ctrl)

  expect_type(sim_id, "character")
  # A sensitivity run: sim_id is stemmed "S".
  expect_match(sim_id, "_S_\\d+$")
  sim_id <- unname(sim_id[[1]])

  sen <- read_sen(ctrl = ctrl, sim_id = sim_id)
  expect_true(is.list(sen))
  expect_gt(nrow(sen[[sim_id]]$indices), 0)
  # pestpp-sen writes <case>.mos alongside the Morris indices, so
  # read_pest_sen_indices() also returns the range-scaled effect as
  # index_type "scaled_sen".
  expect_setequal(unique(sen[[sim_id]]$indices$index_type),
                  c("mu_star", "mu", "sigma", "scaled_sen"))
  expect_setequal(unique(sen[[sim_id]]$indices$variable),
                  c("surf_temp", "bot_temp"))
  expect_s3_class(plot_sen(sen), "ggplot")

  # The raw model runs reached the sensitivity tables.
  out <- read_simulation_output(ctrl = ctrl, sim_id = sim_id, type = "sa")
  expect_gt(nrow(out$simulation_data), 0)
  expect_equal(unique(out$simulation_metadata$engine), "pest")

  # read_sa refuses a pestpp-sen run.
  expect_error(read_sa(ctrl = ctrl, sim_id = sim_id), "read_sen")
})
