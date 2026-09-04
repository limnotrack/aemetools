# PEST++ tests against the shipped Lake Rototoa record.
#
# test-calib_aeme-pest.R covers the PEST interface against hand-built
# fixtures: three observations, three parameters, exact expected values.
# This file covers the same machinery against a real monitoring record,
# where the things that actually break are different - ~40k observations of
# 32 variables spanning 1988-2025 against a one-year simulation window, so
# most observations are unusable, most variables are not targets, and the
# per-variable weighting has to hold across variables whose units differ by
# orders of magnitude.
#
# The interface tests need neither PEST++ nor a model run. The end-to-end
# tests are skipped unless have_pest() finds a binary, and use 85% of the
# machine's cores (see pest_ncore() in helper-rototoa.R).

# ---- the shipped object ------------------------------------------------

test_that("the shipped Rototoa object is intact and output-free", {
  aeme <- rototoa_aeme()

  expect_s4_class(aeme, "Aeme")
  expect_equal(AEME::lake(aeme)$id, "LID50270")
  expect_equal(AEME::lake(aeme)$name, "Rototoa")
  # GLM-AED only: the object was shipped with gotm_wet dropped.
  expect_equal(unname(AEME::list_models(aeme)), "glm_aed")
  expect_null(AEME::configuration(aeme)$gotm_wet)
  # Biogeochemistry stays on - most of the targetable variables need it.
  expect_true(AEME::configuration(aeme)$use_bgc)

  # remove_output() was applied before shipping: the object carries the
  # inputs and observations a calibration needs, and none of the model
  # output it was distributed with (69 of its 78 MB).
  out <- AEME::output(aeme)
  expect_equal(out$n_members, 0L)
  expect_null(out$ens_001)
  expect_null(out$model_obs_df)
  expect_lt(as.numeric(object.size(aeme)) / 1e6, 20)

  # The parts a calibration does need survived.
  obs <- AEME::observations(aeme)
  expect_gt(nrow(obs$lake), 30000)
  expect_gt(nrow(obs$level), 1000)
  expect_true(all(c("HYD_temp", "CHM_oxy") %in% obs$lake$var_aeme))
  expect_gt(nrow(AEME::input(aeme)$meteo), 7000)
  expect_gt(nrow(AEME::input(aeme)$hypsograph), 50)
})

test_that("every observed, simulatable variable is a target - level included", {
  aeme <- rototoa_window(years = 1)
  vars <- rototoa_vars(aeme)

  # Water level is the one that is easy to lose: its observations live in
  # observations()$level, not $lake, so a set built only from $lake drops it.
  expect_true("LKE_lvlwtr" %in% vars)
  expect_true(all(c("HYD_temp", "CHM_oxy", "CHM_salt", "NIT_amm", "PHS_tp",
                    "PHY_tchla", "HYD_strat") %in% vars))
  expect_gte(length(vars), 20)

  # Every one of them is switched on in model_controls, or the model would
  # never write it.
  mc <- AEME::configuration(aeme)$model_controls
  expect_true(all(vars %in% mc$var_aeme[mc$simulate]))

  # And the ones deliberately left out are the ones the model cannot make:
  # secchi, turbidity, TLI and the saturation/derived series.
  obs <- AEME::observations(aeme)
  observed <- unique(c(obs$lake$var_aeme, obs$level$var_aeme))
  left_out <- setdiff(observed, mc$var_aeme[mc$simulate])
  expect_true(all(c("RAD_secchi", "CHM_oxysat", "LKE_tli3") %in% left_out))
  expect_length(intersect(vars, left_out), 0)
})

test_that("the meteorology covers the shipped window and its spin-up", {
  aeme <- rototoa_aeme()
  tme <- AEME::time(aeme)
  met <- range(AEME::input(aeme)$meteo$Date)

  # The shipped object asks for a five-year spin-up; the forcing has to
  # reach back that far or every model run fails at the first timestep.
  expect_lte(met[1], as.Date(tme$start) - max(unlist(tme$spin_up)))
  expect_gte(met[2], as.Date(tme$stop))
})

# ---- observation table on a real record --------------------------------

test_that("pest_obs_table keeps only observations the model can simulate", {
  aeme <- rototoa_window(years = 1)
  tme <- AEME::time(aeme)
  obs <- AEME::observations(aeme)$lake

  ot <- pest_obs_table(aeme, vars_sim = "HYD_temp",
                       weights = set_weights("HYD_temp"))
  map <- attr(ot, "map")

  # Rototoa carries HYD_temp back to 1988; a one-year window can use only a
  # fraction of it. An observation outside the window still carries weight,
  # so leaving it in would let phi be dominated by residuals the model was
  # never asked to reproduce.
  all_temp <- sum(obs$var_aeme == "HYD_temp" & !is.na(obs$value))
  expect_lt(nrow(ot), all_temp)
  expect_gt(nrow(ot), 0)
  expect_true(all(map$Date >= as.Date(tme$start)))
  expect_true(all(map$Date <= as.Date(tme$stop)))

  # Only the requested variable becomes an observation, out of the 30-odd
  # in the record.
  expect_equal(unique(map$var_aeme), "HYD_temp")
  expect_equal(unique(ot$obgnme), "hyd_temp")
  expect_equal(nrow(ot), nrow(map))
  expect_false(any(duplicated(ot$obsnme)))
  expect_true(all(is.finite(ot$obsval)))
  expect_true(all(ot$weight > 0 & is.finite(ot$weight)))
})

test_that("balanced weighting equalises variables of different magnitude", {
  # Rototoa's oxygen is ~10 mg/L with a spread of a few, temperature ~15 C
  # with a spread of ~5, and there are several times more oxygen readings.
  # Unit weights would let whichever variable has the larger residuals in
  # its own units dominate phi; balanced weighting is what stops that.
  aeme <- rototoa_window(years = 1)
  vars <- c("HYD_temp", "CHM_oxy")
  w <- set_weights(vars)

  bal <- pest_obs_table(aeme, vars_sim = vars, weights = w,
                        weight_method = "balanced")
  unit <- pest_obs_table(aeme, vars_sim = vars, weights = w,
                         weight_method = "unit")
  expect_setequal(unique(bal$obgnme), c("hyd_temp", "chm_oxy"))
  expect_equal(nrow(bal), nrow(unit))

  # Each group's initial contribution, measured as sum((weight * spread)^2),
  # should be far closer between groups under balanced weighting.
  grp_share <- function(tbl) {
    m <- attr(tbl, "map")
    s <- tapply(seq_len(nrow(tbl)), tbl$obgnme, function(i) {
      sum((tbl$weight[i] * (tbl$obsval[i] - mean(tbl$obsval[i])))^2)
    })
    max(s) / min(s)
  }
  expect_lt(grp_share(bal), grp_share(unit))

  # The counts differ a lot between the two variables; balanced weighting
  # must not simply be "more observations, more influence".
  n <- table(bal$obgnme)
  expect_gt(max(n) / min(n), 1)

  # The same has to hold across the whole target set, where the variables
  # range from water level in metres to phosphorus in mg/L and the counts
  # span two orders of magnitude.
  allv <- rototoa_vars(aeme)
  full <- pest_obs_table(aeme, vars_sim = allv, weights = set_weights(allv))
  expect_setequal(unique(full$obgnme), aemetools:::.pest_safe_name(allv))
  expect_true(all(full$weight > 0 & is.finite(full$weight)))
  cnt <- table(full$obgnme)
  expect_gt(max(cnt) / min(cnt), 10)
  # No group's weights collapse to zero or blow up, whatever its spread.
  rng <- tapply(full$weight, full$obgnme, function(w) max(w) / min(w))
  expect_true(all(is.finite(rng)))
})

test_that("a variable with no in-window observations is refused", {
  # NIT_nit is in the record but has nothing in this window, while HYD_temp
  # has 442 readings. Asking for both must fail loudly, naming the variable,
  # rather than writing a .pst with an empty observation group.
  aeme <- rototoa_window(years = 1)
  vars <- c("HYD_temp", "NIT_nit")
  expect_error(
    pest_obs_table(aeme, vars_sim = vars, weights = set_weights(vars)),
    "NIT_nit")

  # A window with no observations at all is a different failure, and says so.
  empty <- AEME::set_time(rototoa_aeme(), start = "2020-01-01",
                          stop = "2020-01-15", spin_up = 365)
  expect_error(
    pest_obs_table(empty, vars_sim = "HYD_temp",
                   weights = set_weights("HYD_temp")),
    "No observations fall within the simulation period")
})

# ---- control file assembly ---------------------------------------------

test_that("the PEST interface files describe the real record correctly", {
  aeme <- rototoa_window(years = 1)
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "rototoa", ncore = 1,
                              noptmax = 2, ies_num_reals = 8)

  param <- rototoa_param("glm_aed", n = 4)
  pt <- pest_param_table(param)
  ot <- pest_obs_table(aeme, vars_sim = "HYD_temp",
                       weights = set_weights("HYD_temp"))

  tpl <- write_pest_tpl(pt, ctrl)
  ins <- write_pest_ins(ot, ctrl)
  pst <- write_pst(par_tbl = pt, obs_tbl = ot, ctrl = ctrl,
                   tpl_files = tpl, ins_files = ins,
                   model_command = "Rscript forward_run.R")

  expect_true(file.exists(pst))
  txt <- readLines(pst)

  # The control-data line carries NPAR and NOBS; both must match the tables.
  hdr <- trimws(txt[grep("^\\* control data", txt) + 2])
  nums <- as.integer(strsplit(hdr, "\\s+")[[1]])
  expect_equal(nums[1], nrow(pt))
  expect_equal(nums[2], nrow(ot))

  # write_pest_tpl()/write_pest_ins() return c("<pest file>" = "<model
  # file>"), so the file written is the *name*, not the value.
  ins_txt <- readLines(file.path(d, names(ins)))
  # Every observation in the table reaches the instruction file.
  expect_equal(sum(grepl("^l1 w !", ins_txt)), nrow(ot))
  expect_true(all(vapply(ot$obsnme, function(o) {
    any(grepl(paste0("!", o, "!"), ins_txt, fixed = TRUE))
  }, logical(1))))

  # Every parameter reaches the template as a ~name~ field.
  tpl_txt <- readLines(file.path(d, names(tpl)))
  expect_identical(tpl_txt[1], "ptf ~")
  for (p in pt$parnme) expect_match(paste(tpl_txt, collapse = "\n"), p)
})

test_that("a localizer ties each variable to its own parameters", {
  aeme <- rototoa_window(years = 1)
  vars <- c("HYD_temp", "CHM_oxy")
  param <- rototoa_param("glm_aed", n = 4)

  # Half the parameters to each variable, so the localizer is not the
  # all-TRUE matrix that would be indistinguishable from having none.
  pvm <- as_param_var_matrix(
    stats::setNames(list(param$name[1:2], param$name[3:4]), vars),
    param = param |> dplyr::mutate(name_full = encode_param(group, name, index)),
    vars_sim = vars)

  d <- withr::local_tempdir()
  pt <- pest_param_table(param)
  ot <- pest_obs_table(aeme, vars_sim = vars, weights = set_weights(vars))

  f <- file.path(d, "rototoa.loc.mat")
  # pest_localizer() returns the matrix invisibly and writes the file.
  loc <- pest_localizer(pvm, par_tbl = pt, obs_tbl = ot, file = f)
  expect_true(file.exists(f))

  expect_true(is.matrix(loc))
  expect_setequal(rownames(loc), c("hyd_temp", "chm_oxy"))
  expect_equal(ncol(loc), nrow(pt))
  # The whole point of a localizer: some links are severed, so an oxygen
  # residual cannot move a parameter assigned to temperature.
  expect_true(any(loc == 0))
  expect_false(all(loc == 1))
  # Every group and every parameter still has at least one live link.
  expect_true(all(rowSums(loc) > 0))
  expect_true(all(colSums(loc) > 0))

  body <- paste(readLines(f), collapse = " ")
  expect_match(body, "hyd_temp")
  expect_match(body, "chm_oxy")
})

test_that("get_calib_periods splits the real record sensibly", {
  aeme <- rototoa_aeme()
  p <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"))

  expect_lt(p$periods$stop[1], p$periods$start[2])
  expect_true(all(p$coverage$n_obs > 0))
  # A record this long should give both periods a full seasonal cycle -
  # unlike the AEME test lake, where the validation period spans five months.
  expect_equal(p$periods$n_months[1], 12)
  expect_equal(p$periods$n_months[2], 12)

  # The split feeds the PEST path through the simulation window.
  cal <- set_calib_period(aeme, p, "calib")
  expect_equal(as.Date(AEME::time(cal)$start), p$periods$start[1])
  ot <- pest_obs_table(cal, vars_sim = "HYD_temp",
                       weights = set_weights("HYD_temp"))
  map <- attr(ot, "map")
  expect_true(all(map$Date <= p$periods$stop[1]))

  # Conditioning on everything instead covers strictly more.
  all <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"),
                           split = FALSE)
  expect_gt(all$periods$n_obs, p$periods$n_obs[1])
})

# ---- end to end --------------------------------------------------------

test_that("calib_aeme calibrates Rototoa on HYD_temp with pestpp-ies", {
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")

  cached <- rototoa_run(model = "glm_aed", vars_sim = "HYD_temp",
                        use_bgc = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  param <- rototoa_param("glm_aed", n = 4)

  # Deliberately small: one iteration, a single lambda multiplier and scale
  # factor. The point is that the whole path works on this object, not that
  # it converges.
  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 8,
    ncore = pest_ncore(),
    file_dir = file.path(path, "calib_sa"), pest_dir = "pest",
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1))

  sim_id <- calib_aeme(aeme = aeme, path = path, param = param,
                       model = "glm_aed", vars_sim = "HYD_temp",
                       FUN_list = list(HYD_temp = kge_loss),
                       weights = set_weights("HYD_temp"), ctrl = ctrl)

  expect_type(sim_id, "character")
  sim_id <- unname(sim_id[[1]])

  # Every evaluation reached the results database, so the built-in helpers
  # work on a PEST run unchanged.
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  expect_gt(nrow(calib$simulation_data), 0)
  expect_equal(unique(calib$simulation_metadata$engine), "pest")

  # Read through `calib`, not `ctrl`: pest_dir is resolved against the lake
  # directory when the run starts, so the control still holds the relative
  # path while the files are under <lake_dir>/pest.
  phi <- read_pest_phi(calib)
  expect_s3_class(phi, "data.frame")
  expect_gt(nrow(phi), 0)
  expect_true(all(c("iteration", "total_runs", "min") %in% names(phi)))

  best <- get_best_params(calib = calib)
  expect_equal(nrow(best), nrow(param))
  # The calibrated values are inside the bounds they were given.
  expect_true(all(best$value >= best$min & best$value <= best$max))
})

test_that("a Rototoa run uses 85% of the machine's cores", {
  # The agent count is what actually consumes the machine, so it is worth
  # pinning: create_pest_control() must carry through the value it is given
  # rather than falling back to its own default.
  n <- pest_ncore()
  expect_equal(n, max(1L, floor(0.85 * parallel::detectCores())))
  expect_lte(n, parallel::detectCores())
  expect_gte(n, 1L)

  ctrl <- create_pest_control(exe = "pestpp-ies", ncore = n)
  expect_equal(ctrl$ncore, n)
  expect_true(ctrl$parallel)
})

test_that("every variable reaches the PEST interface, water level included", {
  # The full target set - all 20 variables, LKE_lvlwtr among them - through
  # the whole file-writing path. The end-to-end solve for this set is
  # blocked by the two failures documented in rototoa_indices_ok() and in
  # the test below it; everything up to handing over to the solver works,
  # and that is what this pins.
  skip_on_cran()

  cached <- rototoa_run(model = "glm_aed")   # all targetable variables
  aeme <- cached$aeme
  path <- cached$path
  vars <- cached$vars_sim
  expect_true("LKE_lvlwtr" %in% vars)
  expect_gte(length(vars), 20)

  # Every variable reaches the observation table as its own group, water
  # level included - it comes from observations()$level and is matched
  # against the modelled level, not a gridded profile.
  ot <- pest_obs_table(aeme, vars_sim = vars, weights = set_weights(vars))
  expect_setequal(unique(ot$obgnme), aemetools:::.pest_safe_name(vars))
  lmap <- attr(ot, "map")
  expect_gt(sum(lmap$var_aeme == "LKE_lvlwtr"), 0)
  # Level is depth-less; every gridded variable carries a depth.
  expect_true(all(is.na(lmap$depth[lmap$var_aeme == "LKE_lvlwtr"])))
  expect_false(any(is.na(lmap$depth[lmap$var_aeme == "HYD_temp"])))

  param <- rototoa_param("glm_aed", n = 4)
  pt <- pest_param_table(param)
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "rototoa", ncore = 1,
                              noptmax = 1, ies_num_reals = 8)

  # The whole interface, assembled for all 20 groups at once.
  tpl <- write_pest_tpl(pt, ctrl)
  ins <- write_pest_ins(ot, ctrl)
  pst <- write_pst(par_tbl = pt, obs_tbl = ot, ctrl = ctrl,
                   tpl_files = tpl, ins_files = ins,
                   model_command = "Rscript forward_run.R")
  txt <- readLines(pst)
  nums <- as.integer(strsplit(trimws(txt[grep("^\\* control data", txt) + 2]),
                              "\\s+")[[1]])
  expect_equal(nums[1], nrow(pt))
  expect_equal(nums[2], nrow(ot))

  # One observation group per variable, all 20 declared in the control file.
  grp_block <- txt[(grep("^\\* observation groups", txt) + 1):
                     (grep("^\\* observation data", txt) - 1)]
  expect_setequal(trimws(grp_block), aemetools:::.pest_safe_name(vars))
  expect_equal(sum(grepl("^l1 w !", readLines(file.path(d, names(ins))))),
               nrow(ot))
})

test_that("every calibratable variable solves end to end, level included", {
  # The full target set less the three whose simulated equivalent is
  # structurally non-finite (see rototoa_nonfinite_vars): 17 variables,
  # water level among them, through a real pestpp-ies solve. LKE_lvlwtr
  # matters here because its residuals are not a gridded profile, so it
  # takes a separate path through the forward run.
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")

  vars <- rototoa_calibratable()
  expect_true("LKE_lvlwtr" %in% vars)
  expect_gte(length(vars), 17)

  cached <- rototoa_run(model = "glm_aed", vars_sim = vars, use_bgc = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  param <- rototoa_param("glm_aed", n = 4)

  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 8,
    ncore = pest_ncore(),
    file_dir = file.path(path, "calib_sa"), pest_dir = "pest",
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1))

  sim_id <- calib_aeme(aeme = aeme, path = path, param = param,
                       model = "glm_aed", vars_sim = vars,
                       FUN_list = rototoa_fun_list(vars),
                       weights = set_weights(vars), ctrl = ctrl)
  expect_type(sim_id, "character")
  calib <- read_calib(ctrl = ctrl, sim_id = unname(sim_id[[1]]))

  # Per-group phi is what tells you whether one variable is swamping the
  # rest - the reason pest_adjust_weights() exists. Seventeen groups is also
  # a real exercise of the character-realisation-column fix.
  pg <- read_pest_phi_group(calib)
  expect_s3_class(pg, "data.frame")
  expect_gt(nrow(pg), 0)
  expect_equal(length(unique(pg$obgnme)), length(vars))
  expect_true(all(c("hyd_temp", "lke_lvlwtr") %in% unique(pg$obgnme)))
  expect_type(pg$phi, "double")
  expect_true(all(is.finite(pg$phi)))

  # Every group's residuals came back, water level among them.
  resid <- pest_residuals(calib)
  expect_gt(nrow(resid), 0)
  expect_true("LKE_lvlwtr" %in% resid$var_aeme)
  expect_equal(resid$residual, resid$model - resid$obs)
})

test_that("a structurally non-finite target is named, not just failed", {
  # Regression for the diagnosis above: adding PHY_cyano - which this AED
  # configuration never produces - fails every realisation, and PEST++
  # reports only "all realizations failed during initial evaluation". The
  # forward run now says which variables were non-finite and which were
  # never produced at all, so the fix is obvious from the message.
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")

  vars <- c("HYD_temp", "PHY_cyano")
  cached <- rototoa_run(model = "glm_aed", vars_sim = vars, use_bgc = TRUE)
  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 6, ncore = 2,
    file_dir = file.path(cached$path, "calib_sa"), pest_dir = "pest")

  err <- tryCatch(
    calib_aeme(aeme = cached$aeme, path = cached$path,
               param = rototoa_param("glm_aed", n = 4), model = "glm_aed",
               vars_sim = vars, FUN_list = rototoa_fun_list(vars),
               weights = set_weights(vars), ctrl = ctrl),
    error = conditionMessage)

  expect_type(err, "character")
  expect_match(err, "not finite")
  expect_match(err, "PHY_cyano")
  expect_match(err, "drop from vars_sim")
})

test_that("validate_aeme scores a calibrated Rototoa on held-out data", {
  skip_on_cran()

  # Water level plus a gridded variable: both residual shapes at once, and
  # use_bgc = FALSE because that pairing is what works today (see
  # rototoa_indices_ok()).
  vars <- c("HYD_temp", "LKE_lvlwtr")
  cached <- rototoa_run(model = "glm_aed", vars_sim = vars, years = 2,
                        use_bgc = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  # get_calib_periods() splits the whole 1988-2025 record and takes no view
  # on the simulation window, so trim first - otherwise both periods fall
  # outside the two years the model was built for and neither run scores
  # anything.
  p <- get_calib_periods(rototoa_trim_obs(aeme), vars_sim = vars)
  expect_gte(p$periods$start[1], as.Date(AEME::time(aeme)$start))
  expect_lte(p$periods$stop[2], as.Date(AEME::time(aeme)$stop))
  param <- rototoa_param("glm_aed", n = 4)

  v <- validate_aeme(aeme = aeme, param = param, periods = p,
                     model = "glm_aed", vars_sim = vars, path = path,
                     FUN_list = rototoa_fun_list(vars))

  expect_s3_class(v, "aeme_validation")
  expect_setequal(v$fit$period, c("calib", "valid"))
  expect_setequal(v$fit$var_aeme, vars)
  # Both periods scored both variables - the whole point of splitting a
  # record this dense.
  expect_true(all(v$fit$n_obs > 0))
  expect_true(all(is.finite(v$fit$fit)))
  expect_equal(nrow(v$degradation), 2)
  expect_setequal(v$degradation$var_aeme, vars)
  for (v_ in vars) {
    expect_equal(v$degradation$degradation[v$degradation$var_aeme == v_],
                 v$fit$fit[v$fit$period == "valid" & v$fit$var_aeme == v_] -
                   v$fit$fit[v$fit$period == "calib" & v$fit$var_aeme == v_])
  }
})
