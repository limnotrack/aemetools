# partrans = "fixed" for frozen parameters, and carrying a previous
# pestpp-ies run's posterior in as the next run's prior ensemble.
# No PEST++ binary needed - these exercise the table/ensemble plumbing.
# Fixtures pest_param() / make_obs_tbl() / make_pest_dir() live in helper-pest.R.

nf_cols <- function(p) {
  p$name_full <- encode_param(p$group, p$name, p$index)
  p
}

# ---- Feature 1: partrans = "fixed" -----------------------------------------

frozen_param <- function() {
  rbind(
    pest_param(),                          # Kw[1], ce  (both adjustable)
    data.frame(model = "glm_aed", file = "glm4.nml", group = "mixing",
               name = "coef_mix_conv", index = NA_integer_,
               value = 0.2, min = 0.2, max = 0.2, log = FALSE,
               stringsAsFactors = FALSE))
}

test_that("pest_param_table() writes a frozen parameter as partrans = fixed", {
  tbl <- pest_param_table(frozen_param())

  expect_equal(tbl$partrans, c("none", "none", "fixed"))
  expect_equal(tbl$parval1[3], 0.2)
  expect_equal(tbl$parlbnd[3], 0.2)
  expect_equal(tbl$parubnd[3], 0.2)
  expect_equal(tbl$parchglim[3], "relative")
  expect_true(any(grepl("coef_mix_conv", attr(tbl, "map")$name_full, fixed = TRUE)))
})

test_that("pest_param_table() still rejects a genuinely inverted range", {
  p <- pest_param()
  p$min[1] <- 2; p$max[1] <- 1                    # min > max, not fixed
  expect_error(pest_param_table(p), "min|adjustable")
})

test_that("write_pst() keeps the fixed row and its template field", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              noptmax = 2)
  pt <- pest_param_table(frozen_param())
  ot <- make_obs_tbl()
  tpl <- write_pest_tpl(pt, ctrl)
  ins <- write_pest_ins(ot, ctrl)
  write_pst(pt, ot, ctrl, tpl, ins, "cmd")

  pst <- readLines(file.path(d, "aeme.pst"))
  fixed_pn <- pt$parnme[pt$partrans == "fixed"]
  expect_length(fixed_pn, 1)
  expect_true(any(grepl(paste0("^", fixed_pn, "\\s+fixed\\b"), pst)))

  tpl_lines <- readLines(file.path(d, names(tpl)[1]))
  expect_true(any(grepl(fixed_pn, tpl_lines, fixed = TRUE)))

  pm <- utils::read.csv(file.path(d, "aeme_par_map.csv"))
  expect_true(fixed_pn %in% pm$parnme)
})

test_that("pest_localizer() ignores a fixed parameter instead of aborting", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  pt <- pest_param_table(frozen_param())
  ot <- make_obs_tbl()

  pvm <- as_param_var_matrix(list(HYD_temp = c("Kw", "ce")),
                             param = nf_cols(frozen_param()),
                             vars_sim = "HYD_temp")

  expect_no_error(loc <- pest_localizer(pvm, par_tbl = pt, obs_tbl = ot))
  fixed_pn <- pt$parnme[pt$partrans == "fixed"]
  expect_false(fixed_pn %in% colnames(loc))
  expect_setequal(colnames(loc), pt$parnme[pt$partrans != "fixed"])
})

# ---- Feature 2: carry a posterior ensemble forward ------------------------

# destination set: one NEW parameter FIRST (so the shared parameters land on
# different parnme than in the source), then the two the source calibrated.
carry_dest_param <- function() {
  rbind(
    data.frame(model = "glm_aed", file = "glm4.nml", group = "mixing",
               name = "coef_mix_conv", index = NA_integer_,
               value = 0.2, min = 0.1, max = 0.3, log = FALSE,
               stringsAsFactors = FALSE),
    pest_param())
}

test_that(".pest_carry_ensemble() carries shared columns and draws the rest", {
  src <- make_pest_dir()                    # list(d, ctrl, param, pt, ot)
  param <- nf_cols(carry_dest_param())
  pt <- pest_param_table(param)

  out <- aemetools:::.pest_carry_ensemble(
    source = src$d, par_tbl = pt, param = param, n = 3,
    dist = "uniform", seed = 99)

  expect_equal(names(out), c("real_name", "p001", "p002", "p003"))
  expect_equal(out$real_name, c("0", "1", "2"))

  src_post <- utils::read.csv(file.path(src$d, "aeme.2.par.csv"),
                              check.names = FALSE)
  set.seed(99); idx <- sample.int(nrow(src_post), 3)
  expect_equal(unname(out$p002), unname(src_post$p001[idx]))   # Kw[1]
  expect_equal(unname(out$p003), unname(src_post$p002[idx]))   # ce

  expect_true(all(out$p001 >= 0.1 & out$p001 <= 0.3))          # new draw
})

test_that(".pest_setup_ensembles() wires prior_par_ensemble = <run> through", {
  src <- make_pest_dir()
  dest <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = dest, case = "aeme", ncore = 1,
                              exe = "pestpp-ies", ies_num_reals = 3,
                              prior_par_ensemble = src$d)
  param <- nf_cols(carry_dest_param())
  pt <- pest_param_table(param)
  ot <- make_obs_tbl()

  ctrl2 <- aemetools:::.pest_setup_ensembles(ctrl, par_tbl = pt, obs_tbl = ot,
                                             param = param)

  expect_equal(ctrl2$pestpp_options$ies_parameter_ensemble, "prior_par_en.csv")
  f <- file.path(dest, "prior_par_en.csv")
  expect_true(file.exists(f))
  en <- utils::read.csv(f, check.names = FALSE)
  expect_equal(nrow(en), 3)
  expect_equal(ctrl2$ies_num_reals, 3)
  expect_setequal(names(en), c("real_name", pt$parnme))
})

test_that("a source ensemble smaller than ies_num_reals warns and shrinks", {
  src <- make_pest_dir()
  param <- nf_cols(carry_dest_param())
  pt <- pest_param_table(param)

  expect_warning(
    out <- aemetools:::.pest_carry_ensemble(source = src$d, par_tbl = pt,
                                            param = param, n = 50),
    "fewer than"
  )
  expect_equal(nrow(out), 4)                # real_0, real_1, real_2, base
})
