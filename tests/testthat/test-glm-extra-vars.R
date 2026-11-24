skip_if_not(!is.null(tryCatch(AEME::glm_extra_vars, error = function(e) NULL)),
            "AEME without glm_extra_vars")

test_that("is_glm_extra_var recognises sediment variables only", {
  expect_equal(is_glm_extra_var(c("SEDL_Fsed_oxy", "SED_Fsed_oxy",
                                  "SED_Fsed_oxy_lake", "HYD_temp", "nope")),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("check_sa_extra_vars validates regions", {
  ok <- list(flux = list(var = "SEDL_Fsed_oxy", month = 1:12),
             zflux = list(var = "SED_Fsed_oxy", zone = 2),
             lflux = list(var = "SED_Fsed_oxy_lake"),
             stemp = list(var = "SED_temp", zone = 1, sed_layer = 2),
             temp = list(var = "HYD_temp", depth_range = c(0, 2)))
  expect_silent(check_sa_extra_vars(ok, model = "glm_aed"))

  # per-zone variable without a zone
  expect_error(check_sa_extra_vars(list(a_b = list(var = "SED_Fsed_oxy"))),
               "per sediment zone")
  # zone on a lake-wide / ordinary variable
  expect_error(check_sa_extra_vars(list(a_b = list(var = "SEDL_Fsed_oxy",
                                                  zone = 1))),
               "not per zone")
  expect_error(check_sa_extra_vars(list(a_b = list(var = "HYD_temp", zone = 1))),
               "only apply to")
  # layer on a variable without layers
  expect_error(check_sa_extra_vars(list(a_b = list(var = "SED_Fsed_oxy",
                                                  zone = 1, sed_layer = 1))),
               "layer dimension")
  # bad zone
  expect_error(check_sa_extra_vars(list(a_b = list(var = "SED_Fsed_oxy",
                                                  zone = 1.5))),
               "positive integer")
  # _lake of a non-zone variable
  expect_error(check_sa_extra_vars(list(a_b = list(var = "SEDL_Fsed_oxy_lake"))),
               "no lake-wide")
  # not glm_aed
  expect_error(check_sa_extra_vars(ok, model = c("glm_aed", "gotm_wet")),
               "cannot be analysed")
  # depth_range is ignored with a warning
  expect_warning(check_sa_extra_vars(
    list(a_b = list(var = "SEDL_Fsed_oxy", depth_range = c(0, 2)))),
    "ignored")
})

test_that("create_sa_control / create_sen_control validate sediment regions", {
  expect_error(create_sa_control(N = 8, vars_sim = list(
    z_flux = list(var = "SED_Fsed_oxy"))), "per sediment zone")
  expect_no_error(create_sa_control(N = 8, vars_sim = list(
    z_flux = list(var = "SED_Fsed_oxy", zone = 1))))
  expect_error(create_sen_control(vars_sim = list(
    z_flux = list(var = "SED_Fsed_oxy"))), "per sediment zone")
})

test_that(".raf_extract_extra_var reduces extras to a time series", {
  dates <- as.Date("2020-01-01") + 0:3
  idx <- list(date_index = 1:4, dates = dates, depths = 0)
  zone_arr <- matrix(1:8, nrow = 2)             # 2 zones x 4 times
  lyr_arr <- array(1:16, dim = c(2, 2, 4))      # layers x zones x times
  fake <- function(nc, lake_dir, model, vars_sim, date_index, incl_fluxes,
                   extra_vars, ...) {
    expect_null(vars_sim)
    expect_false(incl_fluxes)
    switch(extra_vars,
      SEDL_Fsed_oxy = list(SDF_Fsed_oxy = c(1, 2, 3, 4)),
      SED_Fsed_oxy = list(sediment = list(SDF_Fsed_oxy_Z = AEME:::new_grouped_var(
        zone_arr, c("nzones", "time"), list(nzones = 1:2, time = dates)))),
      SED_Fsed_oxy_lake = list(sediment = list(SDF_Fsed_oxy_Z_lake = c(9, 8, 7, 6))),
      SED_temp = list(sediment = list(sed_temp = AEME:::new_grouped_var(
        lyr_arr, c("sed_layers", "nzones", "time"),
        list(sed_layers = 1:2, nzones = 1:2, time = dates)))),
      SED_hflux = list())
  }
  testthat::local_mocked_bindings(read_model_outputs = fake, .package = "AEME")
  ex <- function(v, ...) .raf_extract_extra_var(v, idx = idx, nc = NULL,
                                                lake_dir = ".", model = "glm_aed",
                                                name = "r_1", ...)

  r <- ex("SEDL_Fsed_oxy")
  expect_equal(r$model, c(1, 2, 3, 4))
  expect_equal(r$Date, dates)
  expect_true(all(is.na(r$depth)))
  expect_equal(unique(r$name), "r_1")

  expect_equal(ex("SED_Fsed_oxy", zone = 2)$model, zone_arr[2, ])
  expect_equal(ex("SED_Fsed_oxy_lake")$model, c(9, 8, 7, 6))
  expect_equal(ex("SED_temp", zone = 2, sed_layer = 2)$model, lyr_arr[2, 2, ])
  expect_equal(ex("SED_temp", zone = 1)$model, lyr_arr[1, 1, ])

  # missing zone / out-of-range zone / variable absent from the file
  expect_null(suppressMessages(ex("SED_Fsed_oxy")))
  expect_null(suppressMessages(ex("SED_Fsed_oxy", zone = 3)))
  expect_null(suppressMessages(ex("SED_hflux", zone = 1)))
  # wrong model
  expect_null(suppressMessages(.raf_extract_extra_var(
    "SEDL_Fsed_oxy", idx = idx, nc = NULL, lake_dir = ".", model = "gotm_wet")))
})

test_that("simulation_zone_table records only zone-specific regions", {
  vs <- list(a_b = list(var = "HYD_temp", depth_range = c(0, 2)),
             c_d = list(var = "SED_Fsed_oxy", zone = 2),
             e_f = list(var = "SED_temp", zone = 1, sed_layer = 3),
             g_h = list(var = "SED_Fsed_oxy_lake"))
  z <- simulation_zone_table("sim_001", vs)
  expect_equal(z$variable, c("c_d", "e_f"))
  expect_equal(z$zone, c(2L, 1L))
  expect_equal(z$sed_layer, c(NA, 3L))
  expect_named(z, c("sim_id", "variable", "var", "zone", "sed_layer"))
  expect_equal(nrow(simulation_zone_table("s", vs[c(1, 4)])), 0)
})

# A minimal set of the tables a sensitivity read expects
.zone_fixture <- function(zone = TRUE) {
  out <- list(
    lake_metadata = data.frame(id = "L1", name = "lake"),
    simulation_metadata = data.frame(sim_id = "s_001", id = "L1"),
    function_metadata = data.frame(sim_id = "s_001", var = "x", fun = "f"),
    parameter_metadata = data.frame(sim_id = "s_001", name = "p"),
    simulation_data = data.frame(sim_id = "s_001", run = 1),
    sensitivity_metadata = data.frame(sim_id = "s_001", variable = "c_d"))
  if (zone) out$simulation_zone <- simulation_zone_table(
    "s_001", list(c_d = list(var = "SED_Fsed_oxy", zone = 2)))
  out
}

test_that("simulation_zone round-trips through csv and db, and is optional", {
  skip_if_not_installed("dbplyr")

  # csv
  d <- withr::local_tempdir()
  write_to_csv(.zone_fixture(), path = d, sim_id = "s_001", gen_n = 1,
               add_lake_meta = TRUE)
  ctrl <- list(method = "sa", file_type = "csv", file_dir = d)
  z <- read_simulation_output(ctrl = ctrl)$simulation_zone
  expect_equal(z$zone, 2)

  # db
  d2 <- withr::local_tempdir()
  write_to_db("r.db", path = d2, output = .zone_fixture(), add_lake_meta = TRUE)
  ctrl <- list(method = "sa", file_type = "db", file_dir = d2, file_name = "r.db")
  z <- read_simulation_output(ctrl = ctrl)$simulation_zone
  expect_equal(z$variable, "c_d")

  # a results file from before the table existed: reads without it, and a
  # later run that has one can still be appended to it
  d3 <- withr::local_tempdir()
  write_to_db("r.db", path = d3, output = .zone_fixture(zone = FALSE),
              add_lake_meta = TRUE)
  ctrl <- list(method = "sa", file_type = "db", file_dir = d3, file_name = "r.db")
  expect_false("simulation_zone" %in% names(read_simulation_output(ctrl = ctrl)))
  write_to_db("r.db", path = d3, output = .zone_fixture(), add_lake_meta = FALSE)
  expect_equal(nrow(read_simulation_output(ctrl = ctrl)$simulation_zone), 1)
})
