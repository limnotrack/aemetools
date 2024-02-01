gotm_dir <- system.file("extdata/gotm_wet/", package = "AEME")

fabm_file <- file.path(gotm_dir, "fabm.yaml")

fabm <- yaml::read_yaml(fabm_file)
fabm_text <- readLines(fabm_file)

# Water column ----
wc <- "abiotic_water"

wc_pars <- names(fabm$instances$abiotic_water$parameters)
rem_pars <- c("diagnostics")
wc_pars <- wc_pars[!wc_pars %in% rem_pars]

out <- lapply(wc, \(w) {
  pars <- fabm$instances[[w]]$parameters
  df <- lapply(wc_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    value <- pars[[p]]
    data.frame(module = w, par = p,
               name = paste0("instances/", w, "/parameters/", p),
               description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

wc_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::filter(min != 0 & max != 0)

# Sediment ----

sed <- "abiotic_sediment"

sed_pars <- names(fabm$instances$abiotic_sediment$parameters)
rem_pars <- c("hypsograph", "diagnostics")
sed_pars <- sed_pars[!sed_pars %in% rem_pars]

out <- lapply(sed, \(s) {
  pars <- fabm$instances[[s]]$parameters
  df <- lapply(sed_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    value <- pars[[p]]
    data.frame(module = s, par = p,
               name = paste0("instances/", s, "/parameters/", p),
               description = desc, default = def, value)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    )
}) |>
  do.call(rbind, args = _)

sed_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::filter(min != 0 & max != 0)

# Phytoplankton ----
phy <- c("cyanobacteria", "diatoms", "greens")

phyto_pars <- names(fabm$instances$cyanobacteria$parameters)
rem_pars <- c("qLightMethod", "cLOptRef", "lSi", "hSiAss", "cSiD", "lNfix",
              "cNFixMax", "fMuNFix", "qTrans", "cVSwim", "fLVMmin",
              "fNutLimVMdown", "fNutLimVMup", "diagnostics")

phyto_pars <- phyto_pars[!phyto_pars %in% rem_pars]

out <- lapply(phy, \(ph) {
  pars <- fabm$instances[[ph]]$parameters
  df <- lapply(phyto_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    value <- as.numeric(pars[[p]])
    data.frame(module = ph, par = p,
               name = paste0("instances/", ph, "/parameters/", p),
               description = desc, default = def, value = value, fraction)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

phy_sa <- out |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())|>
  dplyr::select(-fraction)


# Zooplankton ----
zoop <- c("cladocerans")

zoop_pars <- names(fabm$instances$cladocerans$parameters)
rem_pars <- c("lSi1", "lSi2", "lSi3", "nPrey", "prey_suffix1", "prey_suffix2",
              "prey_suffix3", "qODB", "qTrans", "diagnostics" )
zoop_pars <- zoop_pars[!zoop_pars %in% rem_pars]

out2 <- lapply(zoop, \(zp) {
  pars <- fabm$instances[[zp]]$parameters
  df <- lapply(zoop_pars, \(p) {
    txt <- grep(p, fabm_text, value = TRUE) [1]
    desc <- strsplit(txt, "#")[[1]][2]
    def <- strsplit(desc, "default = ")[[1]][2] |>
      as.numeric()
    fraction <- grepl("fraction", desc)
    value <- pars[[p]]
    data.frame(module = zp, par = p,
               name = paste0("instances/", zp, "/parameters/", p),
               description = desc, default = def, value = value, fraction)
  }) |>
    do.call(rbind, args = _)
  df |>
    dplyr::mutate(
      min = default - (0.25 * abs(default)),
      max = default + (0.25 * abs(default))
    ) |>
    dplyr::mutate(
      min = dplyr::case_when(
        fraction ~ 0,
        .default = min
      ),
      max = dplyr::case_when(
        fraction ~ 1,
        .default = max
      )
    )
}) |>
  do.call(rbind, args = _)

zoop_sa <- out2 |>
  dplyr::mutate(
    model = "gotm_wet",
    file = "fabm.yaml"
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything()) |>
  dplyr::select(-fraction)

# Combine ----
gotm_wet_parameters <- rbind(
  wc_sa,
  sed_sa,
  phy_sa,
  zoop_sa
) |>
  dplyr::select(model, file, module, name, value, min, max, default,
                description, par) |>
  dplyr::mutate(
    module = dplyr::case_when(
      module == "cladocerans" ~ "zooplankton",
      module == "cyanobacteria" ~ "phytoplankton",
      module == "diatoms" ~ "phytoplankton",
      module == "greens" ~ "phytoplankton",
      TRUE ~ module
    )
  )

usethis::use_data(gotm_wet_parameters, overwrite = TRUE)
