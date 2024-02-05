# Grab from LER-WQ library ----
ler_wq <- read.csv("https://raw.githubusercontent.com/aemon-j/LakeEnsemblR.WQ/main/data/LakeEnsemblR_WQ_dictionary.csv")

glm_dir <- system.file("extdata/glm_aed/", package = "AEME")

aed_file <- file.path(glm_dir, "aed2.nml")
aed2_pars <- AEME::read_nml(aed_file)
aed2_phy_idx <- AEME::get_nml_value(aed2_pars, "the_phytos")


phy_file <- file.path(glm_dir, "aed2_phyto_pars.nml")
phy_pars <- AEME::read_nml(phy_file)

zoop_file <- file.path(glm_dir, "aed2_zoop_pars.nml")
zoop <- AEME::read_nml(zoop_file)
z_lines <- readLines(zoop_file)
strsplit(z_lines[5], "] - ")[[1]][2]

def_pars <- lapply(zoop$zoop_params, \(p) {
  p[[1]]
})
sel_pars <- c("zoop_param%zoop_initial", "zoop_param%min_zoo",
              "zoop_param%Rgrz_zoo", "zoop_param%fassim_zoo",
              "zoop_param%Kgrz_zoo", "zoop_param%theta_grz_zoo",
              "zoop_param%Rresp_zoo", "zoop_param%Rmort_zoo",
              "zoop_param%ffecal_zoo", "zoop_param%fexcr_zoo",
              "zoop_param%ffecal_sed", "zoop_param%theta_resp_zoo",
              "zoop_param%Tstd_zoo", "zoop_param%Topt_zoo",
              "zoop_param%Tmax_zoo", "zoop_param%saltfunc_zoo",
              "zoop_param%Smin_zoo", "zoop_param%Smax_zoo",
              "zoop_param%Sint_zoo", "zoop_param%INC_zoo", "zoop_param%IPC_zoo",
              "zoop_param%DOmin_zoo", "zoop_param%Cmin_grz_zoo")

zoo_pars <- lapply(sel_pars, \(p) {
  desc = strsplit(z_lines[grep(gsub("zoop_param%", "", p), z_lines)], "] - ")[[1]][2]
  data.frame(parameter = p, note = desc, default = def_pars[[p]], domain = "water")
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(module = "zooplankton",
                path = paste0("zoop_params/", parameter)) |>
  dplyr::mutate(parameter = gsub("zoop_param%", "", parameter),
                process = dplyr::case_when(
                  grepl("initial", note, ignore.case = TRUE) ~ "initial_conditions",
                  grepl("std", note, ignore.case = TRUE) ~ "standard",
                  grepl("resp|oxy|temp|fecal", note, ignore.case = TRUE) ~ "respiration",
                  grepl("graz", note, ignore.case = TRUE) ~ "grazing",
                  grepl("assim", note, ignore.case = TRUE) ~ "assimilation",
                  grepl("resp", note, ignore.case = TRUE) ~ "respiration",
                  grepl("mort", note, ignore.case = TRUE) ~ "mortality",
                  grepl("fexcr", note, ignore.case = TRUE) ~ "respiration",
                )
  )

# AED C, N, O, P ----
aed <- ler_wq |>
  dplyr::mutate(
    module = dplyr::case_when(
      module == "phosphorous" ~ "phosphorus",
      .default = module
    ),
    group = NA, file = "aed2.nml"
  ) |>
  dplyr::filter(model %in% c("aed2") &
                  module %in% c("carbon", "nitrogen", "oxygen",
                                "phosphorus")) |>
  dplyr::mutate(
    process = dplyr::case_when(
      grepl("frp_initial", parameter) ~ "initial_conditions",
      .default = process
    )
  )

head(aed)
tail(aed, 10)
# View(aed)

# Phytoplankton ----
phy <- c("cyanobacteria", "diatoms", "greens")

phy_pars <- lapply(phy, \(p) {

  sub <- ler_wq |>
    dplyr::mutate(
      parameter = dplyr::case_when(
        grepl("fT_method", parameter) ~ "fT_Method",
        .default = parameter
      )
    ) |>
    dplyr::mutate(
      path = dplyr::case_when(
        grepl("fT_Method", parameter) ~ paste0("phyto_data/pd%", "fT_Method"),
        .default = path
      )
    ) |>
    dplyr::filter(module == "phytoplankton" & model == "aed2") |>
    dplyr::filter(parameter != "p_name") |>
    dplyr::mutate(group = p, file = "aed2_phyto_pars.nml")

  phy_names <- AEME::get_nml_value(phy_pars, "pd%p_name") |>
    strsplit(",") |>
    unlist()
  idx <- grep(substr(p, 1, 4), phy_names)

  for (i in 1:nrow(sub)) {
    vals <- AEME::get_nml_value(phy_pars, paste0("pd%", sub$parameter[i]))
    sub$default[i] <- vals[idx]
  }
  sub
}) |>
  do.call(rbind, args = _)

# View(phy_pars)

# Zooplankton ----
zoop <- c("cladocerans")
zoo_pars <- zoo_pars |>
  dplyr::mutate(group = "cladocerans", file = "aed2_zoop_pars.nml",
                module = "zooplankton", default = as.character(default))

# zoo_pars <- lapply(zoop, \(p) {
#   sub <- ler_wq |>
#     dplyr::filter(module == "zooplankton" & model == "aed2") |>
#     dplyr::mutate(group = p, file = "aed2_zoop_pars.nml")
#   sub
# }) |>
#   do.call(rbind, args = _)

glm_aed_parameters <- dplyr::bind_rows(aed, phy_pars, zoo_pars) |>
  dplyr::mutate(default = as.numeric(default), value = default)

glm_aed_parameters <- glm_aed_parameters |>
  dplyr::mutate(
    model = "glm_aed",
    min = default - (0.25 * abs(default)),
    max = default + (0.25 * abs(default))
  ) |>
  dplyr::rename(
    name = path,
    par = parameter,
    description = note
  ) |>
  dplyr::select(model, file, name, value, min, max, dplyr::everything())
head(glm_aed_parameters)
tail(glm_aed_parameters)
glm_aed_parameters |>
  dplyr::filter(process == "initial_conditions")

glm_aed_parameters |>
  dplyr::filter(module == "phytoplankton")

glm_aed_parameters |>
  dplyr::filter(!par %in% param$par)

usethis::use_data(glm_aed_parameters, overwrite = TRUE)

