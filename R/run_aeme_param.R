#' Run AEME with parameter dataframe
#'
#' @inheritParams AEME::run_aeme
#' @inheritParams run_and_fit
#' @param na_value numeric; value to return if model run is unsuccessful
#' @param return_nc boolean; return netCDF file connection
#' @param return_aeme boolean; return AEME object
#'
#' @importFrom AEME run_aeme lake input observations outflows
#' @importFrom AEME read_nml write_nml set_nml
#' @importFrom AEME write_yaml
#' @importFrom yaml read_yaml
#' @importFrom ncdf4 nc_open nc_close
#'
#' @return `na_value` if model run is unsuccessful
#' @export

run_aeme_param <- function(aeme, param, model, path = ".",
                           model_controls = NULL,
                           na_value = 999, return_nc = FALSE,
                           return_aeme = FALSE, parallel = FALSE) {

  # Function checks ----
  if (!is.data.frame(param))
    stop("Parameter 'param' must be a data.frame.")
  if (!is.character(model))
    stop("Parameter 'model' must be a character string.")
  if (return_nc & return_aeme)
    stop("Only one of 'return_nc' and 'return_aeme' can be TRUE.")
  # if (length(model) != 1)
  #   stop("Only one model can be run at a time.")
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }

  # Load AEME data
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))
  inp <- AEME::input(aeme)
  obs <- AEME::observations(aeme)
  obs$lake$depth_mid <- (obs$lake$depth_to - obs$lake$depth_from) / 2

  for (m in model) {

    # Path for model
    model_path <- file.path(lake_dir, m)

    if (!is.null(obs$level)) {
      z_max <- mean(obs$level[["value"]]) - min(inp$hypsograph$elev)
    } else {
      z_max <- max(inp$hypsograph$elev) - min(inp$hypsograph$elev)
    }

    # Check model parameters in supplied parameters
    if (!m %in% param[["model"]])
      stop("No parameters in 'param' for ", m, ".")

    all_p <- param[param$model == m, ] # Subset parameters to model specific

    # Scale met data ----
    if ("met" %in% all_p[["file"]]) {

      # Read in meteo file ----
      met_idx <- which(param$model == m & param$file == "met")

      met <- inp$meteo

      z_max <- max(inp$hypsograph$elev) - min(inp$hypsograph$elev)

      # Apply scaling factors ----
      for (v in met_idx) {
        if (param$name[v] == "MET_wndspd") {
          met[["MET_wnduvu"]] <- met[["MET_wnduvu"]] * param[["value"]][v]
          met[["MET_wnduvv"]] <- met[["MET_wnduvv"]] * param[["value"]][v]
        } else {
          met[[param$name[v]]] <- met[[param$name[v]]] * param[["value"]][v]
        }
      }

      if(m == "glm_aed") {
        AEME:::make_metGLM(obs_met = met, path_glm = model_path,
                           use_lw = inp$use_lw)
      } else if(m == "gotm_wet") {
        AEME:::make_metGOTM(df_met = met, path.gotm = model_path,
                            return_colname = FALSE, lat = lke$latitude,
                            lon = lke$longitude)
      } else if(m == "dy_cd") {
        # lakename <- strsplit(basename(lake_dir), "_")[[1]][2]
        AEME:::make_DYmet(lakename = lakename, info = "test", obsMet = met,
                          filePath = model_path, infRain = FALSE, wndType = 0,
                          metHeight = 15, z_max = z_max, use_lw = inp$use_lw)
      }
    }

    # Scale wdr data ----
    if ("wdr" %in% all_p[["file"]]) {

      # Read in wdr data ----
      wdr_idx <- which(param$model == m & param$file == "wdr")
      col_id <- paste0(param$name[wdr_idx], "_", m)
      aeme_outf <- AEME::outflows(aeme)
      wdr <- aeme_outf[["data"]]
      for (c in names(wdr)) {
        if (c == "wbal") {
          # wdr[[c]] <- wdr[[c]][, c("Date", col_id)]
          wdr[[c]][[col_id]] <- wdr[[c]][[col_id]] * param[["value"]][wdr_idx]
        } else {
          wdr[[c]][["outflow"]] <- wdr[[c]][["outflow"]] *
            param[["value"]][wdr_idx]
        }
      }

      # wdr <- wdr[, c("Date", col_id)]
      # names(wdr)[2] <- "outflow"

      # if ("ampl" %in% all_p$name) {
      #   # wdr <- generate_wdr(model = model,
      #   #                     ampl = all_p$value[all_p$name == "ampl"],
      #   #                     wdr_factor = all_p$value[all_p$file == "wdr"],
      #   #                     hyps = hyps,
      #   #                     inf = inf,
      #   #                     obs.lvl = lvl,
      #   #                     obs.lake = obs,
      #   #                     obs.met = met, ext_elev = ext_elev, altitude = altitude, coeffs = coeffs)
      # } else {
      #   # Apply scaling factors ----
      #   # for(c in wdr_idx) {
      #   #   wdr[[param$name[c]]] <- wdr[[param$name[c]]] * param[["value"]][wdr_idx]
      #   # }
      # }

      if (m == "glm_aed") {
        AEME:::make_wdrGLM(outf = wdr, path_glm = model_path, update_nml = FALSE)
      } else if(m == "gotm_wet") {
        AEME:::make_wdrGOTM(outf = wdr, path_gotm = model_path, outf_factor = 1)
      } else if(m == "dy_cd") {
        AEME:::make_DYwdr(lakename = lakename, wdrData = wdr, filePath = model_path, info = "test")
        # make_DYwdr(lakename = lakename, wdrData = wdr, filePath = model_path, info = "built for calibration")
      }
    }

    # Inputting parameters ----
    if (m == "dy_cd") {
      cfg_file <- list.files(file.path(lake_dir, m),
                             pattern = "cfg")
      cfg_files <- c("dyresm3p1.par", cfg_file)
      for (f in cfg_files) {
        if (tools::file_ext(f) %in% param$file | f %in% param$file) {
          cfg_file <- file.path(lake_dir, m, f)
          cfg <- readLines(cfg_file)
          idx <- which(grepl(tools::file_ext(f), param$file))
          for (p in idx) {
            nme <- strsplit(param$name[p], "/")[[1]]
            lno <- as.numeric(nme[length(nme)])
            cmnt <- strsplit(trimws(cfg[lno]), "#")[[1]]
            cfg[lno] <- paste0(param$value[p], paste(" #", cmnt[2], collapse = " "))
          }
          writeLines(cfg, cfg_file)
        }
      }
    } else if (m == "glm_aed") {
      cfg_files <- c("glm3.nml", "aed2/aed2.nml", "aed2/aed2_phyto_pars.nml",
                     "aed2/aed2_zoop_pars.nml")
      for (f in cfg_files) {
        if (basename(f) %in% param$file) {
          idx <- which(param$file == basename(f))
          cfg_file <- file.path(lake_dir, m, f)
          nml <- AEME::read_nml(cfg_file)

          if (basename(f) %in% c("aed2_phyto_pars.nml", "aed2_zoop_pars.nml")) {
            aed_file <- file.path(lake_dir, m, "aed2/aed2.nml")
            aed <- AEME::read_nml(aed_file)
            grp <- ifelse(basename(f) == "aed2_phyto_pars.nml", "the_phytos",
                          "the_zoops")
            grp_idx <- AEME::get_nml_value(aed, grp)

            if (length(grp_idx) > 1) {
              wid <- tidyr::pivot_wider(param[idx, c("name", "value", "group")],
                                        names_from = "group",
                                        values_from = "value") |>
                as.data.frame()


              names <- strsplit(AEME::get_nml_value(nml, "pd%p_name"), ",")[[1]]

              grp_idx <- grep(paste0(substr(names(wid)[-1], 1, 4), collapse = "|"), names)

              names(grp_idx) <- names(wid)[-1]
              # param[, c("value", "par", "group")]
              grps <- unique(param$group[idx])
              arg_list <- lapply(1:nrow(wid), \(p) {
                par <- strsplit(wid$name[p], "/")[[1]][2]
                vals <- AEME::get_nml_value(nml, par)
                for (v in 2:ncol(wid)) {
                  if (!is.na(wid[p, v])) {
                    vals[grp_idx[v-1]] <- wid[p, v]
                  }
                }
                vals
              })
              names(arg_list) <- sapply(1:nrow(wid), \(p) {
                nme <- gsub("/", "::", wid$name[p])
              })
            } else {
              arg_list <- lapply(idx, \(p) {
                par <- strsplit(param$name[p], "/")[[1]][2]
                vals <- AEME::get_nml_value(nml, par)
                vals[grp_idx] <- param$value[p]
                vals
              })
              names(arg_list) <- sapply(idx, \(p) {
                nme <- gsub("/", "::", param$name[p])
              })
            }
          } else {
            pnames <- sapply(idx, \(p) {
              nme <- gsub("/", "::", param$name[p])
            })
            arg_list <- lapply(idx, \(p) {
              param$value[p]
            })
            names(arg_list) <- pnames
          }

          # Set and write nml file
          nml <- AEME::set_nml(nml, arg_list = arg_list)
          AEME::write_nml(nml, cfg_file)
        }
      }
    } else if (m == "gotm_wet") {
      cfg_files <- c("gotm.yaml", "fabm.yaml")
      for (f in cfg_files) {
        if (f %in% param$file) {
          cfg_file <- file.path(lake_dir, m, f)
          yaml <- yaml::read_yaml(cfg_file)
          idx <- which(param$file == f)
          pnames <- lapply(idx, \(p) {
            list(name = strsplit(param$name[p], "/")[[1]],
                 value = param$value[p])
            # nme[length(nme)]
          })
          for (i in pnames) {
            if (length(i[["name"]]) == 2) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]] <- i[["value"]]
            } else if (length(i[["name"]]) == 3) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]][[i[["name"]][3]]] <- i[["value"]]
            } else if (length(i[["name"]]) == 4) {
              yaml[[i[["name"]][1]]][[i[["name"]][2]]][[i[["name"]][3]]][[i[["name"]][4]]] <- i[["value"]]
            }
          }
          AEME::write_yaml(yaml, cfg_file)
        }
      }
    }
  }

  # Run model ----
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path,
                         check_output = FALSE, parallel = parallel,
                         model_controls = model_controls, return = return_aeme)


  # Check if model output is produced ----
  out_file <- dplyr::case_when(model == "dy_cd" ~
                                 file.path(lake_dir,
                                           model, "DYsim.nc"),
                               model == "glm_aed" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc"),
                               model == "gotm_pclake" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc"),
                               model == "gotm_wet" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc")
  )

  out_file_chk <-  !file.exists(out_file)
  if (any(out_file_chk)) {
    message("No ", out_file[out_file_chk], " present.")
    return(na_value)
  }

  if (return_nc) {
    nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
    return(nc)
  }

  if (return_aeme) {
    return(aeme)
  }
}
