#' Run AEME in a Shiny app
#'
#' @inheritParams AEME::build_aeme
#' @inheritParams calib_aeme
#'
#' @importFrom shiny shinyApp fluidPage sidebarLayout sidebarPanel mainPanel
#'  plotOutput actionButton radioButtons h1 h3 tableOutput renderUI observeEvent
#'  reactiveValues renderPlot renderTable sliderInput req validate
#'  need withProgress incProgress tabPanel tabsetPanel uiOutput checkboxInput
#'  checkboxGroupInput selectInput
#' @importFrom AEME configuration lake
#' @importFrom dplyr filter mutate n
#'
#' @return Launches shiny app
#' @export

run_aeme_shiny <- function(aeme, param, path = ".", model_controls = NULL) {

  # data("aeme_parameters")
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  data("key_naming", package = "AEME")
  out_vars <- key_naming$name
  names(out_vars) <- key_naming$name_full
  out_vars <- out_vars[-1]
  out_vars <- grep("HYD|LKE|PHY|CHM|PHS|NIT", out_vars, value = TRUE)
  out_vars_aeme <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::select(var_aeme) |>
    dplyr::left_join(key_naming, by = c("var_aeme" = "name"))
  out_vars <- out_vars_aeme$var_aeme
  names(out_vars) <- out_vars_aeme$name_text

  diag_vars <- c("Chlorophyll", "Zooplankton", "Nitrogen", "Phosphorus")

  # Add cheats for shinytest2
  model <- NULL
  module <- NULL
  name <- NULL
  value <- NULL
  min <- NULL
  max <- NULL

  # param <- aeme_parameters
  cfg <- AEME::configuration(aeme)
  # Which models are not NULL in cfg
  models <- names(cfg)
  models <- models[!models %in% c("model_controls")]
  names(models) <- c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET")
  idx <- sapply(models, \(x) !is.null(cfg[[x]][["hydrodynamic"]]))
  models <- models[idx]
  param <- param |>
    dplyr::mutate(id = 1:dplyr::n(),
                  initial = value)

  # UI ----
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h1("Run AEME"),
        # checkboxGroupInput("model", "Model:",
        #                    models,
        #                    selected = models[1]
        #                    ),
        shiny::radioButtons("model", "Model:",
                            models,
                            selected = models[1]
        ),
        shiny::checkboxInput("use_bgc", "Use BGC", value = FALSE),
        shiny::actionButton("build", "Build!"),
        shiny::uiOutput("param"),
        shiny::uiOutput('module_tabs')
      ),
      shiny::mainPanel(

        shiny::tabsetPanel(
          id = "module_tabs",
          shiny::tabPanel("Time-series",
                          shiny::h3("Time-series"),
                          shiny::plotOutput("plot", height = "600px"),
                          shiny::radioButtons("out_var", "Output variable:",
                                              out_vars, selected = "HYD_temp",
                                              inline = TRUE)
                          ),
          shiny::tabPanel("Diagnostics",
                          shiny::h3("Diagnostics"),
                          shiny::plotOutput("diag_plot", height = "600px"),
                          shiny::radioButtons("diag_var", "Dianostic variable:",
                                              diag_vars, selected = diag_vars[1],
                                              inline = TRUE)
                          )
          ),
        shiny::actionButton("run", "Run!"),
        shiny::h3("Parameters"),
        shiny::tableOutput("table")
      )
    ),
    shiny::actionButton("exitButton", "Exit App")
  )

  # Server ----
  server <- function(input, output) {

    reac <- shiny::reactiveValues(df = NULL, aeme = aeme,
                                  update_tab = TRUE, tabs = NULL)

    # Exit button ----
    shiny::observeEvent(input$exitButton, {
      shiny::stopApp()
    })

    # Create tabs for each module ----
    shiny::observeEvent({
      input$model
      input$use_bgc
    }, {
      reac$df <- param |>
        dplyr::filter(model %in% input$model)
      if (!input$use_bgc) {
        reac$df <- reac$df |>
          dplyr::filter(module == "hydrodynamic")
      }
      print(reac$df)

      modules <- unique(reac$df$module)
      myTabs = lapply(modules, \(m) {
        sub <- reac$df[reac$df$module == m, ]
        sliders <- lapply(1:nrow(sub), function(i) {
          # print("sub")
          # print(sub[i, ])
          step <- (sub[i, "max"] - sub[i, "min"]) / 100
          shiny::sliderInput(paste0("param", sub[i, "id"]), sub[i, "name"],
                             min = sub[i, "min"], max = sub[i, "max"],
                             value = sub[i, "value"], step = step)
        })
        shiny::tabPanel(m,
                        sliders)
      })
      reac$tabs <- do.call(shiny::tabsetPanel, myTabs)
    })

    output$module_tabs = shiny::renderUI({
      shiny::validate(shiny::need(!is.null(reac$tabs),
                                  "Please select a model"))
      reac$tabs
    })

    # Build AEME ----
    shiny::observeEvent(input$build, {
      shiny::withProgress(message = "Building AEME", value = 0, {
        shiny::incProgress(amount = 0.5)
        reac$aeme <- AEME::build_aeme(path = path, aeme = reac$aeme,
                                          model = input$model,
                                          model_controls = model_controls,
                                          inf_factor = 1,
                                          ext_elev = 5,
                                          use_bgc = input$use_bgc)
        shiny::incProgress(amount = 0.9)
      })
    })

    # Run AEME ----
    shiny::observeEvent(input$run, {

      shiny::withProgress(message = "Running AEME", value = 0, {
        shiny::incProgress(amount = 0.5)
        outp <- AEME::output(reac$aeme)
        mod_res <- lapply(input$model, \(m) {
          out <- run_aeme_param(aeme = reac$aeme,
                                model = m,
                                param = reac$df, path = path,
                                model_controls = model_controls,
                                na_value = 999, return_aeme = TRUE) |>
            AEME::output()
          out[["ens_001"]][[m]]
        })
        names(mod_res) <- input$model
        for (m in input$model) {
          outp[["ens_001"]][[m]] <- mod_res[[m]]
        }
        AEME::output(reac$aeme) <- outp
        shiny::incProgress(amount = 0.9)
      })
    })

    # Update parameters ----
    shiny::observe({
      shiny::req(!is.null(input[[paste0("param", 1)]]) &
                   length(input$model) > 0)
      chk <- sapply(1:nrow(reac$df), \(i) !is.null(input[[paste0("param",
                                                                 reac$df[i, "id"])]]))
      shiny::req(all(chk))
      for (i in 1:nrow(reac$df)) {
        reac$df[i, "value"] <- input[[paste0("param", reac$df[i, "id"])]]
      }
    })

    # Update table ----
    output$table <- shiny::renderTable({
      reac$df |>
        dplyr::select(name, value, min, max, initial)
    })

    # Plot model output ----
    output$plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(!is.null(input$out_var), "Please select an output variable")
      )
      shiny::validate(
        shiny::need(length(input$model) > 0, "Please select a model")
      )
      v <- AEME::get_var(aeme = reac$aeme, var_sim = input$out_var,
                         model = input$model)
      shiny::validate(
        shiny::need(any(!is.na(v[["value"]])),
                    "This variable is not available.")
      )

      outp <- AEME::output(reac$aeme)
      shiny::validate(
        shiny::need(input$out_var %in% names(outp[["ens_001"]][[input$model]]),
                    "This variable is not available.")
      )
      AEME::plot_output(aeme = reac$aeme, model = input$model,
                        var_sim = input$out_var, level = FALSE) +
        ggplot2::theme_bw(base_size = 16)
    })

    # Plot diagnostic variable ----
    output$diag_plot <- shiny::renderPlot({

      if (input$diag_var == "Chlorophyll") {
        p1 <- AEME::plot_phytos(aeme = reac$aeme, model = input$model)
      } else if (input$diag_var == "Zooplankton") {
        p1 <- AEME::plot_zoops(aeme = reac$aeme, model = input$model)
      } else if (input$diag_var == "Nitrogen") {
        p1 <- AEME::plot_nit(aeme = reac$aeme, model = input$model)
      } else if (input$diag_var == "Phosphorus") {
        p1 <- AEME::plot_phs(aeme = reac$aeme, model = input$model)
      }
      p1 +
        ggplot2::theme_bw(base_size = 16)
    })
  }

  # Use a modal dialog as a viewr.
  # viewer <- shiny::dialogViewer("AEME", width = 1200, height = 600)
  # shiny::runGadget(ui, server, viewer = viewer)
  shiny::shinyApp(ui, server)
}
