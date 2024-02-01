#' Run AEME in a Shiny app
#'
#' @inheritParams AEME::run_aeme
#'
#' @importFrom shiny shinyApp fluidPage sidebarLayout sidebarPanel mainPanel
#'  plotOutput actionButton radioButtons h1 h3 tableOutput renderUI observeEvent
#'  reactiveValues renderPlot renderTable sliderInput req validate
#'  need withProgress incProgress tabPanel tabsetPanel
#' @importFrom AEME configuration lake
#' @importFrom dplyr filter mutate n
#'
#' @return Launches shiny app
#' @export

run_aeme_shiny <- function(aeme_data, param, path, mod_ctrls) {
  require(shiny)
  require(AEME)

  # data("aeme_parameters")
  data("key_naming", package = "AEME")
  out_vars <- key_naming$name
  names(out_vars) <- key_naming$name_full
  out_vars <- out_vars[-1]
  out_vars <- grep("HYD|LKE|PHY|CHM|PHS|NIT", out_vars, value = TRUE)

  # param <- aeme_parameters
  cfg <- AEME::configuration(aeme_data)
  # Which models are not NULL in cfg
  models <- names(cfg)
  names(models) <- c("DYRESM-CAEDYM", "GLM-AED", "GOTM-WET")
  idx <- sapply(names(cfg), \(x) !is.null(cfg[[x]][["hydrodynamic"]]))
  models <- models[idx]
  param <- param |>
    dplyr::mutate(id = 1:dplyr::n())

  shiny::shinyApp(
    ui = shiny::fluidPage(
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
          shiny::uiOutput("param"),
          uiOutput('module_tabs')
        ),
        shiny::mainPanel(
          shiny::plotOutput("plot", height = "600px"),
          shiny::actionButton("build", "Build!"),
          shiny::actionButton("run", "Run!"),
          shiny::radioButtons("out_var", "Output variable:", out_vars,
                              selected = "HYD_temp", inline = TRUE),
          shiny::h3("Parameters"),
          shiny::tableOutput("table")
        )
      )
    ),
    server = function(input, output) {

      reac <- shiny::reactiveValues(df = NULL, aeme = aeme_data,
                                    update_tab = TRUE, tabs = NULL)

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

      output$module_tabs = renderUI({
        shiny::validate(shiny::need(!is.null(reac$tabs),
                                    "Please select a model"))
        reac$tabs
      })

      # Build AEME ----
      shiny::observeEvent(input$build, {
        shiny::withProgress(message = "Building AEME", value = 0, {
          shiny::incProgress(amount = 0.5)
          reac$aeme <- AEME::build_ensemble(path = path, aeme_data = reac$aeme,
                                            model = input$model,
                                            mod_ctrls = mod_ctrls,
                                            inf_factor = inf_factor,
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
            out <- run_aeme_param(aeme_data = reac$aeme,
                                  model = input$model,
                                  param = reac$df, path = path,
                                  mod_ctrls = mod_ctrls,
                                  na_value = 999, return_aeme = TRUE) |>
              AEME::output()
            out[[m]]
          })
          names(mod_res) <- input$model
          for (m in input$model) {
            outp[[m]] <- mod_res[[m]]
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
          dplyr::select(name, value, min, max, description)
      })

      # Plot model output ----
      output$plot <- shiny::renderPlot({
        shiny::validate(
          shiny::need(!is.null(input$out_var), "Please select an output variable")
        )
        shiny::validate(
          shiny::need(length(input$model) > 0, "Please select a model")
        )
        v <- AEME::get_var(aeme_data = reac$aeme, var_sim = input$out_var,
                           model = input$model)
        shiny::validate(
          shiny::need(any(!is.na(v[["value"]])),
                      "This variable is not available.")
        )

        outp <- AEME::output(reac$aeme)
        shiny::validate(
          shiny::need(input$out_var %in% names(outp[[input$model]]),
                      "This variable is not available.")
        )
        AEME::plot_output(aeme_data = reac$aeme, model = input$model,
                          var_sim = input$out_var, level = FALSE) +
          ggplot2::theme_bw(base_size = 16)
      }
      )
    }
  )
}
