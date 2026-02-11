#' Edit model parameters using a Shiny gadget
#' 
#' This function launches a Shiny gadget that allows users to edit model 
#' parameters in an interactive table. The parameters are filtered by model and 
#' file, and users can edit the values, minimums, maximums, and groups of the 
#' parameters. The edited parameters are returned as a data frame when the user 
#' clicks "Done". If the user clicks "Cancel", the original parameters are 
#' returned.
#' 
#' @inheritParams run_and_fit
#' 
#' @returns A data frame with the edited parameters. The structure of the data
#' frame is the same as the input, with columns for model, file, name, value, 
#' min, max, group and index. If the user cancels the editing, the original 
#' input is returned.
#' 
#' @export
#' 
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny selectInput uiOutput renderUI observeEvent stopApp req
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable
#' @importFrom dplyr filter pull mutate
#' 
#' @examples
#' \dontrun{
#' data("aeme_parameters", package = "AEME")
#' param <- aeme_parameters
#' edited_param <- edit_parameters_shiny(param)
#' }

edit_parameters_shiny <- function(param) {
  
  # UI ----
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit Model Parameters"),
    
    miniUI::miniContentPanel(
      
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(
          # shiny::selectInput(
            "model_filter",
            "Model",
            choices = unique(param$model),
            selected = unique(param$model)[1],
            inline = TRUE
          )
        ),
        shiny::column(
          6,
          shiny::uiOutput("file_ui")
        )
      ),
      
      rhandsontable::rHandsontableOutput("param_table")
    )
  )
  
  # Server ----
  server <- function(input, output, session) {
    
    param$.row_id <- seq_len(nrow(param))
    param_r <- shiny::reactiveVal(param)
    
    # update file choices based on model
    output$file_ui <- shiny::renderUI({
      file_choices <- param_r() |>
        dplyr::filter(model == input$model_filter) |>
        dplyr::pull(file) |>
        unique()
      
      shiny::selectInput(
        "file_filter",
        "File",
        choices = c("All", file_choices),
        selected = "All"
      )
    })
    
    # filtered table
    filtered_data <- shiny::reactive({
      shiny::req(input$model_filter, input$file_filter)
      
      df <- param_r() |>
        dplyr::filter(model == input$model_filter)
      
      if (input$file_filter != "All") {
        df <- df |>
          dplyr::filter(file == input$file_filter)
      }
      
      df
    })
    
    
    output$param_table <- rhandsontable::renderRHandsontable({
      
      rhandsontable::rhandsontable(
        filtered_data(),
        stretchH = "all"
      ) |>
        rhandsontable::hot_col("value", type = "numeric") |>
        rhandsontable::hot_col("min", type = "numeric") |>
        rhandsontable::hot_col("max", type = "numeric") |>
        rhandsontable::hot_col("group", type = "text") |>
        
        # make non-editable columns read-only
        rhandsontable::hot_cols(
          colWidths = 120
        ) |>
        rhandsontable::hot_col(
          col = setdiff(
            names(filtered_data()),
            c("value", "min", "max", "group")
          ),
          readOnly = TRUE
        ) |> 
        rhandsontable::hot_col(".row_id", readOnly = TRUE, width = 0)
    })
    
    # update reactive dataframe when edited
    shiny::observeEvent(input$param_table, {
      
      edited <- rhandsontable::hot_to_r(input$param_table)
      if (is.null(edited)) return()
      
      df <- param_r()
      
      # match on row_id instead of model/file
      idx <- match(edited$.row_id, df$.row_id)
      
      df[idx, ] <- edited
      param_r(df)
    })
    
    # Done
    shiny::observeEvent(input$done, {
      out_df <- param_r() |>
        dplyr::select(-.row_id)
      shiny::stopApp(out_df)
    })
    
    # Cancel
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(param)
    })
    
  }
  
  shiny::runGadget(app = ui, server = server)
}
