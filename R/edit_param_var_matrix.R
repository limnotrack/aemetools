#' Edit parameter–response matrix in a Shiny gadget
#'
#' @param param_var_matrix a data frame with columns 'model', 'file', 
#' 'name_full', and variable names as columns. The 'name_full' column 
#' should be encoded as 'group/name\[index\]'. The variable columns should 
#' contain TRUE/FALSE values indicating whether the parameter is associated with
#' the variable or not. This is the output of \code{get_param_var_matrix()}.
#'
#' @returns a named list of edited matrices, with the same structure as the 
#' input. If the user cancels the editing, the original input is returned.
#' @export
#' 
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny radioButtons observeEvent stopApp req
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r
#' @importFrom dplyr mutate pull
#'
#' @examples
#' \dontrun{
#' param <- aeme_parameters
#' vars_sim <- c("HYD_temp", "CHM_oxy", "PHY_tchla")
#' param_var_matrix <- create_param_var_matrix(param, vars_sim)
#' edited_matrix <- edit_param_var_matrix(param_var_matrix)
#' }

edit_param_var_matrix <- function(param_var_matrix) {
  
  # add temporary row ID for safe updating
  param_var_matrix$.row_id <- seq_len(nrow(param_var_matrix))
  
  models <- unique(param_var_matrix[["model"]])
  stopifnot(length(models) > 0)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Edit parameter–response matrix"),
    miniUI::miniContentPanel(
      
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(
            inputId = "model_filter",
            label   = "Model",
            choices = models,
            selected = models[1],
            inline  = TRUE
          )
        ),
        shiny::column(
          6,
          shiny::uiOutput("file_ui")
        )
      ),
      
      rhandsontable::rHandsontableOutput("table")
    )
  )
  
  server <- function(input, output, session) {
    
    rv <- shiny::reactiveVal(param_var_matrix)
    
    # update file choices based on model
    output$file_ui <- shiny::renderUI({
      files <- rv() |>
        dplyr::filter(model == input$model_filter) |>
        dplyr::pull(file) |>
        unique()
      
      shiny::selectInput(
        "file_filter",
        "File",
        choices = c("All", files),
        selected = "All"
      )
    })
    
    # reactive filtered table
    filtered_data <- shiny::reactive({
      shiny::req(input$model_filter, input$file_filter)
      df <- rv() |>
        dplyr::filter(model == input$model_filter)
      
      if (input$file_filter != "All") {
        df <- df |>
          dplyr::filter(file == input$file_filter)
      }
      
      df
    })
    
    # render rhandsontable
    output$table <- rhandsontable::renderRHandsontable({
      df <- filtered_data()

      rhandsontable::rhandsontable(df, stretchH = "all") |>
        rhandsontable::hot_col("model", readOnly = TRUE) |>
        rhandsontable::hot_col("file", readOnly = TRUE) |>
        rhandsontable::hot_col("name_full", readOnly = TRUE) |> 
        rhandsontable::hot_col(".row_id", readOnly = TRUE, width = 0.5)
    })
    
    # update reactive value on edit
    shiny::observeEvent(input$table, {
      edited <- rhandsontable::hot_to_r(input$table)
      if (is.null(edited)) return()
      
      df <- rv()
      
      # match by row_id to update correctly
      idx <- match(edited$.row_id, df$.row_id)
      df[idx, names(edited)] <- edited
      
      rv(df)
    })
    
    # Done / Cancel
    shiny::observeEvent(input$done, {
      shiny::stopApp(rv())
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
    
  }
  
  edited <- shiny::runGadget(ui, server)
  
  if (is.null(edited)) {
    return(param_var_matrix)
  }
  
  # remove .row_id before returning
  edited <- edited |> dplyr::select(-.row_id)
  
  # make sure columns match original
  edited <- edited |> dplyr::select(dplyr::all_of(colnames(param_var_matrix)))
  
  return(edited)
}

