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
  
  models <- unique(param_var_matrix[["model"]])
  stopifnot(length(models) > 0)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      title = "Edit parameter–response matrix"
    ),
    miniUI::miniContentPanel(
      shiny::radioButtons(
        inputId = "model",
        label   = "Model",
        choices = models,
        selected = models[1],
        inline  = TRUE
      ),
      rhandsontable::rHandsontableOutput("table")
    )
  )
  
  server <- function(input, output, session) {
    
    # store matrices for all models
    rv <- shiny::reactiveVal(
      param_var_matrix |> 
        dplyr::mutate(
          decode_param_full(name_full)
        )
    )
    
    output$table <- rhandsontable::renderRHandsontable({
      shiny::req(input$model)
      rv() |> 
        dplyr::filter(
          model == input$model
        ) |> 
        dplyr::select(-model, -name_full) |>
        dplyr::select(file, group, name, index, dplyr::everything()) |>
        rhandsontable::rhandsontable()
    })
    
    shiny::observeEvent(input$table, {
      shiny::req(input$model)
      edited_tbl <- rhandsontable::hot_to_r(input$table) |>
        dplyr::mutate(
          model = input$model,
          name_full = encode_param(group, name, index)
        )
      
      updated <- rv() |>
        dplyr::filter(model != input$model) |>
        dplyr::bind_rows(edited_tbl)
      
      rv(updated)
    })
    
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
  
  edited <- edited |> 
    dplyr::select(dplyr::all_of(colnames(param_var_matrix)))
  
  return(edited)
}
