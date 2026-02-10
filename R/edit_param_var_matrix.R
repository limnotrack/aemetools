#' Edit parameter–response matrix in a Shiny gadget
#'
#' @param param_var_matrix a named list of matrices, where each matrix corresponds to a model and has
#' parameters as rows and response variables as columns. This is the output of \code{get_param_var_matrix()}.
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
  
  models <- names(param_var_matrix)
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
      lapply(param_var_matrix, as.data.frame)
    )
    
    output$table <- rhandsontable::renderRHandsontable({
      shiny::req(input$model)
      
      rn <- rownames(rv()[[input$model]]) |>  
        decode_param_full() |> 
        dplyr::mutate(display_name = display_param_name(group, name, index)) |> 
        dplyr::pull(display_name)
      
      width <- max(nchar(rn)) * 7  # ~7px per character
      width <- max(200, min(width, 600))
      
      rhandsontable::rhandsontable(
        rv()[[input$model]],
        rowHeaders     = rn,
        rowHeaderWidth = width,
        useTypes       = TRUE
      )
    })
    
    shiny::observeEvent(input$table, {
      mats <- rv()
      mats[[input$model]] <-
        rhandsontable::hot_to_r(input$table)
      rv(mats)
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
  
  return(edited)
}
