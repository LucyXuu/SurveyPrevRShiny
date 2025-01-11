#' res_visual_scatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_res_visual_scatter_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    tags$head(
      # Custom CSS for styling
      tags$style(HTML("
      .button-container {
        display: flex;           /* Use flexbox to center the button */
        justify-content: center; /* Center button horizontally */
        width: max(50%, 600px);  /* Max width same as map */
        margin: 20px auto;       /* Centering the container itself horizontally */
      }
    "))
    ),
    
    div(class = "module-title",
        h4("Subnational Estimate Comparison - Scatter Plot")
    ),
    ## country, survey and indicator info
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
      )
    ),
    fluidRow(
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      ),
      column(4,
             selectInput(ns("selected_measure"), "Select Statistics",
                         choices = c("Mean"="mean",
                                     "Coefficient of Variation"= "cv",
                                     "Width of 95% Credible Interval"="CI.width"))
      )
    ),
    fluidRow(
      column(4,
             selectInput(ns("method_x"), "Select Method on X-axis",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      ),
      column(4,
             selectInput(ns("method_y"), "Select Method on Y-axis",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      )
      
    ),
    fluidRow(
      column(12,
             tags$h4("Scatter plot comparing estimates from fitted models for the same Admin level"),
             hr(style="border-top-color: #E0E0E0;"), # More subtle horizontal line
             shinyWidgets::materialSwitch(inputId = ns("Interactive_Ind"), label = "Interactive Plot Enabled",
                                          status = "success",value =T),
             
             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin-top: 20px;",
               uiOutput(ns("Plot_Canvas"))
               #leaflet::leafletOutput(ns("prev_map"))
             ),
             div( style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))
             )
      )
    )
  )
}
    
#' res_visual_scatter Server Functions
#'
#' @noRd 
mod_res_visual_scatter_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_res_visual_scatter_ui("res_visual_scatter_1")
    
## To be copied in the server
# mod_res_visual_scatter_server("res_visual_scatter_1")
