#' res_visual_prev_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_res_visual_prev_map_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyWidgets::chooseSliderSkin("Flat", color = "#b0c4de"),
    tags$head(
      tags$style(type = 'text/css', "#big_slider .irs-grid-text, #big_slider .irs-min,
      #big_slider .irs-max,#big_slider .irs-single {font-size: 14px;}"),
      
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
        h4("Subnational Results Mapping")
    ),
    
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
      )
    ),
    
    fluidRow(
      column(4,
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Direct Estimates"="Direct",
                                     "Area-level Model"= "FH", "Unit-level Model"="Unit"))
      ),
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),
    
    fluidRow(
      column(4,
             selectInput(ns("selected_measure"), "Select Statistics",
                         choices = c("Mean"="mean",
                                     "Coefficient of Variation"= "cv",
                                     "Width of 95% Credible Interval"="CI.width",
                                     "Exceedance Probability"="exceed_prob"))
      ),
      div(id = 'big_slider',
          column(4,
                 uiOutput(ns("choose_prob"))
          ))
    ),
    
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("text_display"))
             )
      )
    ),
    fluidRow(
      column(12,
             #tags$h4("Map for estimates from selected model"),
             hr(style="border-top-color: #E0E0E0;"), # More subtle horizontal line
             shinyWidgets::materialSwitch(inputId = ns("PrevmapType"), label = "Interactive Map Enabled",
                                          status = "success",value =T),
             
             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin-top: 20px;",
               uiOutput(ns("prev_map"))
               #leaflet::leafletOutput(ns("prev_map"))
             ),
             div( style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))
                  
                  #downloadButton(ns("dl"), "Download as HTML", icon = icon("download"),
                  #            class = "btn-primary")
             )
      )
    )
  )
}
    
#' res_visual_prev_map Server Functions
#'
#' @noRd 
mod_res_visual_prev_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_res_visual_prev_map_ui("res_visual_prev_map_1")
    
## To be copied in the server
# mod_res_visual_prev_map_server("res_visual_prev_map_1")
