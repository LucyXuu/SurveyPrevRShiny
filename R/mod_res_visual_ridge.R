#' res_visual_ridge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_res_visual_ridge_ui <- function(id) {
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
        h4("Subnational Posterior Density Plot")
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
             selectInput(ns("selected_method"), "Select Method",
                         choices = c("Area-level Model"= "FH", "Unit-level Model"="Unit"))
             #choices = c("Unit-level Model"="Unit"))
             
      ),
      column(4,
             selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
      )
    ),
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("model_fitted_text"))
             )
      )
    ),
    tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
    fluidRow(
      # Main panel on the left
      column(12,
             tabsetPanel(id = ns("plot_type"),
                         tabPanel("All Regions",
                                  div(style = "margin-top:15px;",
                                      fluidRow(
                                        column(6,
                                               div(style = " margin: auto;width: max(100%, 400px);float: left;",
                                                   uiOutput(ns("select_adm2"))
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        div(
                                          style = "width: min(98%, 1100px); margin-top: 10px;margin-left: 20px;margin-right: 10px;",
                                          uiOutput(ns("ridge_plot_all"))
                                        ),
                                        div( style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
                                             uiOutput(ns("download_button_ridge_all"))
                                        )
                                        
                                      )
                                  )
                         ),
                         tabPanel("Regions with Highest/Lowest Prevalence",
                                  div(style = "margin-top:15px;",
                                      fluidRow(
                                        column(4,
                                               selectInput(ns("num_region_each"),
                                                           "Number of highest/lowest regions",
                                                           choices = c(1:30),
                                                           selected= 8)
                                        ),
                                        column(4,
                                               selectInput(ns("selected_format"),
                                                           "Choose a plot style",
                                                           choices = c('Wide','Long'))
                                        ),
                                        column(4,
                                        )
                                      ),
                                      fluidRow(
                                        div(
                                          style = "width: min(98%, 1100px); margin-top: 10px;margin-left: 20px;margin-right: 10px;",
                                          plotOutput(ns("ridge_plot_extreme"),height = "auto")
                                        ),
                                        div( style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
                                             uiOutput(ns("download_button_ridge_extreme"))
                                        )
                                      ),
                                  )
                         )
             )
      )
    )
  )
}
    
#' res_visual_ridge Server Functions
#'
#' @noRd 
mod_res_visual_ridge_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_res_visual_ridge_ui("res_visual_ridge_1")
    
## To be copied in the server
# mod_res_visual_ridge_server("res_visual_ridge_1")
