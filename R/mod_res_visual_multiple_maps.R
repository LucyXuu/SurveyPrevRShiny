#' res_visual_multiple_maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_res_visual_multiple_maps_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
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
      .shiny-input-container:not(.shiny-input-container-inline) {
         width: 700px;
         max-width: 100%;
        }

        .model-checkbox-table {
          width: 100%; /* Full width to contain the DataTable */
          max-width: 800px;
          margin: 0 auto; /* Center the table horizontally */
          float: left;
        }

        .model-checkbox-table .dataTable {
          font-size: 16px; /* Larger text for readability */
          width: 100% !important; /* Force the table to expand to the container width */
          table-layout: fixed; /* Equal column widths */
          border-collapse: collapse; /* For border styling */
        }

        /* Header and cells styling */
        .model-checkbox-table .dataTable th,
        .model-checkbox-table .dataTable td {
          border: 1px solid #ddd; /* Light grey border */
          text-align: center; /* Center alignment for text */
  max-width: 300px !important; /* Ensure cells are less than 300px in width */

        }

        /* Zebra striping for rows */
        .model-checkbox-table .dataTable tr:nth-child(even){background-color: #f2f2f2;}

        /* Column and row headers styling */
        .model-checkbox-table .dataTable thead th {
          background-color: #ADD8E6; /* Green background for column headers */
          color: white; /* White text for contrast */
        }

         .model-checkbox-table .dataTable tbody tr td:first-child,
        .model-checkbox-table .dataTable thead th:first-child {
          width: 20%; /* Increase the width of the row names */
        }

        .model-checkbox-table .dataTable td input[type='checkbox'],
        .model-checkbox-table .dataTable td input[type='radio'] {
          display: block;
          margin-top: 10px;
          padding-left:3px;
          display: flex !important; justify-content: center !important; align-items: center !important;
          /* Additional custom styles for checkboxes and radio buttons can go here */
        }
    "))
    ),
    
    div(class = "module-title",
        h4("Comparing Multiple Maps")
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
      # Main panel on the left
      column(12,
             tabsetPanel(
               tabPanel("Single Model: Comparing Statistics",
                        div(style = "margin-top:15px;margin-bottom:-10px",
                            fluidRow(
                              column(4,
                                     selectInput(ns("selected_method"), "Select Method",
                                                 choices = c("Direct Estimates"="Direct",
                                                             "Area-level Model"= "FH", "Unit-level Model"="Unit"))
                              ),
                              column(4,
                                     selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))
                              )
                            )
                        ),
                        tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
                        
                        fluidRow(
                          div(style = "display: flex; justify-content: start;
                                            width: min(1000px,100%); align-items: center;margin-bottom:-10px;",
                              # Checkbox Group
                              div(style = "flex-grow: 1; padding-right: 5px;margin-left:15px;",  # Reduced padding for closer alignment
                                  checkboxGroupInput(ns("selected_stats"), with_red_star("Select (Multiple) Statistics to Plot: "),
                                                     choices = c("Mean"="mean",
                                                                 "Coefficient of Variation"= "cv",
                                                                 "Width of 95% Credible Interval"="CI.width",
                                                                 "Exceedance Probability"="exceed_prob"),
                                                     inline = TRUE)
                              ),
                              
                              # Conditional Panel
                              div(id = 'big_slider',
                                  style = "flex-grow: 0; max-width: 300px;",  # Using max-width for better control
                                  uiOutput(ns("choose_thresh_1"))
                                  #conditionalPanel(
                                  #  condition = "input.selected_stats.includes('exceed_prob')",
                                  #   sliderInput("probLevel", "Probability Level:",
                                  #              min = 0, max = 1, value = 0.95, step = 0.01)
                                  #)
                              )
                          )),
                        tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")
                        fluidRow(
                          column(12,
                                 div(style = " margin: auto;float: left;",
                                     uiOutput(ns("single_model_text_display"))
                                 )
                          )
                        ),
                        
                        # Action Button at the bottom
                        fluidRow(
                          div(style = "display: flex;",
                              
                              ### toggle input for interactive map
                              div(style = "margin-top: 10px; margin-left: 15px;max-width: 250px;",
                                  shinyWidgets::materialSwitch(inputId = ns("mapType"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                                               status = "success",value =T)
                              ),
                              div(style = "margin-left: 15px;margin-right: 15px;",
                                  tags$div(style = "margin-top: 5px; font-size: 18px;", # Larger font size for better readability
                                           "Click ",
                                           tags$b(actionButton(ns("plot_single_model"), "Generate Plot",
                                                               style = "padding: 0px 3px 3px ;font-size: 18px;", class = "btn-primary")),
                                           " to produce plot and/or apply changes."
                                  )
                              )
                          )
                        ),
                        
                        fluidRow(
                          column(12,
                                 div(
                                   id = "map-container",
                                   style = "width: min(98%, 1000px); margin-top: 20px;margin-bottom: 10px;",
                                   uiOutput(ns("map_single_model"))
                                 )
                          )),
                        fluidRow(
                          column(12,
                                 div( style = "width: min(98%, 1000px); margin-top: 10px; display: flex; justify-content: center;",
                                      uiOutput(ns("download_single_model_ui"))
                                      
                                      #downloadButton(ns("dl"), "Download as HTML", icon = icon("download"),
                                      #            class = "btn-primary")
                                 )
                          )
                        )
                        
               ),
               tabPanel("Same Statistics Compared Across Models",
                        fluidRow(
                          div( style = "margin-top: 15px;margin-left:0px; ",
                               column(4,
                                      selectInput(ns("selected_measure_multiple_model"), "Select Statistics",
                                                  choices = c("Mean"="mean",
                                                              "Coefficient of Variation"= "cv",
                                                              "Width of 95% Credible Interval"="CI.width",
                                                              "Exceedance Probability"="exceed_prob"))
                               ),
                               div(id = 'big_slider',
                                   column(4,
                                          uiOutput(ns("choose_thresh_multiple_model"))
                                   ))
                          )),
                        div( style = "margin-top: -5px; ",
                             tags$hr(style="border-top-color: #E0E0E0;") # (style="border-top: 2px solid #707070;")
                        ),
                        fluidRow(
                          column(12,
                                 div(style = " margin: auto;float: left;",
                                     uiOutput(ns("text_display_multiple_model"))
                                 )
                          )
                        ),
                        fluidRow(
                          div(style = "width: 100%; max-width: 800px;margin-top:10px;",
                              column(12,
                                     div(DT::DTOutput(ns('select_model_table')), class = "model-checkbox-table"),
                              )
                          )),
                        
                        # Action Button at the bottom
                        fluidRow(
                          div(style = "display: flex;margin-top:10px;",
                              
                              ### toggle input for interactive map
                              div(style = "margin-top: 10px; margin-left: 15px;max-width: 250px;",
                                  shinyWidgets::materialSwitch(inputId = ns("mapType2"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                                               status = "success",value =T)
                              ),
                              div(style = "margin-left: 15px;margin-right: 15px;",
                                  tags$div(style = "margin-top: 5px; font-size: 18px;", # Larger font size for better readability
                                           "Click ",
                                           tags$b(actionButton(ns("plot_multiple_model"), "Generate Plot",
                                                               style = "padding: 0px 3px 3px ;font-size: 18px;", class = "btn-primary")),
                                           " to produce plot and/or apply changes."
                                  )
                              )
                          )
                        ),
                        fluidRow(
                          column(12,
                                 div(
                                   id = "map-container",
                                   style = "width: min(98%, 1000px); margin-top: 20px;margin-bottom: 10px;",
                                   uiOutput(ns("map_multiple_model"))
                                 )
                          )),
                        fluidRow(
                          column(12,
                                 div( style = "width: min(98%, 1000px); margin-top: 10px; display: flex; justify-content: center;",
                                      uiOutput(ns("download_multiple_model_ui"))
                                 )
                          )
                        )
               )
             )
      )
    )
  )
}
    
#' res_visual_multiple_maps Server Functions
#'
#' @noRd 
mod_res_visual_multiple_maps_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_res_visual_multiple_maps_ui("res_visual_multiple_maps_1")
    
## To be copied in the server
# mod_res_visual_multiple_maps_server("res_visual_multiple_maps_1")
