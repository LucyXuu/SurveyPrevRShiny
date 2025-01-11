#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  
  website_link <- "https://sae4health.stat.uw.edu"
  
  fluidPage(
    div(class = "welcome-note",
        h4("Welcome to the SurveyPrev - RShiny App !"),
        
        HTML(paste0(
          "<p style='font-size: large; margin-bottom: 20px;'>",
          "Our tool empowers hands-on analysis of health and demographic indicators in low- and middle-income countries (LMICs) at the subnational level.  <br> ",
          "Visit our ",
          tags$a("official website", href = paste0(website_link),
                 target = "_blank", class = "official-link"),
          " for detailed ",
          tags$a("instruction videos", href = paste0(website_link,"/overview/youtube_app_demo/"),
                 target = "_blank", class = "instruction-link"),
          ", ",
          tags$a("methodology descriptions", href = paste0(website_link,"/method/approach_overview/"),
                 target = "_blank", class = "instruction-link"),
          " and guidance on ",
          tags$a("results visualization and interpretation", href = paste0(website_link,"/gallery/visual_overview/"),
                 target = "_blank", class = "instruction-link"),
          ".",
          "<br>",
          "Follow the step-by-step instructions below to navigate through the application.",
          "<br>",
          "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
          "<strong>Expandable Instructions Boxes</strong>: ",
          "Click the button in the top-right corner of each box to expand or collapse its contents.",
          "</p>",
          "</p>",
          "<p style='font-size: small; text-align: right;margin-top:-10px'>", "Version 1.1.1 (2024-09-26)",
          "</p>"
        ))
    ),
    # colors
    # color_codes <- viridis::viridis(n=8,direction=-1)  # Generate 5 colors from the default Viridis palette
    # print((color_codes)[2:5])
    
    ### Step 0 prepare data
    div(
      class = "box0",
      shinydashboard::box(title = "Step 0 - Request Data",
                          status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          width = NULL,
                          tags$div(class="instructions",
                                   HTML(paste0(
                                     "<p style=' margin-bottom:10px;'>",
                                     "Before using the App, the user needs access to the required DHS survey data. ",
                                     "DHS maintains strict data access rules, particularly for sensitive GPS-tagged data needed for SAE analysis.</p> ",
                                     "<p style=''>",
                                     "Follow these steps to <strong>obtain access to DHS datasets</strong> (instruction videos also available ",
                                     tags$a("here", href = paste0(website_link,"/overview/youtube_data_request/"),
                                            target = "_blank", class = "official-link"), "):</p>",
                                     tags$ol(type="a",
                                             tags$li(tags$b("Register an account"),": Start by creating an account on the ",
                                                     tags$a("DHS website", href = "https://dhsprogram.com/Data/",
                                                            target = "_blank", class = "official-link"),
                                                     '.'
                                             ),
                                             tags$li(tags$b("Provide project information"),": Complete the forms with specific details about the project."),
                                             tags$li(tags$b("Request survey access"), ": Post-registration, formally request access to the country/year specific survey datasets. Be sure to request ",
                                                     tags$b("GPS data"), "; approval typically occurs within 48 hours.")
                                     ) #,
                                     #"<p>",
                                     #"Additionally, download <strong>WHO shapefiles</strong> from the WHO GIS Hub. Please reference the <strong>implementation guide</strong> for detailed instructions.</p>"
                                   ))
                          )
      )
    ),
    
    
    ### Step 1 specifying survey meta data
    div(
      class = "box1",
      shinydashboard::box(title = "Step 1 - Country/Survey Meta Data Specification",
                          status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          collapsed = T,
                          width = NULL,
                          tags$div(class="instructions",
                                   HTML(paste0(
                                     "<p style=' margin-bottom:10px;'>",
                                     "After obtaining access to the DHS data, the user may proceed to  ",
                                     actionButton(
                                       ns("switch_country_tab"),  # Button ID to trigger the modal
                                       "country specification panel ",
                                       style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"  # Larger font
                                     ),
                                     " to specify the meta data for the analysis, including: </p> ",
                                     tags$ul(
                                       tags$li(tags$b("Specify Country/Survey"), ": Select a country first; then, surveys available for that country will be displayed."),
                                       tags$li(tags$b("Choose an Indicator"),
                                               ": Indicators are organized by the chapters they appear within the DHS final report. For a detailed list of supported indicators, visit the",
                                               actionButton(
                                                 ns("switch_app_ind"),
                                                 "Tool Kit",
                                                 style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                               ),
                                               "tab."
                                       ),
                                       tags$li(tags$b("Select Administration Levels"),
                                               ": Choose the administrative levels (e.g., Admin-1, Admin-2) for your analysis."),
                                       tags$li(tags$b("Display Boundaries and Check Region Count"),
                                               ": Use our mapping tool to ensure the administrative regions match your analytical needs. Default boundary sources are GADM or WHO, depending on the app version. For custom boundaries, please contact us.")
                                     )
                                   ))
                          )
      )
    ),
    
    
    ### Step 2 data upload
    div(
      class = "box2",
      shinydashboard::box(title = "Step 2 - Data Upload",
                          status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          width = NULL,
                          collapsed = T,
                          tags$div(class="instructions",
                                   HTML(paste0(
                                     "<p>Access the ",
                                     actionButton(
                                       ns("switch_data_upload"),
                                       "Data Upload Panel",
                                       style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                     ),
                                     " after obtaining your DHS data (see instructional videos ",
                                     tags$a("here", href = paste0(website_link,"/overview/youtube_data_request/"),
                                            target = "_blank", class = "official-link"),
                                     ").</p>",
                                     tags$ul(
                                       tags$li(tags$b("Data Download"),': ',
                                               tags$ol(type="a",
                                                       tags$li("Log in at the",
                                                               tags$a("DHS website", href = "https://dhsprogram.com/Data/",
                                                                      target = "_blank", class = "official-link"),
                                                               "navigate to your pre-approved project, and select the country."),
                                                       tags$li("Follow the instructions on the data upload page to select the required Stata format recode data. Be sure to include ", tags$b("Geographic Data"), "."),
                                                       tags$li("Download all datasets in a zip file.")
                                               )
                                       ),
                                       tags$li(tags$b("Data Upload"),':',
                                               " Click ", tags$b("Data Upload"), ", select the downloaded .zip file, and submit.",
                                               "Once the data upload is complete (indicated by the green check marks on the upper right of the tab),",
                                               "the app will automatically prepare the dataset for analysis."
                                       ),
                                       tags$li(tags$b("Cluster Map"),
                                               ": Use this visualization tool to examine cluster density across Admin areas. Hover to see number of clusters and evaluate data sparsity issue."),
                                       tags$li(tags$b("Data Preview"),':',
                                               "The user may preview and download the prepared analysis dataset in .csv format.")
                                     )
                                   ))
                          )
      )
    ),
    
    
    ### Step 3 Model Fitting
    div(
      class = "box3",
      shinydashboard::box(title = "Step 3 - Model Fitting",
                          status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          width = NULL,collapsed = T,
                          tags$div(class="instructions",
                                   HTML(paste0(
                                     "<p>Before proceeding to ",
                                     actionButton(
                                       ns("switch_model_fitting"),
                                       "model fitting",
                                       style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                     ),
                                     ", we strongly recommend familiarizing yourself with the ",
                                     tags$a("methods", href = paste0(website_link,"/method/approach_overview/"),
                                            target = "_blank", class = "official-link"),
                                     " used in our analysis.</p>",
                                     "<p>Follow the steps below to carry out analysis:</p>",
                                     tags$ol(type="a",
                                             tags$li("Select from various model types across different administrative levels. Recommended models are listed for guidance."),
                                             tags$li(tags$b("Data Sparsity Check"),
                                                     ": to assess data sufficiency for model fitting. Results are categorized as:",
                                                     tags$ul(
                                                       tags$li(tags$b("Green check message", style="color: green;"), ": Data sufficient. Model is ready to be implemented."),
                                                       tags$li(tags$b("Orange warning message", style="color: orange;"), ": Potential data sparsity issue. Fitting the model is possible but advised against."),
                                                       tags$li(tags$b("Red warning message", style="color: red;"), ": Data sparsity check failed. Model fitting is not recommended and may result in errors.")
                                                     ),
                                                     "Note: Admin-2 level analysis typically lack sufficient data, requiring Small Area Estimation (SAE).",
                                                     "Contact us if you encounter non-meaningful error messages."
                                             ),
                                             tags$li("Following the data sparsity check, choose to ",
                                                     tags$ul(
                                                       tags$li(tags$b("Run Models that Passed Check"), " (conservative and recommended option) or "),
                                                       tags$li(tags$b("Run All Selected Models"), " (extra caution needed for results interpretation).")
                                                     )),
                                             tags$li("Model fitting statuses are:",
                                                     tags$ul(
                                                       tags$li(tags$b("Successful", style="color: green;"), ": The model is fitted successfully."),
                                                       tags$li(tags$b("Model not fitted", style="color: orange;"), ": The model was not chosen for fitting."),
                                                       tags$li(tags$b("Model will not be fitted", style="color: gray;"), ": The model fails the data sparsity check and will not be fitted.")
                                                     ))
                                     )
                                   ))
                          )
      )
    ),
    
    ### Step 4 - Result visualization and tabulation
    div(
      class = "box4",
      shinydashboard::box(title = "Step 4 - Results Visualization and Tabulation",
                          status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          collapsed = T,
                          width = NULL,
                          tags$div(class="instructions",
                                   HTML(paste0(
                                     "<p>Before using the visualization tools, we strongly recommend reviewing our comprehensive ",
                                     tags$a("visualization interpretation guide", href = paste0(website_link,"/gallery/visual_overview/"),
                                            target = "_blank", class = "official-link"),
                                     ". Access the tools under the ",
                                     actionButton(
                                       ns("switch_res_visual"),
                                       "results visualization tab",
                                       style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                     ),
                                     " to explore subnational mapping and other analytical plots.</p>",
                                     tags$ul(
                                       tags$li(tags$b("Single Map"),
                                               ": Map subnational estimates with four key statistics:.",
                                               tags$ul(
                                                 tags$li(tags$b("Mean"), ": Point estimates across regions."),
                                                 tags$li(tags$b("Coefficient of Variance"), ": Measure of relative variability among estimates."),
                                                 tags$li(tags$b("Width of 95% Credible Interval"), ": Reflects the uncertainty around the estimates."),
                                                 tags$li(tags$b("Exceedance Probability"), ": Probability that prevalence exceeds a certain threshold.")
                                               )
                                       ),
                                       tags$li(tags$b("Map Comparison"),
                                               ": Visualize different statistics from the same model or compare the same statistics across different models."),
                                       tags$li(tags$b("Scatter Plot"),
                                               ": Compare estimates from different models for the same admin level, and evaluate model performance."),
                                       tags$li(tags$b("Ridge Plot"),
                                               ": Compare full distribution of estimates across regions.")
                                     ),
                                     '<p> Tabulated results can be exported as .csv through ',
                                     actionButton(
                                       ns("switch_res_tab"),
                                       "result tabulation",
                                       style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                     ),
                                     '.</p>'
                                   ))
                          )
      ))
  #end of fluid page
  )
}
    
#' landing_page Server Functions
#'
#' @noRd 
#' 
mod_landing_page_server <- function(id,CountryInfo,AnalysisInfo,parent_session){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    observeEvent(input$switch_country_tab, {
      message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "country_spec")
    })
    
    ### switch to tab with indicator supported by our app
    observeEvent(input$switch_app_ind, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "indicator_in_app")
      shinyjs::js$activateTab("tool_kit")
    })
    
    
    
    observeEvent(input$switch_data_upload, {
      message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "data_upload")
    })
    
    observeEvent(input$switch_model_fitting, {
      #message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "model_fit")
    })
    
    observeEvent(input$switch_res_visual, {
      #message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "res_prev_map")
      shinyjs::js$activateTab("res_visual")
      
    })
    
    observeEvent(input$switch_res_tab, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "res_tab")
    })
    
  })
}

    
## To be copied in the UI
# mod_landing_page_ui("landing_page_1")
    
## To be copied in the server
# mod_landing_page_server("landing_page_1")
