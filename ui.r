# source("requirement.R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome Page", tabName = "welcome", icon = icon("chart-area")),

    fileInput(inputId = "upload", label = "Upload data:", accept = c(".csv")),

    menuItem(" Data & Infos", tabName = "dashboard", icon = icon("info-circle")),

    menuItem(" Dynammic Visualization", icon = icon("chart-area"), tabName = "concordance"),

    menuItem(" Report Generation", icon = icon("file-alt"), tabName = "report_generate",
             badgeLabel = "new", badgeColor = "yellow")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome",
            br(),
            h1("Welcome on the Vaccine R&D Concordance App"),
            br(),
            h4("First, load you dataset and then go to the different tabs to visualize or analyze the data.")
    ),

    tabItem(tabName = "dashboard",
            tabBox(title = "Data",id = "tabset_descriptive analysis",width = "100%",height = "100%",
                   tabPanel("Dashboard",
                            fluidRow(
                              valueBoxOutput("NbColumns", width = 2),
                              valueBoxOutput("NbRows", width = 2)

                            ),
                            fluidRow(
                              box(title = "Summary", width = "100%",status = "primary",solidHeader = T, verbatimTextOutput("summary")
                                  # %>% shinycssloaders::withSpinner(color="#0dc5c1")
                              )
                            ),
                            fluidRow(
                              box(title = "Dataframe", width = "100%",status = "primary",solidHeader = T, DTOutput("head")
                                  # %>% shinycssloaders::withSpinner(color="#0dc5c1")
                              )
                            )
                   )
            )
    ),

    tabItem(tabName = "concordance",
            fluidRow(
              column(10,
                     box( title = "Concordance table", status = "primary", width = 4,
                          tableOutput('concordance_table')
                          #%>% shinycssloaders::withSpinner(color="#0dc5c1"),
                     ),
                     box(title = "GMT table", status = "primary", width = 4,
                         tableOutput('GMT_table')
                         #%>% shinycssloaders::withSpinner(color="#0dc5c1"),
                     ),
                     box(title = "Altman-Brand table", status = "primary", width = 4,
                         tableOutput('ABE_table')
                         #%>% shinycssloaders::withSpinner(color="#0dc5c1"),
                     )
              ),
              column(width = 2,
                     box(
                       width = NULL, status = "warning",title = "Parameters",
                       tags$div(style = "display: block;",
                         radioButtons(
                           inputId = "log",
                           label = "Log Base",
                           choices = c(2,10),
                           selected = 10,
                           inline = TRUE
                         )),
                       selectInput(
                         inputId = 'var.x.concordance',
                         label = 'Choose variable X',
                         choices = NULL
                       ),
                       selectInput(
                         inputId = 'var.y.concordance',
                         label = 'Choose variable Y',
                         choices = NULL
                       ),
                       #actionButton("update_filters", "Update filters"),
                       br()
                       #,
                       #downloadButton("RMD_report", "Generate Word report"),
                     )
              ),
              fluidRow(
                column(12,
                       tags$head(tags$style("
                     #concordance_plot_fix{height:700px !important;}
                     #concordance_plot{height:700px !important;}
                     ")),
                       tabBox(width = 10, id = "tab_box_reg",
                              title = "Concordance regression",
                              tabPanel("Image", "Standard image",
                                       plotOutput("concordance_plot_fix")
                                       #%>% shinycssloaders::withSpinner(color="#0dc5c1")
                              ),
                              tabPanel("Dynamic", "Dynamic and personalized image",
                                       plotlyOutput("concordance_plot")
                                       #%>% shinycssloaders::withSpinner(color="#0dc5c1")
                              )
                       )
                )
              ),
              fluidRow(
                column(12,
                       tags$head(tags$style("
           #altman_bland_plot_fix{height:700px !important;}
           #altman_bland_plot{height:700px !important;}
         ")),
                       tabBox(width = 10, id = "tab_box_altman",
                              title = "Altman-Bland plot",
                              tabPanel("Image", "Standard image",
                                       plotOutput("altman_bland_plot_fix")
                                       #%>% shinycssloaders::withSpinner(color="#0dc5c1")
                              ),
                              tabPanel("Dynamic", "Dynamic and personalized image",
                                       plotlyOutput("altman_bland_plot")
                                       #%>% shinycssloaders::withSpinner(color="#0dc5c1")
                              )
                       )
                )
              )
            )
    ),
    tabItem(tabName = "report_generate",
            fluidRow(
              column(width = 4,
                     box(title = "Report & Analysis Settings", width = "100%", id = "report_info_box",
                         div(style = "font-weight: bold; font-size: 18px; margin-bottom: 10px;", "Scientific Information"),
                         tags$div(style = "display: block;", radioButtons("log_base", "Log Base: (Mandatory)", choices = c(2, 10), inline = TRUE)),
                         tags$div(style = "display: block;", radioButtons("analysis_type", "Cutoff Parameter: (Mandatory)", choices = c("Cutoff", "No Cutoff"), inline = TRUE)),
                         tags$div(style = "display: block;", radioButtons("concordance_type", "Other criteria : (Mandatory)", choices = c("None","Fold difference", "Percentage difference"), inline = FALSE)),
                         conditionalPanel(
                           condition = "input.concordance_type == 'Percentage difference'",
                           radioButtons("percentage_difference", "Percentage difference: (Mandatory)", choices = c("20","25", "30"), inline = TRUE)
                         ),
                         conditionalPanel(
                           condition = "input.concordance_type == 'Fold difference'",
                           radioButtons("fold", "Fold: (Mandatory)", choices = c("2", "3"), inline = TRUE)
                         ),
                         textInput("unit", "Unit: (Mandatory)", placeholder = "Enter unit"),
                         div(style = "font-weight: bold; font-size: 18px; margin-top: 20px; margin-bottom: 10px;", "Report Information"),
                         textInput("name_project", "Name of the project: (Optionnal)", placeholder = "Project's Name"),
                         textInput("title_report", "Title of the report: (Optionnal)", placeholder = "Report's Title"),
                         #textInput("requester", "Requester: (Optionnal)", placeholder = "Requester's name"),
                         div(style = "font-weight: bold; margin-bottom: 5px;", "Context / Additional informations : (Optional)"),
                         tags$textarea(id = "assay_info",
                                       class = "form-control",
                                       placeholder = "Additional Information",
                                       style = "width: 100%; height: 100px;",
                                       maxlength = "500"),
                         span(id = "charCount", "0/500"),
                         tags$script(HTML("
                             $(document).on('input', '#assay_info', function() {
                               var len = $(this).val().length;
                               $('#charCount').text(len + '/500');
                             });
                           ")),
                         tags$div(
                           style = "display: block;",
                           radioButtons(
                             "report_format",
                             "Choose format to export report: (Mandatory)",
                             choices = c("Word", "PDF"),
                             inline = TRUE
                           )
                         ),
                         div(style = "text-align: center;",
                             downloadButton("RMD_report", "Create Report")
                         )
                     )
              ),
              column(width = 8,
                     box(title = "Concordances Information",width = "100%",
                         div(style = "text-align: center;",  # Center-aligns only the content in this div
                             uiOutput("summary_table"),
                             uiOutput("conditional_message"),
                             uiOutput("dynamic_buttons")
                         )
                     ),
                     div(id = "sci_param_container")  # Container for dynamic UI
              )

          )
    )
  )
)




dbHeader <- dashboardHeader(title = "Vx R&D Concordance",
                            tags$li(a(href = 'https://www.sanofi.ca/',
                                      img(src = "logo_sanofi.png",
                                          title = "Sanofi Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

ui <- dashboardPage(skin = "black",
                    dbHeader,
                    sidebar,
                    body
)

