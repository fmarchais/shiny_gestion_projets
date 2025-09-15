library(shiny)

ui <- dashboardPage(
  header = dashboardHeader(title = "Gestion de projets",titleWidth = 450),
  
  
  sidebar = dashboardSidebar(minified = F, 
                             collapsed = F,
                             uiOutput("sidebar_etudes"),
                             uiOutput("sidebar_swimmer"),
                             uiOutput("sidebar_filieres"),
                             uiOutput("sidebar_calendar")
                             
                             
                             
  ),
  body = dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel("Résumé des études",
                         column(2),
                         column(8, gt_output(outputId = "my_gt_table")),
                         column(2)    
                ),
                tabPanel("Gestion des projets",
                         textOutput("nrow_swimmer"),
                         plotOutput("swimmerplot")
                         
                ),
                
                tabPanel("Filières",
                         tags$div(
                           textOutput("plot_titre"),
                           style = "text-align: center; white-space: pre-line; font-size: 18px;"
                         ),
                         tags$br(),
                         plotlyOutput("plot", width = "100%"),
                         textOutput("n_row_pie") 
                ),
                tabPanel("Calendrier",
                         plotOutput("calendar_plot",
                                    height = "800")
                         )
                
    )
  ),
  controlbar = dashboardControlbar(width = 300,
                                   uiOutput("control_download_resume"), #### outputs conditionels, mettre ici les boutons pour télécharger
                                   uiOutput("control_download_swimmer")
  ),
  title = "Suivi des projets"
)