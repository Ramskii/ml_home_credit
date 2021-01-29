packages_to_use <- c("shiny", "shinydashboard","shinythemes","dplyr","ggplot2","readr","DT")


install_load <- function(packages){
  to_install <- packages[!(packages %in% installed.packages()[, "Package"])] # identify unavailable packages
  
  if (length(to_install)){  # install unavailable packages 
    install.packages(to_install, repos='http://cran.us.r-project.org', dependencies = TRUE)  # install those that have not yet been installed
  }
  
  for(package in packages){  # load all of the packges 
    suppressMessages(library(package, character.only = TRUE))
  }
}

install_load(packages_to_use) 
dashboardPage(skin="blue",
              dashboardHeader(title=tags$em("Shiny Home Credit prediction app", style="text-align:center;color:#0A2994;font-size:100%"),titleWidth = 800),
              
              dashboardSidebar(width = 200,
                               sidebarMenu(
                                 br(),
                                 menuItem(tags$em("Upload Test Data",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                 menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                                 
                                 
                               )
              ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName="data",
                          
                          
                          br(),
                          br(),
                          br(),
                          br(),
                          tags$h4("Avec cette application de prédiction, vous pouvez télécharger vos données et recupérer des préditions.
                                    Le modéle est une light gradient boosting machine LightGBM c'est un cadre de renforcement de gradient qui utilise des algorithmes d'apprentissage basés sur des arbres.
                                    En utilisant les données historiques le modèle va prédire si un demandeur sera en mesure de rembourser un prêt ou non.
                                    Il s'agit d'une tâche de classification supervisée standard.", style="font-size:150%"),
                          
                          
                          br(),
                          
                          tags$h4("Pour prédire grâce à l'utilisation de ce modéle, télécharger les données de test au format csv.", style="font-size:150%"),
                          
                          tags$h4("Then, go to the", tags$span("Download Predictions",style="color:red"),
                                  tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),
                          
                          br(),
                          br(),
                          br(),
                          column(width = 4,
                                 fileInput('file1', em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                           accept=c('.csv')),
                                 
                                 uiOutput("sample_input_data_heading"),
                                 tableOutput("sample_input_data"),
                                 
                                 
                                 br(),
                                 br(),
                                 br(),
                                 br()
                          ),
                          br()
                          
                  ),
                  
                  
                  tabItem(tabName="download",
                          fluidRow(
                            br(),
                            br(),
                            br(),
                            br(),
                            column(width = 8,
                                   tags$h4("Après avoir téléchargé un jeu de données de test, vous pouvez télécharger les prédictions au format csv en
                                    en cliquant sur le bouton ci-dessous.", 
                                           style="font-size:200%"),
                                   br(),
                                   br()
                            )),
                          fluidRow(
                            
                            column(width = 7,
                                   downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
                                   plotOutput('plot_predictions')
                            ),
                            column(width = 4,
                                   uiOutput("sample_prediction_heading"),
                                   #tableOutput("sample_predictions")
                                   DT::dataTableOutput('sample_predictions')
                            ),
                            column(width = 2,
                                   uiOutput("kpi_prediction_heading"),
                                   tableOutput("kpi_predictions")
                                   
                                   ),
                            valueBoxOutput("opponent_card")
                            )                            
                          ))
                ))























