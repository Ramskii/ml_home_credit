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

dashboardPage(skin="red",
  dashboardHeader(title =tags$em("Dashboard Home credit",style="text-align:center;color:#F7F9F9;font-size:100%"),titleWidth = 800,
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "% Credit accepted "),
                               taskItem(value = 10, color = "aqua",
                                        "% Credit Failled"
                               )
                  )
                 
                  ),
  dashboardSidebar(width=150,
                   sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                     label = "Search..."),
                   sidebarMenuOutput("menu")
                   
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info",
              h2("Info"),
              br(),
              br(),
              br(),
              tags$h4("Home Credit propose des crédits à la consommation pour des personnes ayant peu ou pas du tout d'historique de prêt.", style="font-size:150%"),
              br(),
              tags$h4("Après avoir élaborer un modèle de scoring de la probabilité de défaut de paiement du client.", style="font-size:150%"),  
              br(),
              tags$h4("Pour être transparent avec le client vis-à-vis des décisions d'octroi de credit.", style="font-size:150%"),
              br(),
              tags$h4("Ce dashboard interactif vien expliquer les raisons qui nous poussent à refuser un crédit à un client.", style="font-size:150%"),
              br(),
              tags$h4(" Les données à disposition sont variées : données comportementales, données provenant d'autres institutions financières, etc.", style="font-size:150%"),
              br(),
              tags$h4("Nous allons nous basé sur",
                      tags$span("les variables importantes qui construisent notre modele",style="color:red"),
                      tags$span("Cliquer sur les autres onglets pour voir la donnée et le dashboard"), style="font-size:150%")
              ),
      
      tabItem(tabName = "data",
              fluidRow(
                column(width = 8,
                       tags$h4("Nous avons ici les données de nos clients et  nos futurs clients", 
                               style="font-size:200%"),
                       br(),
                       br()
                )),
              fluidRow(
                
                column(width = 7,
                       
                       valueBoxOutput("opponent_card"),
                       uiOutput("sample_train_ui",style="text-align:center;color:#117864;font-size:150%"),
                       DT::dataTableOutput('sample_train')
                ),
                column(width = 7,
                       infoBox("New Orders", nrow(application_test), icon = icon("credit-card")),
                       uiOutput("sample_test_ui",style="text-align:center;color:#1DDBE2 ;font-size:150%"),
                       DT::dataTableOutput('sample_test')
                )
            
              
              
      )),
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4,
                       box(
                         title = "Genre", width = NULL, status = "primary",
                         plotOutput("code_gender")
                       ),
                       box(
                         title = "Répartition de l'Age des clients n'ayant pas remboursé leur crédit", width = 8, solidHeader = TRUE, status = "primary",
                         plotOutput("age")
                       ),
                ),
                
                column(width = 4,
                       box(
                         status = "warning", width = NULL,
                         "Explication du refus de crédit :
                         Les clients jeunes ont plus de chances de ne pas rembourser leur crédit 
                         de mmême que ceux qui ont un score externe (score calculé en amont ) 1 et 2 petit.
                        Les revenus et le sexe ont un impact sur la probabilité de défaut."
                       ),
                       box(
                         title = "Score 3 externe", width = NULL, solidHeader = TRUE, status = "warning",
                         plotOutput("score_ext3")
                       ),
                       box(
                         title = "Score 3 & Score 2", width = NULL, background = "light-blue",
                         plotOutput("mixtescore_ext")
                       )
                ),
                
                column(width = 4,
                       box(
                         title = "Probabilité de défaut selon le revenu et le sexe (
Rente de prêt/Revenu du client )", width = NULL, solidHeader = TRUE,
                         plotOutput("Ratio")
                       ),
                       box(
                         title = "Education Type", width = NULL, background = "maroon",
                         plotOutput("NAME_EDUCATION_TYPE")
                       )
                )
              )
              
      )
    )
    
  )
)