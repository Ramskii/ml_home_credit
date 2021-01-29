library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(questionr)
library(dplyr)
library(lightgbm)
library(DT)



server <- function(input, output) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("info"),tabName="info"),
      menuItem(tags$em("Data",style="font-size:120%"),icon=icon("table"),tabName="data"),
      menuItem(tags$em("Dashboard",style="font-size:120%"),icon=icon("dashboard"),tabName="dashboard",
               badgeLabel = "new", badgeColor = "green")
    )
  })
  output$opponent_card <- renderValueBox({

    repartition_target=round(prop.table(table(application_train$TARGET)),2)
    opponent_prob <-     round(repartition_target[1]*100,1)
    
    valueBox(
      value = paste(opponent_prob, "%", sep = ""),
      subtitle = "Taux de crédit rembourser dans notre base",
      icon = icon("hand-rock"),
      width = 4,
      color = "green")
    
    
  })
  output$sample_train_ui = renderUI({  

      tags$h4(    br(),
                  br(),
                  'Base de donnée client')
    
  })  
  output$sample_train <- DT::renderDataTable(
    DT::datatable( application_train, 
                   extensions = c("Scroller"),
                   options = list(
                     scrollY = 200, scrollX = 400, scroller = TRUE,
                     lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                     columnDefs = list(list(className = 'dt-center', targets = 4)), 
                     order = list(list(0, "desc"), list(1, "asc")),
                     pageLength = 6))
    
  )
  output$sample_test_ui = renderUI({  
    tags$h4('Base de donnée futur client')
    
  })  
  output$sample_test <- DT::renderDataTable(
    DT::datatable( application_test, 
                   extensions = c("Scroller"),
                   options = list(
                     scrollY = 200, scrollX = 400, scroller = TRUE,
                     lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                     columnDefs = list(list(className = 'dt-center', targets = 4)), 
                     order = list(list(0, "desc"), list(1, "asc")),
                     pageLength = 6))
    
  )
  output$age <- renderPlot({
    ggplot(base, 
           aes(x = YEARS_BIRTH_class,
               y = TARGET2)) + 
      geom_bar(stat = "identity", 
               fill = "indianred3", 
               color = "black") +
      geom_text(aes(label = pctlabel), 
                vjust = -0.25)  +
      labs(x = "age group (years)",
           y = "Failure to Repay(%)",
           title  = "Failure to Repay by Age group")
  })
  output$score_ext3 <- renderPlot({
    cols <- c("1" = "red","0" = "blue")   
    ggplot(application_train, aes(EXT_SOURCE_3, col = factor(TARGET))) +
      geom_density()+scale_colour_manual(values = cols,labels = c( "Passed","Failed"),name="Result")
  })
  output$mixtescore_ext <- renderPlot({
  cols <- c("1" = "red","0" = "blue")
  ggplot(application_train, aes(x=EXT_SOURCE_3, y=EXT_SOURCE_2, color=factor(Target), shape=factor(CODE_GENDER))) +
    geom_point() +scale_colour_manual(values = cols,labels = c("Failed", "Passed"),name="Test Result")+
    scale_colour_manual(values = cols,labels = c("Failed", "Passed"),name="Test Result")+
    scale_shape_manual(values=c(3,17,5),labels = c("F", "M","NA"),name="Gender")
  })
  output$code_gender <- renderPlot({
    ggplot(data = application_train, mapping = aes(x = factor(CODE_GENDER), fill = factor(Target)))+
      geom_bar(stat = "count")  +
      scale_fill_brewer(palette="Set1")+
      geom_text(stat = 'count', aes(label = ..count..), color = "black", size = 3.5)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle("Répartition en nombre des crédits selon le sexe ")
  })
  output$NAME_EDUCATION_TYPE <- renderPlot({
  ggplot(data = application_train, mapping = aes(x = NAME_EDUCATION_TYPE, fill = factor(Target)))+
    geom_bar(stat = "count") +
    scale_fill_brewer(palette="Set1")+
    geom_text(stat = 'count', aes(label = ..count..), color = "black", size = 3.5)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle("NAME_EDUCATION_TYPE")
  })
  output$Ratio <- renderPlot({
  application_train %>%
    filter(CODE_GENDER != "XNA") %>%
    mutate(CODE_GENDER = ifelse(CODE_GENDER == "F", "Female", "Male")) %>%
    mutate(Ratio = AMT_ANNUITY/AMT_INCOME_TOTAL) %>%
    select(CODE_GENDER, Ratio, TARGET) %>%
    filter(!is.na(TARGET)) %>%
    ggplot(aes(x=Ratio, y=TARGET)) +
    geom_smooth() + 
    scale_x_continuous("Debt/Incomes Ratio %", lim=c(0, 0.5), labels=percent_format()) +
    scale_y_continuous("Avg. Default Rate %", labels=percent_format(), breaks=seq(0, 1, 0.01)) +
    facet_wrap(~CODE_GENDER)
  })
}