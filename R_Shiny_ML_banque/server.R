library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(dplyr)
library(lightgbm)
library(DT)
load("lgb.rda")    # Load saved model



shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)   # Ceci est un nombre qui spécifie la taille maximale de la requête Web,
  # qui sert de limite de taille pour les téléchargements de fichiers.
  # Si elle n'est pas définie, la taille maximale de la demande est par défaut de 5 Mo.
  # La valeur que j'ai mise ici est de 80 Mo
  
  
  output$sample_input_data_heading = renderUI({   
    # afficher uniquement si les données ont été téléchargées
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      head(input_data)
    }
  })
  
  
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        
        #df_final= input_data %>% select(-Target,-Id_train)
        df_final= input_data %>% select(-Target,-Id_train)
        df_final2=data.matrix(df_final)
        prediction = predict(lgb.model2, data = df_final2)
        
        pred_target = factor(ifelse(prediction > 
                                      0.5, 1, 0))
        
        input_data_with_prediction = cbind(input_data,prediction,pred_target )
        input_data_with_prediction$Target = as.factor(input_data_with_prediction$Target )
        input_data_with_prediction
        
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({  
    # afficher uniquement si les données ont été téléchargées
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable({   # the last 6 rows to show
    pred = predictions()
    head(pred)
    
  })
  
  
  output$plot_predictions = renderPlot({   # the last 6 rows to show
    pred = predictions()
    cols <- c("1" = "red","0" = "blue")
    ggplot(pred, aes(x=EXT_SOURCE_3, y=EXT_SOURCE_2, color=factor(pred_target), shape=factor(Target))) +
      geom_point() +scale_colour_manual(values = cols,labels = c("Failed", "Passed"),name="Test Result")+
      scale_colour_manual(values = cols,labels = c("Failed", "Passed"),name="Test Result")+
      scale_shape_manual(values=c(3,17),labels = c("Failed", "Passed"),name="Official Result")
  })
  
  #output$sample_predictions = renderTable({   # the last 6 rows to show
   # pred = predictions()
    #head(pred)
    
  #})
  
  output$sample_predictions <- DT::renderDataTable(
    DT::datatable( predictions(), 
                   extensions = c("Scroller"),
                   options = list(
      scrollY = 200, scrollX = 400, scroller = TRUE,
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      columnDefs = list(list(className = 'dt-center', targets = 4)), 
      order = list(list(0, "desc"), list(1, "asc")),
      pageLength = 6))
  )
  
  output$kpi_prediction_heading = renderUI({  
    # afficher uniquement si les données ont été téléchargées
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Matrice de confusion',style="text-align:center;color:blue;font-size:150%")
    }
  })
  output$kpi_predictions = renderTable({   # the last 6 rows to show
    # Matrice de confusion
    pred = predictions()
    mat_confusion <- table(pred$pred_target, pred$Target)
    matrice2=data.frame(mat_confusion)
    colnames(matrice2) <- c("Prediction","Realiter","Nb")
    matrice2
  })


  
  
  output$opponent_card <- renderValueBox({
    # afficher uniquement si les données ont été téléchargées
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Taux d erreur',style="text-align:center;color:blue;font-size:150%")
    }
    pred = predictions()
    Tx_err <- function(y, ypred) {
      mc <- table(y, ypred)
      error <- (mc[1, 2] + mc[2, 1])/sum(mc)
      print(error)
    }
    Tx_err(pred$pred_target, pred$Target)
    
    opponent_prob <-     round(Tx_err(pred$pred_target, pred$Target)*100,1)
    
    valueBox(
      value = paste(opponent_prob, "%", sep = ""),
      subtitle = "Taux d'erreur",
      icon = icon("hand-rock"),
      width = 4,
      color = "aqua")
    
    
  })
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
})

