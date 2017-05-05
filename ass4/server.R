function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "method1" = read.csv("methods/method1.csv"),
           "method2" = read.csv("methods/method2.csv"),
           "method3" = read.csv("methods/method3.csv"),
           "method4" = read.csv("methods/method4.csv"),
           "method5" = read.csv("methods/method5.csv"),
           "method6" = read.csv("methods/method6.csv"),
           "method7" = read.csv("methods/method7.csv"),
           "method8" = read.csv("methods/method8.csv"),
           "method9" = read.csv("methods/method9.csv"),
           "method10" = read.csv("methods/method10.csv"))
  })
  
  
  output$table <- renderTable({
    datasetInput()
  })
  
  
  
  output$plot <- renderPlot({
    
    d <- datasetInput()
    
    query_m <- input$query
    
    if(query_m == "male"){
      cM <- table(truth=d$reference, prediction=d$prediction)
      sensitivity <- round ( cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
      specificity <- round ( cM[1, 1] / (cM[1, 1] + cM[1, 2]), digits = 2)
      precision <- round ( cM[2, 2] / (cM[1, 2] + cM[2, 2]), digits = 2)
      F1 <- round ( 2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
      # AUC evaluation
      library('ROCR')
      eval <- prediction(d$pred.score,d$reference)
      AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
    }
    else if (query_m == "female") {
      cM <- table(truth=d$reference, prediction=d$prediction)
      sensitivity <- round (cM[1, 1] / (cM[1, 2] + cM[1, 1]), digits = 2)
      specificity <- round (cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
      precision <- round (cM[1, 1] / (cM[1, 1] + cM[2, 1]), digits = 2)
      F1 <- round (2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
      # AUC evaluation
      library('ROCR')
      eval <- prediction(d$pred.score,d$reference)
      AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
    }
    
    
    plot(specificity,sensitivity)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
		 paste(input$dataset, '.csv', sep='') 
	 },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
}
