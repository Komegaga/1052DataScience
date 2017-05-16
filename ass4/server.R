library(shiny)
library(gplots)
library(rsconnect)

function(input, output) {

  # datasetInput<- reactive({
  #     
  #     switch(input$dataset,
  #            "method1" = read.csv("methods/method1.csv"),
  #            "method2" = read.csv("methods/method2.csv"),
  #            "method3" = read.csv("methods/method3.csv"),
  #            "method4" = read.csv("methods/method4.csv"),
  #            "method5" = read.csv("methods/method5.csv"),
  #            "method6" = read.csv("methods/method6.csv"),
  #            "method7" = read.csv("methods/method7.csv"),
  #            "method8" = read.csv("methods/method8.csv"),
  #            "method9" = read.csv("methods/method9.csv"),
  #            "method10" = read.csv("methods/method10.csv"))
  #   
  # })

  # # for(i in 1:length(input$dataset[,1])){
  # #   lst[[i]] <- read.csv(input$dataset[[i, 'datapath']])
  # }
  pt1 <- reactive({
    if (!input$method1) return(NULL)
    
    d <- read.csv("methods/method1.csv")
    
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
    name <- "method1"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
  })
  
  pt2 <- reactive({
    if (!input$method2) return(NULL)
    d <- read.csv("methods/method2.csv")
    
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
    name <- "method2"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt3 <- reactive({
    if (!input$method3) return(NULL)
    d <- read.csv("methods/method3.csv")
    
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
    name <- "method3"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt4 <- reactive({
    if (!input$method4) return(NULL)
    d <- read.csv("methods/method4.csv")
    
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
    name <- "method4"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt5 <- reactive({
    if (!input$method5) return(NULL)
    d <- read.csv("methods/method5.csv")
    
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
    name <- "method5"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt6 <- reactive({
    if (!input$method6) return(NULL)
    d <- read.csv("methods/method6.csv")
    
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
    name <- "method6"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt7 <- reactive({
    if (!input$method7) return(NULL)
    d <- read.csv("methods/method7.csv")
    
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
    name <- "method7"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt8 <- reactive({
    if (!input$method8) return(NULL)
    d <- read.csv("methods/method8.csv")
    
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
    name <- "method8"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt9 <- reactive({
    if (!input$method9) return(NULL)
    d <- read.csv("methods/method9.csv")
    
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
    name <- "method9"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  pt10 <- reactive({
    if (!input$method10) return(NULL)
    d <- read.csv("methods/method10.csv")
    
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
    name <- "method10"
    data.frame(name = name,specificity = specificity,sensitivity = sensitivity)
    
  })
  
  output$table <- renderTable({
    
    rbind(pt1(),pt2(),pt3(),pt4(),pt5(),pt6(),pt7()
          ,pt8(),pt9(),pt10())
    
    
    # specificity <- c(pt1()$specificity,pt2()$specificity,pt3()$specificity,pt4()$specificity
    #                  ,pt5()$specificity,pt6()$specificity,pt7()$specificity,pt8()$specificity
    #                  ,pt9()$specificity,pt10()$specificity)
    # sensitivity <- c(pt1()$sensitivity,pt2()$sensitivity,pt3()$sensitivity,pt4()$sensitivity
    #                  ,pt5()$sensitivity,pt6()$sensitivity,pt7()$sensitivity,pt8()$sensitivity
    #                  ,pt9()$sensitivity,pt10()$sensitivity)
    # method <- c(pt1()$name,pt2()$name,pt3()$name,pt4()$name
    #                  ,pt5()$name,pt6()$name,pt7()$name,pt8()$name
    #                  ,pt9()$name,pt10()$name)
    # 
    # data.frame(name = method ,specificity = specificity,sensitivity = sensitivity)
  })
   
  # formulaText <- reactive({
  #   
  #   paste0("methods/",input$dataset,".csv")
  # })
   
  
  
  # output$table2 <- renderTable({
  #   rbind(pt1(),pt2())
  #   # method <- c(pt1()$name,pt2()$name,pt3()$name,pt4()$name
  #   #             ,pt5()$name,pt6()$name,pt7()$name,pt8()$name
  #   #             ,pt9()$name,pt10()$name)
  #   
  # })
  
  output$plot <- renderPlot({
   table <-rbind(pt1(),pt2(),pt3(),pt4(),pt5(),pt6(),pt7()
          ,pt8(),pt9(),pt10())
   # specificity <- c(pt1()$specificity,pt2()$specificity,pt3()$specificity,pt4()$specificity
   #                  ,pt5()$specificity,pt6()$specificity,pt7()$specificity,pt8()$specificity
   #                  ,pt9()$specificity,pt10()$specificity)
   # sensitivity <- c(pt1()$sensitivity,pt2()$sensitivity,pt3()$sensitivity,pt4()$sensitivity
   #                  ,pt5()$sensitivity,pt6()$sensitivity,pt7()$sensitivity,pt8()$sensitivity
   #                  ,pt9()$sensitivity,pt10()$sensitivity)
   # 
  if (length(table)==0) return(NULL)
   plot(table$specificity,table$sensitivity,
        main= "Specificity vs. Sensitivity",
        xlab= "specificity",
        ylab= "sensitivity",
        col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)
   
   text(table$specificity,table$sensitivity, labels=table$name, 
        cex= 0.7, pos = 1, offset = 0.2 )
    
    
  })
  
  
  # output$plot <- renderPlot({
  #   
  #   # sapply(sprintf('method%d', 1:10), function(id) {
  #   #   path <- paste0("methods/",id,".csv", sep="")
  #   #   d <- read.csv(path)
  #   # }
  #   
  #   d <- datasetInput()
  #   
  #   query_m <- input$query
  #   
  #   if(query_m == "male"){
  #     cM <- table(truth=d$reference, prediction=d$prediction)
  #     sensitivity <- round ( cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
  #     specificity <- round ( cM[1, 1] / (cM[1, 1] + cM[1, 2]), digits = 2)
  #     precision <- round ( cM[2, 2] / (cM[1, 2] + cM[2, 2]), digits = 2)
  #     F1 <- round ( 2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
  #     # AUC evaluation
  #     library('ROCR')
  #     eval <- prediction(d$pred.score,d$reference)
  #     AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
  #   }
  #   else if (query_m == "female") {
  #     cM <- table(truth=d$reference, prediction=d$prediction)
  #     sensitivity <- round (cM[1, 1] / (cM[1, 2] + cM[1, 1]), digits = 2)
  #     specificity <- round (cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
  #     precision <- round (cM[1, 1] / (cM[1, 1] + cM[2, 1]), digits = 2)
  #     F1 <- round (2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
  #     # AUC evaluation
  #     library('ROCR')
  #     eval <- prediction(d$pred.score,d$reference)
  #     AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
  #   }
  #   
  #   
  #   plot(specificity,sensitivity)
  # }) 
  
  
}
