library(shiny)
library(dplyr)
library(gplots)
library(ggplot2)
library(rsconnect)
library(plotly)

function(input, output) {

  output$table2 <- renderTable({
    data <- paste0("./Data/methods/",input$content,".csv")
    d <- read.csv(data)
  })
  
  output$table <- renderTable({
    files <- input$datasets
    
    if(is.null(files)) return(NULL)
    
    out_data <- read.csv("./Data/eff.csv")
    
    out_data <- out_data[,c("method",input$x,input$y)]
    filter(out_data,method == files)
  })
 
 output$plot <- renderPlot({
    files <- input$datasets
    
    if(is.null(files)) return(NULL)
  
    names<-c()
    sens<-c()
    spes<-c()
    F1s<-c()
    AUCs<-c()

    for (file in files) {
      name <- file
      data <- paste0("./Data/methods/",file,".csv")
      d <- read.csv(data)

      cM <- table(truth=d$reference, prediction=d$prediction)
      sensitivity <- round ( cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
      specificity <- round ( cM[1, 1] / (cM[1, 1] + cM[1, 2]), digits = 2)
      precision <- round ( cM[2, 2] / (cM[1, 2] + cM[2, 2]), digits = 2)
      F1 <- round ( 2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
      # AUC evaluation
      library('ROCR')
      eval <- prediction(d$pred.score,d$reference)
      AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)

      sens<-c(sens, sensitivity)
      spes<-c(spes, specificity)
      F1s<-c(F1s, F1)
      AUCs<-c(AUCs, AUC)
      names<-c(names,name)
    }

    out_data<-data.frame("method"=files, "sensitivity"=sens, "specificity"=spes, "F1"=F1s, "AUC"=AUCs, stringsAsFactors = F)

    data <- out_data[,c(input$x,input$y)]
    
    # plot_ly(data , x = input$x, y = input$y )

    title <- paste0(input$x," vs. ",input$y )

    plot(data,
         main= title,
         xlab= input$x,
         ylab= input$y,
         col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

    text(data, labels=out_data$method,
         cex= 0.7, pos = 1, offset = 0.2 )
    
  })
 
   # output$plot2 <- renderPlotly({
   #   
   #   # files <- input$datasets
   #   # 
   #   # if(is.null(files)) return(NULL)
   #   # 
   #   # names<-c()
   #   # sens<-c()
   #   # spes<-c()
   #   # F1s<-c()
   #   # AUCs<-c()
   #   # 
   #   # for (file in files) {
   #   #   name <- file
   #   #   data <- paste0("./Data/methods/",file,".csv")
   #   #   d <- read.csv(data)
   #   #   
   #   #   cM <- table(truth=d$reference, prediction=d$prediction)
   #   #   sensitivity <- round ( cM[2, 2] / (cM[2, 1] + cM[2, 2]), digits = 2)
   #   #   specificity <- round ( cM[1, 1] / (cM[1, 1] + cM[1, 2]), digits = 2)
   #   #   precision <- round ( cM[2, 2] / (cM[1, 2] + cM[2, 2]), digits = 2)
   #   #   F1 <- round ( 2 *  precision * sensitivity / (precision + sensitivity), digits = 2)
   #   #   # AUC evaluation
   #   #   library('ROCR')
   #   #   eval <- prediction(d$pred.score,d$reference)
   #   #   AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
   #   #   
   #   #   sens<-c(sens, sensitivity)
   #   #   spes<-c(spes, specificity)
   #   #   F1s<-c(F1s, F1)
   #   #   AUCs<-c(AUCs, AUC)
   #   #   names<-c(names,name)
   #   # }
   #   # 
   #   # out_data<-data.frame("method"=files, "sensitivity"=sens, "specificity"=spes, "F1"=F1s, "AUC"=AUCs)
   #   out_data <- read.csv("./Data/eff.csv")
   #   
   #   x <- input$x
   #   
   #   y <- input$y
   #   
   #   plot_ly(out_data, x = out_data[,x] , y = out_data[,y] ,color = ~method)
   # })
  
}
