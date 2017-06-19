#system("Rscript hw5_105753032.R -fold 5 -out performance.csv")


# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query min|max -files file1 file2 ... filen –out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-fold"){
    query_n<-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query fold :", query_n))
print(paste("files      :", out_f))

### Main

data <- read.csv("Archaeal_tfpssm.csv",header=F,na.strings='NULL')
#data$V5603[is.na(data$V5603)] <- 0
data <- data[,-5603]

# levels(data[,2])
# head(data[,5600:5603]) 
# custdata$gp <- runif(dim(custdata)[1])

library(plyr)
library(randomForest)

#k =  10 #Folds
k = as.integer(query_n)

set.seed(9487)
# sample from 1 to k, nrow times (the number of observations in the data)
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

trainErros <- c()
calErrors <- c()
testErrors <- c()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

#compute cal&test erro
myPred <- function(mymodel,dataset){
  prediction <- data.frame()
  testsetCopy <- data.frame()
  temp <- as.data.frame(predict(mymodel, dataset[,-2]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(dataset[,2]))
  result <- cbind(prediction, testsetCopy[, 1])
  names(result) <- c("Predicted", "Actual")
  for (i in 1:length(result$Actual)){
    if (result$Actual[i] == result$Predicted[i]) {
      result$Difference[i] <- 1
    } else {
      result$Difference[i] <- 0
    }
  }
  testErro <- count(result$Difference)[2,2]/sum(count(result$Difference)$freq)
  
  testErro
}


for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  dTrainAll <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  
  useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=1/ (k-1))>0 	# Note: 14 
  dCal <- subset(dTrainAll,useForCal)
  dTrain <- subset(dTrainAll,!useForCal)
  
  # run a random forest model
  mymodel <- randomForest(V2 ~ ., data = dTrain, ntree = 100,importance=TRUE
                          ,proximity=TRUE)  
  
  testErro <- myPred(mymodel,testset)
  calError <- myPred(mymodel,dCal)

  table.model <- mymodel$confusion
  trainErro <- sum(diag(table.model)/sum(table.model))
  
  trainErros <- c(trainErros,trainErro)
  calErrors <- c(calErrors,calError)
  testErrors <- c(testErrors,testErro)
  
  progress.bar$step()
}

set <- c("trainning","calibration","test")
accuracy <- c(mean(trainErros),mean(calErrors),mean(testErrors))

out_data<-data.frame("set"=set, "accuracy"=accuracy,stringsAsFactors = F)
write.csv(out_data, file=out_f, row.names = F, quote = F)

