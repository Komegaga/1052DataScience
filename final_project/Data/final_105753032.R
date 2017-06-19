#system("Rscript final_105753032.R -fold 5 -out performance.csv")
out_f <- "performance.csv"

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

data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",header=T,na.strings='NULL')

library(plyr)
library(randomForest)

k =  10 #Folds
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
  mymodel <- randomForest(Attrition ~ Age+ BusinessTravel+ DailyRate+ DistanceFromHome+Education+ EducationField+ EmployeeCount+EmployeeNumber+EnvironmentSatisfaction+Gender+HourlyRate+
                            JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+Over18+
                            OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears+
                            TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+id
                          , data = dTrain, ntree = 100,importance=TRUE,proximity=TRUE)
  #mymodel <- randomForest(Attrition ~ . , data = dTrain, ntree = 100,importance=TRUE,proximity=TRUE)
  
  testErro <- myPred(mymodel,testset)
  calError <- myPred(mymodel,dCal)
  
  table.model <- mymodel$confusion
  trainErro <- sum(diag(table.model)/sum(table.model))
  
  trainErros <- c(trainErros,trainErro)
  calErrors <- c(calErrors,calError)
  testErrors <- c(testErrors,testErro)
  
  write.csv(dTrainAll,paste0("dTrainAll_", i ,".csv") , row.names = F, quote = F)
  
  progress.bar$step()
}

# mymodel <- randomForest(Attrition ~ Age+ BusinessTravel+ DailyRate+ DistanceFromHome+Education+ EducationField+ EmployeeCount+EmployeeNumber+EnvironmentSatisfaction+Gender+HourlyRate+
#                         JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+Over18+
#                         OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StandardHours+StockOptionLevel+TotalWorkingYears+
#                         TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+id
#                         , data = dTrainAll, ntree = 100,importance=TRUE,proximity=TRUE)

set <- c("trainning","calibration","test")
accuracy <- c(round(mean(trainErros),digits = 2),round(mean(calErrors),digits = 2),round(mean(testErrors), digits = 2))
out_data_result <-data.frame("set"=set, "accuracy"=accuracy,stringsAsFactors = F)


files <- c()

for (i in 1:k) {
  file <- paste0("dTrainAll_", i ,".csv")
  files <- c(files,file)
}

names<-c()
sens<-c()
spes<-c()
F1s<-c()
AUCs<-c()

for(file in files){
  name<-gsub(".csv", "", basename(file))
  data<-read.table(file, header=T,sep=",")
  
  prob <-as.data.frame(predict(mymodel,data[,-2],type="prob"))
  prob_res <-as.data.frame(predict(mymodel,data[,-2],type="response"))
  result <- ifelse(prob$prediction=="Yes",prob$Yes,prob$No)
  prob$pred.score <- prob$Yes
  prob$reference <- data[,2]
  prob$prediction <- prob_res[,1]
  d <- prob[,c(-1:-2)]
  
  write.csv(d,paste0("methods/",file) , row.names = F, quote = F)
  
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

mytable <- function(mymodel,data){
  
  prob <-as.data.frame(predict(mymodel,data[,-2],type="prob"))
  prob_res <-as.data.frame(predict(mymodel,data[,-2],type="response"))
  # result <- ifelse(prob$prediction=="Yes",prob$Yes,prob$No)
  prob$pred.score <- prob$Yes
  prob$reference <- data[,2]
  prob$prediction <- prob_res[,1]
  d <- prob[,c(-1:-2)]
  d
  
}

query_func<-function(query_m, i)
{
  if(query_m == "male"){
    which.max(i)
  }
  else if (query_m == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}
query_m = "male"

out_data <-data.frame("method"=names, "sensitivity"=sens, "specificity"=spes, "F1"=F1s, "AUC"=AUCs, stringsAsFactors = F)
write.csv(out_data , file= "eff.csv", row.names = F, quote = F)
index<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], query_func, query_m=query_m)
out_data<-rbind(out_data,c("highest",names[index]))

write.csv(out_data_result , file= out_f, row.names = F, quote = F)
write.csv(out_data , file= "eff_list.csv", row.names = F, quote = F)
