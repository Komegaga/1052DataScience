#system("Rscript hw2_105753032.R --target male --files method1.csv method2.csv --out result.csv")

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


# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query min|max -files file1 file2 ... filen –out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))


# read files
names<-c()
sens<-c()
spes<-c()
F1s<-c()
AUCs<-c()

#files <- "method1.csv"
#query_m <- "male"

for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
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
    eval <- prediction(1 - d$pred.score,d$reference)
    AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
  
# AUC evaluation
#  library('ROCR')
#  eval <- prediction(d$pred.score,d$reference)
#  AUC <- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
  
  sens<-c(sens, sensitivity)
  spes<-c(spes, specificity)
  F1s<-c(F1s, F1)
  AUCs<-c(AUCs, AUC)
  names<-c(names,name)
}

# Sensitivity = the  true positive rate = recall = TP/(TP+FN)
# Specificity = the  true negative rate TN/(TN+FP)
# precision = TP/(TP+FP)
# F1 = 2*precision*recall/(precision+recall)
# AUC


out_data<-data.frame("method"=names, "sensitivity"=sens, "specificity"=spes, "F1"=F1s, "AUC"=AUCs, stringsAsFactors = F)
index<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], query_func, query_m=query_m)

# output file
out_data<-rbind(out_data,c("highest",names[index]))
write.csv(out_data, file=out_f, row.names = F, quote = F)
