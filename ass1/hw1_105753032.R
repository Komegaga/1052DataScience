########################
# homework1 example
########################

# system("Rscript hw1_105753032.R -files test.1.csv -out result2.csv")

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R input", call.=FALSE)
} else {
  for(i in c(1:4) ){
    if (args[i] == "-out"){
      out <- args[ i+1 ]
    } else if (args[i] == "-files"){
      i_f <- args[ i+1 ]
    }
  } 
}



### i_f <- "test.1.csv"
data <- read.csv(i_f)

library(tools)
i_f <- file_path_sans_ext(i_f)

w <- round(max(data$weight), digits = 2)
h <- round(max(data$height), digits = 2)

x <- c("set", "weight", "height")
y <- c(i_f, w, h )

final <- rbind(x, y)

write.table(final, file = out , sep = ",", row.names = FALSE, col.names = FALSE)
