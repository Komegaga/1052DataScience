########################
# homework1 example
########################

# system("Rscript hw1_105753032.R -files test.1.csv –out result.csv")

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R input", call.=FALSE)
} else if (length(args) > 1) {
  i_f <- args[2] 
}

i_f <- "test.1.csv"
data <- read.csv(i_f)

w <- round(max(data$weight), digits = 2)
h <- round(max(data$height), digits = 2)

x <- c("set", "weight", "height")
y <- c(i_f, w, h )

final <- rbind(x, y)

write.table(final, file = "test2.CSV", sep = ",", row.names = FALSE, col.names = FALSE)
