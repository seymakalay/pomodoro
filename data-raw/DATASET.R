## code to prepare `DATASET` dataset goes here


#sample_data <- read.csv("C:\\Users\\seyma\\TP\\tp.R\\tp.R5 - Copy\\df.15.2.csv",
#                                          header = TRUE, sep = ",")

fac.cols <- c("political.afl","educ", "rural","region", "married","havejob","sex",#"class.of.HH",
              "Formal","Informal","L.Both" ,  "No.Loan","Loan.Type", "multi.level" ,"fin.intermdiaries", "fin.knowldge") #"Loan.Type",

sample_data[fac.cols] <- lapply(sample_data[fac.cols], factor)


sample_data$region  = relevel(sample_data$region, ref =  "3")
sample_data$Loan.Type = relevel(sample_data$Loan.Type, ref =  "No.Loan")

sample_data$multi.level = relevel(sample_data$multi.level, ref =  "zero")



#sample_data$Credit.Access <- sample_data$multi.level

usethis::use_data(sample_data, compress = "xz", overwrite = TRUE)


# line below was cut from sample_data.R
# #' @source \url{http://www.diamondse.info/}


##' sample_data$multi.level <- factor(sample_data$multi.level)
