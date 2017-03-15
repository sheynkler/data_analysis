rm(list = ls())
library(MASS)
library(stringr)
library(cluster)

library(d3heatmap)
source("module/module.R")

dates <- data()
dates_data_frame <- as.data.frame(dates$results)

only_packages <- c("MASS", "datasets", "cluster")
dates_data_frame <- dates_data_frame[ dates_data_frame$Package %in% only_packages,]




dates_data_frame$Item <- str_split_fixed(dates_data_frame$Item, " ", 2)[,1]



t <- as.character(sapply(dates_data_frame$Item, function(x) class(eval(as.symbol(x)))))
dates_data_frame$class <- t

dates_data_frame <- dates_data_frame[ dates_data_frame$class == "data.frame",]
names_dates <- as.character(dates_data_frame$Item)

t <- sapply(dates_data_frame$Item, function(x) nrow(eval(as.symbol(x))))
dates_data_frame$nrow <- t

t <- sapply(dates_data_frame$Item, function(x) ncol(eval(as.symbol(x))))
dates_data_frame$ncol <- t

dates_data_frame <- dates_data_frame[ dates_data_frame$nrow >= 30 &  dates_data_frame$ncol <= 8,]
summary(dates_data_frame)
names_dates <- as.character(dates_data_frame$Item)
names_dates_mass <- as.character(dates_data_frame$Item[ dates_data_frame$Package == "MASS"])
names_dates_cluster <- as.character(dates_data_frame$Item[ dates_data_frame$Package == "cluster"])
names_dates_datasets <- as.character(dates_data_frame$Item[ dates_data_frame$Package == "datasets"])


#title_dates <- as.character(dates_data_frame$Title)
names_dates_datasets <- c("", names_dates_datasets)


#errorMassage <- data.frame(Sorry = "Not available")


num_classes <- c("integer", "numeric", "double")



lm_diagnostik_list <- c(
  "Residuals vs Fitted" = 1,
  "Normal Q-Q" = 2,
  "Scale-Location" = 3,
  "Cook's distance" = 4,
  "Residuals vs Leverage" = 5
)

based_cluster_list <- c("BIC",
                        "classification",
                        "uncertainty",
                        "density")

plot_korrelation <-
  c("circle", "pie", "color", "number", "square", "ellipse", "shade", "qgraph")
#“circle”, “square”, “ellipse”, “number”, “shade”, “color”, “pie”


first_text <- "Hi! This is a Data Analysis Page.<br><br>
You can explore your basic data statistics, and conduct cluster and regression analysis.<br>
First, you have to upload your data. <br>
You can also practice on preloaded data from R collection.<br>
This project is still in process, we are working on upgrades and extensions.<br> 
This is not a commercial project. We build this page on our free time, and it is open source.<br><br> Please send your suggestions to cheinkler@gmail.com.
"
metode_hierarchical_cluster <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty" , "median" , "centroid")


dist_method <- c("euclidean", "maximum", "manhattan", "canberra", "binary" , "minkowski")

mypalette <- palette()
mypalette[3] <- "green"