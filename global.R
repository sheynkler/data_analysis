rm(list = ls())
library(MASS)
library(stringr)
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



