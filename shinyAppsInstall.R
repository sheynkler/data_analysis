
rm(list = ls())

install.packages('rsconnect')
install.packages('shinyapps')
devtools::install_github("rstudio/shinyapps")
library(shiny)
library(rsconnect)
library(shinyapps)
rsconnect::setAccountInfo(name='daten', token='57E4BB18502891AF3F7155B313676652', secret='MrjVPvknz8S3rBVJ3GtDGCNDXxewJYelr8ICEQyN')


getwd()
deployApp()


