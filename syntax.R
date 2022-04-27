library(dplyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(skimr)

# Import data
data<- read_csv("C:/Users/angel/Desktop/Case Study/Automotive_1.csv",quote = "\"",
                col_types = cols(
                  .default = col_character()))

# Observations with wrong columns
error_cases <- data[grep("Angebot", data$seller),1:(ncol(data)-1)]

colnames(error_cases)[3:ncol(error_cases)] <- colnames(data)[4:ncol(data)]
error_cases$seller <- "privat"

# Corrected data frame
data <- rbind.data.frame(data[-(grep("Angebot", data$seller)),], error_cases)

char_vars <- c("name", "seller", "offertype",	"abtest", "vehicletype", "gearbox", "model", "fueltype", "brand", 
               "notrepaireddamage", "nrofpictures", "postalcode")

num_vars <- c("price", "yearofregistration","powerps","kilometer","monthofregistration")
data[num_vars] <- lapply(data[num_vars], function(x) as.numeric(as.character(x)))

date_vars <- c("datecrawled", "datecreated","lastseen")
data[date_vars] <- lapply(data[date_vars], function(x) as.Date(as.character(x)))


#####Descriptive statistics
describe(data)


# Missing values
skim(data)



ifelse(A$box == 6 & A$document == 75, "big", "other")




