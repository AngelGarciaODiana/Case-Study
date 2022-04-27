library(dplyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)


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







library(tidyverse)

df_0 <- read_csv("C:/Users/angel/Desktop/Case Study/Automotive_1.csv", quote = "\"",
                               col_types = cols(price = col_number(), 
                                                yearofregistration = col_number(), 
                                                powerps = col_number(), kilometer = col_number(), 
                                                monthofregistration = col_number(), 
                                                postalcode = col_number()))

problems <- problems()

unique <- data.frame(unique(problems$row))


df <- read.table(text=readLines(file.choose(), warn = FALSE), as.is = TRUE)

df_1 <- data.frame(str_split_fixed(df$V1, ",", 20))

df_2 <- gsub(pattern = "?", replacement = ",", df_1$X2, fixed = T)
df_2 <- do.call(rbind.data.frame, strsplit(df_2, split = ",", 2))


filter <- df_0 %>%
  filter(price != "NA")

filter <- filter %>%
  group_by(datecrawled, name, datecreated) %>%
  mutate(n = n()) %>% arrange(datecrawled, name, seller, 
                              offertype, price, abtest, 
                              vehicletype, yearofregistration, 
                              gearbox, powerps, model, kilometer, 
                              monthofregistration, fueltype, brand,
                              notrepaireddamage, datecreated,
                              nrofpictures, postalcode, lastseen, desc(n)) %>% 
  ungroup %>%
  distinct(datecrawled, name, seller, 
           offertype, price, abtest, 
           vehicletype, yearofregistration, 
           gearbox, powerps, model, kilometer, 
           monthofregistration, fueltype, brand,
           notrepaireddamage, datecreated,
           nrofpictures, postalcode, lastseen, .keep_all = TRUE) %>%
  select(-n)


library(skimr)


filter %>%
  skim()


da <- read.table( 
  text = readLines(file.choose(), warn = FALSE), 
  header = TRUE,  
  sep = ",",
  quote = "\"",
  na.strings = "NA")

da <- Automotive_1

summary(da)

sum(is.na(da$abtest))

filter(is.na(da$price))

ifelse(A$box == 6 & A$document == 75, "big", "other")




