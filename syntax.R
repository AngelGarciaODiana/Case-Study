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




