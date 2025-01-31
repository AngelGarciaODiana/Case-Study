library(dplyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(skimr)
library(naniar)
library(dlookr)
library(visdat)
library(plotly)

# Import data
data<- read_csv(file.choose(),quote = "\"",
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

# Clean extra vars
rm(error_cases, char_vars, date_vars, num_vars)

# Replacing 0 to NA
data[data == 0] <- NA

# Fix outliers
boxplot(data$price)

out_rm <- data$price[!data$price %in% boxplot.stats(data$price)$out]

boxplot(out_rm)

data <- as.data.frame(data[!data$price %in% boxplot.stats(data$price)$out,])

data <- as.data.frame(data[!data$kilometer %in% boxplot.stats(data$kilometer)$out,])

rm(out_rm)

# Impute numeric NAs
plot_na_pareto(data)

plot_na_intersect(data)

#####Descriptive statistics
describe(data)

skim(data)
skim(eliminated)

## PYTHON
# split into input and output elements
#X, y = df[:, :-1], df[:, -1]
# split into train and test sets
#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=1)
# fit the model
#model = LinearRegression()
#model.fit(X_train, y_train)
# evaluate the model
#yhat = model.predict(X_test)
# evaluate predictions
#mae = mean_absolute_error(y_test, yhat)
# identify outliers in the training dataset
#lof = LocalOutlierFactor()
#yhat = lof.fit_predict(X_train)
# select all rows that are not outliers
#mask = yhat != -1
#X_train, y_train = X_train[mask, :], y_train[mask]
# fit the model
#model = LinearRegression()
#model.fit(X_train, y_train)
# evaluate the model
#yhat = model.predict(X_test)





