install.packages("forecast")
install.packages("ggplot2")
install.packages("tidyverse")

library(forecast)
library(ggplot2)
library(tidyverse)

## Analysis 1: 2020 data (Jan 2020 to May 2020 daily data)

# Importing New York City COVID data

nyc_covid <- read.csv("nyc/nyc_covid.csv")

# Cleaning date columns and standardising

nyc_covid$DATE_OF_INTEREST <- strptime(nyc_covid$DATE_OF_INTEREST, "%m/%d/%y")
nyc_covid$DATE_OF_INTEREST <- format(nyc_covid$DATE_OF_INTEREST, "%Y-%m-%d")
nyc_covid$DATE_OF_INTEREST <- as.Date(nyc_covid$DATE_OF_INTEREST)

summary(nyc_covid)

# Importing New York City economy search topics data

nyc_economy <- read.csv("nyc/economy_topics.csv")

nyc_economy <- nyc_economy[,c(1, 4, 5)]

# Cleaning date columns and standardising

nyc_economy$Day <- as.Date(nyc_economy$Day, format = "%d/%m/%Y")

summary(nyc_economy)

# ARIMA for just unemployment variable and stock market variable (separately)

# Full credits for ARIMA/ARIMAX code for Analysis I and Analysis II goes to Kostiantyn Kravchuk from the following website: https://www.r-exercises.com/2017/05/05/forecasting-arimax-model-exercises-part-5-solutions/

nyc_unemployment_model <- auto.arima(nyc_economy$Unemployment...New.York.NY., seasonal = TRUE)

nyc_unemployment_predict <- forecast(nyc_unemployment_model)

autoplot(nyc_unemployment_predict)

accuracy(nyc_unemployment_predict)

nyc_stock_model <- auto.arima(nyc_economy$Stock.market...New.York.NY., seasonal = TRUE)

nyc_stock_predict <- forecast(nyc_stock_model)

autoplot(nyc_stock_predict)

accuracy(nyc_stock_predict)

# ARIMAX by correlating to COVID cases variable

# Doing a left-join to combine COVID and economic search topics data

nyc_combined <- merge.data.frame(nyc_economy, nyc_covid, by.x = "Day", by.y = "DATE_OF_INTEREST", all.x = TRUE)

nyc_combined_model <- auto.arima(nyc_combined$Unemployment...New.York.NY., xreg = nyc_combined$Cases, seasonal = TRUE)

accuracy(nyc_combined_model)

## Analysis 2: Year-long data (May 2019 to May 2020 weekly data)

# ARIMA for just unemployment variable

nyc_economy_year <- read.csv("yearlong_search/nyc_economy_11may2019_11may2020.csv")

colnames(nyc_economy_year) <- c("weekstartingon", "unemployment", "stockmarket")

nyc_economy_year$weekstartingon <- format(as.Date(nyc_economy_year$weekstartingon, format = "%d/%m/%Y"), "%Y-%U")

nyc_unemployment_year_model <- auto.arima(nyc_economy_year$unemployment, seasonal = TRUE)

nyc_unemployment_year_predict <- forecast(nyc_unemployment_year_model)

autoplot(nyc_unemployment_year_predict)

accuracy(nyc_unemployment_year_predict)

# ARIMAX by correlating to COVID cases variable

nyc_covid_aggregated <- nyc_covid

# Credits for date aggregating method: https://stackoverflow.com/questions/49756987/r-aggregate-by-week

nyc_covid_aggregated <- as.data.frame(
  nyc_covid_aggregated %>%
  group_by(week = format(DATE_OF_INTEREST, '%Y-%U')) %>%
  summarise_if(is.numeric, sum))

nyc_combined_year <- merge.data.frame(nyc_economy_year, nyc_covid_aggregated, by.x = "weekstartingon", by.y = "week", all.x = TRUE)

nyc_combined_year_model <- auto.arima(nyc_combined_year$unemployment, xreg = nyc_combined_year$Cases)

accuracy(nyc_combined_year_model)
