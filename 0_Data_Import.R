#-------------------------------------------------------------------------------
# AUTOMATICALY IMPORT UPDATED DATA FROM SOURCE:
#-------------------------------------------------------------------------------
getwd()
#-------------------------------------------------------------------------------
# Libraries:
base::library(RCurl)
base::library(readr)
base::library(tidyverse)

#-------------------------------------------------------------------------------
# Data
confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed %>%
  readr::write_csv(path = base::paste(base::getwd(), "Data", "time_series_covid19_confirmed_global.csv", sep = "/"))

deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths %>%
  readr::write_csv(path = base::paste(base::getwd(), "Data", "time_series_covid19_deaths_global.csv", sep = "/"))

recovered <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered %>%
  readr::write_csv(path = base::paste(base::getwd(), "Data", "time_series_covid19_recovered_global.csv", sep = "/"))

confirmed_US <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
confirmed_US %>%
  readr::write_csv(path = base::paste(base::getwd(), "Data", "time_series_covid19_confirmed_US.csv", sep = "/"))

deaths_US <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
deaths_US %>%
  readr::write_csv(path = base::paste(base::getwd(), "Data", "time_series_covid19_deaths_US.csv", sep = "/"))

# ------------------------------------------------------------------------------
# https://github.com/ForesightAdamNowacki
