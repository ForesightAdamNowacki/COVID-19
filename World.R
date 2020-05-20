#-------------------------------------------------------------------------------
# AUTOMATICALY ANALYSE WORLD SITUATION RELATED TO COVID-19:
#-------------------------------------------------------------------------------
# Prerequisites:
# Run file Data_Import.R to update data for analysis

#-------------------------------------------------------------------------------
# libraries:
base::library(tidyverse)
base::library(gridExtra)
base::library(readr)
base::library(lubridate)
base::library(stringr)
base::library(knitr)
base::library(ggmap)
base::library(viridis)
base::library(maps)

#-------------------------------------------------------------------------------
# Environment:
base::Sys.setenv(LANG = "en")
base::Sys.setlocale("LC_TIME", "C")

#-------------------------------------------------------------------------------
# Data
dates <- base::as.character(base::seq(from = lubridate::as_date("2020-01-22"), to = lubridate::as_date(Sys.Date()), by = 1))

# Confirmed:
confirmed <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_confirmed_global.csv", sep = "/"))
columns_number <- base::ncol(confirmed)
base::colnames(confirmed) <- base::c("Province_State", "Country", "Latitude", "Longitude", dates)[1:columns_number]
confirmed %>%
  dplyr::mutate(Latitude = NULL,
                Longitude = NULL) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("2020"),
                      names_to = "Date",
                      values_to = "Confirmed") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Confirmed = base::sum(Confirmed)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::mutate(New_Confirmed = Confirmed - dplyr::lag(Confirmed)) %>%
  tidyr::replace_na(base::list(New_Confirmed = 0)) -> confirmed

# Deaths:
deaths <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_deaths_global.csv", sep = "/"))
columns_number <- base::ncol(deaths)
base::colnames(deaths) <- base::c("Province_State", "Country", "Latitude", "Longitude", dates)[1:columns_number]
deaths %>%
  dplyr::mutate(Latitude = NULL,
                Longitude = NULL) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("2020"),
                      names_to = "Date",
                      values_to = "Deaths") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Deaths = base::sum(Deaths)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::mutate(New_Deaths = Deaths - dplyr::lag(Deaths)) %>%
  tidyr::replace_na(base::list(New_Deaths = 0)) -> deaths

#-------------------------------------------------------------------------------
# Recovered:
recovered <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_recovered_global.csv", sep = "/"))
columns_number <- base::ncol(recovered)
base::colnames(recovered) <- base::c("Province_State", "Country", "Latitude", "Longitude", dates)[1:columns_number]
recovered %>%
  dplyr::mutate(Latitude = NULL,
                Longitude = NULL) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("2020"),
                      names_to = "Date",
                      values_to = "Recovered") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Recovered = base::sum(Recovered)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date)) %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::mutate(New_Recovered = Recovered - dplyr::lag(Recovered)) %>%
  tidyr::replace_na(base::list(New_Recovered = 0)) -> recovered

#-------------------------------------------------------------------------------
# Europe
europe_countries <- base::c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
                            "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                            "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                            "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                            "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
                            "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro", 
                            "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                            "San Marino", "Serbia", "Slovakia", "Slovenia",
                            "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine",
                            "UK",
                            #"Russia",
                            "Vatican")
# length(europe_countries)                            
# 
# data_filtered$Country
# europe_map$Country %>% unique() %>% sort()
# 
# world_map <- map_data("world") 
# world_map$region %>% unique() %>% .[grep("Vatican", .)]
# 
# confirmed$Country %>% unique() 

europe_map <- ggplot2::map_data("world", region = europe_countries) %>%
  tibble::as_tibble() %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = base::ifelse(Country == "Czech Republic", "Czechia", Country),
                Country = base::ifelse(Country == "UK", "United Kingdom", Country),
                Country = base::ifelse(Country == "Macedonia", "North Macedonia", Country),
                Country = base::ifelse(Country == "Vatican", "Holy See", Country)); europe_map
  
europe_countries <- europe_map %>%
  dplyr::select(Country) %>%
  dplyr::distinct() %>%
  dplyr::pull(); europe_countries

data_filtered <- confirmed %>%
  dplyr::filter(Country %in% europe_countries) %>%
  dplyr::filter(Date == lubridate::as_date(base::Sys.Date() - 5)); data_filtered

europe_label <- europe_map %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(long = base::mean(long),
                   lat = base::mean(lat)); europe_label


europe_map %>%
  dplyr::left_join(data_filtered, by = "Country") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Confirmed), col = "white") +
  ggplot2::geom_label(mapping = ggplot2::aes(label = Country), data = europe_label)

europe_map %>%
  dplyr::left_join(data_filtered, by = "Country") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Confirmed), col = "white") +
  ggplot2::labs(x = "Longitude",
                y = "Latitude")













# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")



