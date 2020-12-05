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
base::library(gapminder)
base::library(gganimate)

#-------------------------------------------------------------------------------
# Environment:
base::Sys.setenv(LANG = "en")
base::Sys.setlocale("LC_TIME", "C")
base::options(scipen = 10000)

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
                      values_to = "Value") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Value = base::sum(Value)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date),
                Type = "Confirmed") %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::ungroup() -> Type_1; Type_1

# Deaths:
deaths <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_deaths_global.csv", sep = "/"))
columns_number <- base::ncol(deaths)
base::colnames(deaths) <- base::c("Province_State", "Country", "Latitude", "Longitude", dates)[1:columns_number]
deaths %>%
  dplyr::mutate(Latitude = NULL,
                Longitude = NULL) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("2020"),
                      names_to = "Date",
                      values_to = "Value") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Value = base::sum(Value)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date),
                Type = "Deaths") %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::ungroup() -> Type_2; Type_2

# Recovered:
recovered <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_recovered_global.csv", sep = "/"))
columns_number <- base::ncol(recovered)
base::colnames(recovered) <- base::c("Province_State", "Country", "Latitude", "Longitude", dates)[1:columns_number]
recovered %>%
  dplyr::mutate(Latitude = NULL,
                Longitude = NULL) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("2020"),
                      names_to = "Date",
                      values_to = "Value") %>%
  dplyr::mutate(Province_State = NULL) %>%
  dplyr::group_by(Country, Date) %>%
  dplyr::summarise(Value = base::sum(Value)) %>%
  dplyr::mutate(Date = lubridate::as_date(Date),
                Type = "Recovered") %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::ungroup() -> Type_3; Type_3

Type <- dplyr::bind_rows(Type_1, Type_2, Type_3) %>%
  tidyr::pivot_wider(names_from = "Type",
                     values_from = "Value") %>%
  dplyr::mutate(Active = Confirmed - Deaths - Recovered) %>%
  tidyr::pivot_longer(cols = base::c("Confirmed", "Deaths", "Recovered", "Active"),
                      values_to = "Value",
                      names_to = "Type") ; Type

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
                            "UK", "Vatican")

europe_map <- ggplot2::map_data("world", region = europe_countries) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(subregion = NULL,
                order = NULL) %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = base::ifelse(Country == "Czech Republic", "Czechia", Country),
                Country = base::ifelse(Country == "UK", "United Kingdom", Country),
                Country = base::ifelse(Country == "Macedonia", "North Macedonia", Country),
                Country = base::ifelse(Country == "Vatican", "Holy See", Country)); europe_map
  
europe_countries <- europe_map %>%
  dplyr::select(Country) %>%
  dplyr::distinct() %>%
  dplyr::pull(); europe_countries

data_filtered <- Type %>%
  dplyr::filter(Country %in% europe_countries); data_filtered

europe_label <- europe_map %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(long = base::mean(long),
                   lat = base::mean(lat)); europe_label

set_theme <- ggplot2::theme(plot.title = ggplot2::element_text(size = 9, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                              axis.text.y = ggplot2::element_text(size = 7, color = "black", face = "plain"),
                              axis.text.x = ggplot2::element_text(size = 7, color = "black", face = "plain"),
                              axis.title.y = ggplot2::element_text(size = 7, color = "black", face = "bold"),
                              axis.title.x = ggplot2::element_text(size = 7, color = "black", face = "bold"),  
                              axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                              axis.ticks.length = ggplot2::unit(0.1, "cm"),
                              panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dashed"),
                              panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dashed"),
                              panel.grid.minor.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                              panel.grid.minor.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                              plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                              panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                              panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                              plot.caption = ggplot2::element_text(size = 7, color = "black", face = "bold", hjust = 1),
                              legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                              legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                              legend.position = "right",
                              legend.box.spacing = ggplot2::unit(0.25, "cm"),
                              legend.text = ggplot2::element_text(size = 7, color = "black", face = "plain"),
                              legend.title = ggplot2::element_text(size = 7, color = "black", face = "bold"))

data <- europe_map %>%
  dplyr::left_join(data_filtered, by = "Country")

#-------------------------------------------------------------------------------
# Generate GIFs:
data %>%
  dplyr::filter(Type == "Confirmed") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Value), col = "black") +
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                title = "Confirmed",
                caption = 'Date: {frame_time}',
                fill = "Confirmed:") +
  set_theme +
  ggplot2::scale_fill_gradient2(low = "white", high = "black") +
  ggplot2::coord_map() +
  gganimate::transition_time(Date) -> plot_type_1
plot_type_1_gif <- gganimate::animate(plot_type_1, width = 480, height = 480)
gganimate::save_animation(plot_type_1_gif, "plot_1.gif")

data %>%
  dplyr::filter(Type == "Deaths") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Value), col = "black") +
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                title = "Deaths",
                caption = 'Date: {frame_time}',
                fill = "Deaths:") +
  set_theme +
  ggplot2::scale_fill_gradient2(low = "white", high = "red") +
  ggplot2::coord_map() +
  gganimate::transition_time(Date) -> plot_type_2
plot_type_2_gif <- gganimate::animate(plot_type_2, width = 480, height = 480)
gganimate::save_animation(plot_type_2_gif, "plot_2.gif")

data %>%
  dplyr::filter(Type == "Recovered") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Value), col = "black") +
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                title = "Recovered",
                caption = 'Date: {frame_time}',
                fill = "Recovered:") +
  set_theme +
  ggplot2::scale_fill_gradient2(low = "white", high = "green") +
  ggplot2::coord_map() +
  gganimate::transition_time(Date) -> plot_type_3
plot_type_3_gif <- gganimate::animate(plot_type_3, width = 480, height = 480)
gganimate::save_animation(plot_type_3_gif, "plot_3.gif")

data %>%
  dplyr::filter(Type == "Active") %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = long, y = lat)) +
  ggplot2::geom_polygon(mapping = ggplot2::aes(group = group, fill = Value), col = "black") +
  ggplot2::labs(x = "Longitude",
                y = "Latitude",
                title = "Active",
                caption = 'Date: {frame_time}',
                fill = "Active:") +
  set_theme +
  ggplot2::scale_fill_gradient2(low = "white", high = "blue") +
  ggplot2::coord_map() +
  gganimate::transition_time(Date) -> plot_type_4
plot_type_4_gif <- gganimate::animate(plot_type_4, width = 480, height = 480)
gganimate::save_animation(plot_type_4_gif, "plot_4.gif")

#-------------------------------------------------------------------------------
# Combine GIFs:
plot_1_mgif <- magick::image_read("plot_1.gif")
plot_2_mgif <- magick::image_read("plot_2.gif")
plot_3_mgif <- magick::image_read("plot_3.gif")
plot_4_mgif <- magick::image_read("plot_4.gif")

new_gif <- magick::image_append(base::c(plot_1_mgif[1], plot_2_mgif[1], plot_3_mgif[1], plot_4_mgif[1]))
for(i in 2:100){
  combined <- magick::image_append(base::c(plot_1_mgif[i], plot_2_mgif[i], plot_3_mgif[i], plot_4_mgif[i]))
  new_gif <- c(new_gif, combined)
}

gganimate::save_animation(new_gif, "final.gif")










