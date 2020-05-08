#-------------------------------------------------------------------------------
# AUTOMATICALY ANALYSE INDICATED COUNTRY:
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# libraries:
base::library(tidyverse)
base::library(gridExtra)
base::library(readr)
base::library(lubridate)
base::library(strinigr)
base::library(knitr)

#-------------------------------------------------------------------------------
# Environment:
base::Sys.setenv(LANG = "en")
base::Sys.setlocale("LC_TIME", "C")

#-------------------------------------------------------------------------------
# Function:
coronavirus_country <- function(country){
  
  dates <- base::as.character(base::seq(from = lubridate::as_date("2020-01-22"), to = lubridate::as_date(Sys.Date()), by = 1))
  
  #-------------------------------------------------------------------------------
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
  
  #-------------------------------------------------------------------------------
  # Deaths:
  deaths <- readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_deaths_global.csv", sep = "/"))
  columns_number <- base::ncol(deaths)
  base::colnames(deaths) <- base::c("Province_State", "Country", "Latitude", "Longitude", DATES)[1:columns_number]
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
  base::colnames(recovered) <- base::c("Province_State", "Country", "Latitude", "Longitude", DATES)[1:columns_number]
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
  # Concatenate data:
  confirmed %>%
    dplyr::left_join(deaths, by = base::c("Country", "Date")) %>%
    dplyr::left_join(recovered, by = base::c("Country", "Date")) %>%
    dplyr::mutate(Active = Confirmed - Deaths - Recovered) %>%
    dplyr::mutate(New_Active = Active - dplyr::lag(Active)) %>%
    tidyr::replace_na(base::list(New_Active = 0)) %>%
    dplyr::mutate(Year = lubridate::year(Date),
                  Month = lubridate::month(Date, label = TRUE, abbr = FALSE),
                  Day = lubridate::day(Date),
                  Week_Day = lubridate::wday(Date, week_start = 1, label = TRUE, abbr = FALSE)) -> concatenated
  
  #-------------------------------------------------------------------------------
  # Visualize country:
  concatenated %>%
    dplyr::filter(Country == country) %>%
    tidyr::pivot_longer(cols = base::c("Confirmed", "New_Confirmed", "Deaths", "New_Deaths", "Recovered", "New_Recovered", "Active", "New_Active"),
                        names_to = "Type",
                        values_to = "Count") -> concatenated_longer; concatenated_longer
  
  concatenated_longer %>%
    dplyr::filter(Type %in% base::c("Confirmed", "Deaths", "Recovered", "Active")) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(Status = base::sum(Count)) %>%
    dplyr::filter(Status != 0) %>%
    dplyr::mutate(Type = base::factor(Type, levels = base::c("Confirmed", "Deaths", "Recovered", "Active"), ordered = TRUE)) %>%
    ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = Date, y = Count, colour = Type, label = Count)) +
    ggplot2::geom_line(lwd = 1.5) +
    ggplot2::labs(x = "Date",
                  y = "Count",
                  title = base::paste(stringr::str_to_upper(country), "- cumulative statistics"),
                  caption = base::paste0(DATES[1], " - ", DATES[columns_number - 4]),
                  colour = "Type: ") +
    ggplot2::scale_colour_manual(values = base::c("Confirmed" = "black", "Deaths" = "red", "Recovered" = "green", "Active" = "blue")) +
    ggplot2::facet_grid(~Type) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.text.x = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 8, color = "black", face = "bold"),  
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.minor.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = 8, color = "black", face = "bold", hjust = 1),
                   legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                   legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                   legend.position = "bottom",
                   legend.box.spacing = ggplot2::unit(0.25, "cm"),
                   legend.text = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   legend.title = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   strip.background = ggplot2::element_rect(colour = "black", fill = "gray90")) -> plot1
  
  concatenated_longer %>%
    dplyr::filter(Type %in% base::c("New_Confirmed", "New_Deaths", "New_Recovered", "New_Active")) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(Status = base::sum(Count)) %>%
    dplyr::filter(Status != 0) %>%
    dplyr::mutate(Type = base::factor(Type, levels = base::c("New_Confirmed", "New_Deaths", "New_Recovered", "New_Active"), ordered = TRUE)) %>%
    ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = Date, y = Count, colour = Type)) +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = Date, xend = Date, y = 0, yend = Count, colour = Type), lwd = 1.5) +
    ggplot2::geom_smooth(color = "white", lwd = 2, se = FALSE) +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::labs(x = "Date",
                  y = "Count",
                  title = base::paste(stringr::str_to_upper(country), "- day-to-day fluctuations"),
                  caption = base::paste0(DATES[1], " - ", DATES[columns_number - 4]),
                  colour = "Type: ") +
    ggplot2::scale_colour_manual(values = base::c("New_Confirmed" = "black", "New_Deaths" = "red", "New_Recovered" = "green", "New_Active" = "blue")) + 
    ggplot2::facet_grid(~Type) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.text.x = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 8, color = "black", face = "bold"),  
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.minor.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = 8, color = "black", face = "bold", hjust = 1),
                   legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                   legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                   legend.position = "bottom",
                   legend.box.spacing = ggplot2::unit(0.25, "cm"),
                   legend.text = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   legend.title = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   strip.background = ggplot2::element_rect(colour = "black", fill = "gray90")) -> plot2
  
  concatenated %>%
    dplyr::filter(Country == country) %>%
    dplyr::filter(Confirmed != 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Deaths_Ratio = base::round(Deaths/Confirmed, 4),
                  Recovered_Ratio = base::round(Recovered/Confirmed, 4),
                  Active_Ratio = base::round(Active/Confirmed, 4)) %>%
    dplyr::select(Date, Deaths_Ratio, Recovered_Ratio, Active_Ratio) %>%
    tidyr::pivot_longer(cols = base::c("Deaths_Ratio", "Recovered_Ratio", "Active_Ratio"),
                        names_to = "Type",
                        values_to = "Ratio") %>%
    dplyr::mutate(Type = base::factor(Type, levels = base::c("Deaths_Ratio", "Recovered_Ratio", "Active_Ratio"), ordered = TRUE)) %>%
    ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = Date, y = Ratio, colour = Type)) +
    ggplot2::geom_line(lwd = 1.5) +
    ggplot2::scale_colour_manual(values = base::c("Deaths_Ratio" = "red", "Recovered_Ratio" = "green", "Active_Ratio" = "blue")) +
    ggplot2::labs(x = "Date",
                  y = "Percentage",
                  title = base::paste(stringr::str_to_upper(country), "- ratio"),
                  caption = base::paste0(DATES[1], " - ", DATES[columns_number - 4]),
                  colour = "Type: ") +
    ggplot2::facet_grid(rows = dplyr::vars(Type)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.text.x = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   axis.title.y = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 8, color = "black", face = "bold"),  
                   axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                   axis.ticks.length = ggplot2::unit(0.1, "cm"),
                   panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dashed"),
                   panel.grid.minor.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                   panel.grid.minor.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                   plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                   panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                   plot.caption = ggplot2::element_text(size = 8, color = "black", face = "bold", hjust = 1),
                   legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                   legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                   legend.position = "bottom",
                   legend.box.spacing = ggplot2::unit(0.25, "cm"),
                   legend.text = ggplot2::element_text(size = 8, color = "black", face = "plain"),
                   legend.title = ggplot2::element_text(size = 8, color = "black", face = "bold"),
                   strip.background = ggplot2::element_rect(colour = "black", fill = "gray90")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = base::c(0, 1),
                                breaks = base::seq(from = 0, to = 1, by = 0.1)) -> plot3
  
  #-------------------------------------------------------------------------------
  # Display final results:
  plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot3, layout_matrix = base::matrix(base::rbind(base::c(1, 1, 3),base::c(2, 2, 3)), nrow = 2)))
  
  concatenated %>%
    dplyr::filter(Country == country) %>%
    dplyr::mutate(Deaths_Ratio = base::round(Deaths/Confirmed, 4),
                  Recovered_Ratio = base::round(Recovered/Confirmed, 4),
                  Active_Ratio = base::round(Active/Confirmed, 4)) %>%
    dplyr::filter(Confirmed > 0) %>%
    dplyr::mutate(Day_Since_1st_Confirmed = dplyr::row_number(),
                  Day_Since_100st_Confirmed = base::cumsum(base::ifelse(Confirmed > 100, 1, 0))) -> concatenated_2
  
  concatenated_2 %>%
    knitr::kable() %>%
    base::print()
  
  base::invisible(concatenated_2)
}

#-------------------------------------------------------------------------------
# Test function - analyse particular coutry:
coronavirus_country("US")

#-------------------------------------------------------------------------------
# Available countries list:
readr::read_csv(base::paste(base::getwd(), "Data", "time_series_covid19_confirmed_global.csv", sep = "/")) %>%
  dplyr::select("Country/Region") %>%
  dplyr::pull() %>%
  base::unique() %>%
  base::sort() -> countries; countries
