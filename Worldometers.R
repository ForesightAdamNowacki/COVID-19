#-------------------------------------------------------------------------------
# WORLDOMETER:
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# libraries:
base::library(tidyverse)
base::library(gridExtra)
base::library(readr)
base::library(lubridate)
base::library(stringr)
base::library(knitr)
base::library(foreach)
base::library(doParallel)

#-------------------------------------------------------------------------------
url <- "https://www.worldometers.info/coronavirus/?fbclid=IwAR25GxAg67LR3AE1nP-czIHP-YzxiEF_OQwItsDmUM7I4U_QIRHPdGFwGkQ"
scraped <- rvest::html_table(xml2::read_html(url), fill = TRUE)
df <- scraped[[1]] %>%
  tibble::as_tibble() %>%
  dplyr::filter(`#` > 0) %>%
  dplyr::mutate(`#` = NULL,
                `1 Caseevery X ppl` = NULL,
                `1 Deathevery X ppl` = NULL,
                `1 Testevery X ppl` = NULL) 

base::colnames(df) <- base::c("Country", "TotalCases", "NewCases", "TotalDeaths", "NewDeaths", "TotalRecovered", "NewRecovered", "ActiveCases", "SeriousCritical", "CasesPer1MPop", "DeathsPer1MPop",
                              "TotalTests", "TestsPer1MPop", "Population", "Continent")
df <- df %>%
  dplyr::mutate(TotalCases = stringr::str_replace_all(TotalCases, ",", ""),
                TotalCases = stringr::str_replace_all(TotalCases, "N/A", ""),
                TotalCases = base::as.numeric(TotalCases),
                TotalDeaths = stringr::str_replace_all(TotalDeaths, ",", ""),
                TotalDeaths = stringr::str_replace_all(TotalDeaths, "N/A", ""),
                TotalDeaths = base::as.numeric(TotalDeaths),
                TotalRecovered = stringr::str_replace_all(TotalRecovered, ",", ""),
                TotalRecovered = stringr::str_replace_all(TotalRecovered, "N/A", ""),
                TotalRecovered = base::as.numeric(TotalRecovered),
                ActiveCases = stringr::str_replace_all(ActiveCases, ",", ""),
                ActiveCases = stringr::str_replace_all(ActiveCases, "N/A", ""),
                ActiveCases = base::as.numeric(ActiveCases),
                SeriousCritical = stringr::str_replace_all(SeriousCritical, ",", ""),
                SeriousCritical = stringr::str_replace_all(SeriousCritical, "N/A", ""),
                SeriousCritical = base::as.numeric(SeriousCritical),
                TotalTests = stringr::str_replace_all(TotalTests, ",", ""),
                TotalTests = stringr::str_replace_all(TotalTests, "N/A", ""),
                TotalTests = base::as.numeric(TotalTests),
                CasesPer1MPop = stringr::str_replace_all(CasesPer1MPop, ",", ""),
                CasesPer1MPop = stringr::str_replace_all(CasesPer1MPop, "N/A", ""),
                CasesPer1MPop = base::as.numeric(CasesPer1MPop),
                TestsPer1MPop = stringr::str_replace_all(TestsPer1MPop, ",", ""),
                TestsPer1MPop = stringr::str_replace_all(TestsPer1MPop, "N/A", ""),
                TestsPer1MPop = base::as.numeric(TestsPer1MPop),
                Population = stringr::str_replace_all(Population, ",", ""),
                Population = stringr::str_replace_all(Population, "N/A", ""),
                Population = base::as.numeric(Population),
                DeathsPer1MPop = stringr::str_replace_all(DeathsPer1MPop, ",", ""),
                DeathsPer1MPop = stringr::str_replace_all(DeathsPer1MPop, "N/A", ""),
                DeathsPer1MPop = base::as.numeric(DeathsPer1MPop),
                NewCases = stringr::str_replace_all(NewCases, ",", ""),
                NewCases = stringr::str_replace_all(NewCases, "N/A", ""),
                NewCases = stringr::str_replace_all(NewCases, "\\+", ""),
                NewCases = base::as.numeric(NewCases),
                NewDeaths = stringr::str_replace_all(NewDeaths, ",", ""),
                NewDeaths = stringr::str_replace_all(NewDeaths, "N/A", ""),
                NewDeaths = stringr::str_replace_all(NewDeaths, "\\+", ""),
                NewDeaths = base::as.numeric(NewDeaths),
                NewRecovered = stringr::str_replace_all(NewRecovered, ",", ""),
                NewRecovered = stringr::str_replace_all(NewRecovered, "N/A", ""),
                NewRecovered = stringr::str_replace_all(NewRecovered, "\\+", ""),
                NewRecovered = base::as.numeric(NewRecovered)); df

df %>%
  dplyr::filter(!is.na(NewCases)) %>%
  dplyr::mutate(NewCasesPer1MPop = NewCases/(Population/1000000)) %>%
  dplyr::arrange(dplyr::desc(NewCasesPer1MPop))



