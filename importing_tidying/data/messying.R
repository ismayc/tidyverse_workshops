#' Load pkgs
library(tidyr)
library(dplyr)
library(haven)
library(readr)
library(readxl)
library(jsonlite)
library(googlesheets)

#' Retrieve tidied datasets
dem_tidy <- read_rds("data/dem_df.RDS")
fert_tidy <- read_rds("data/fert_df.RDS")
pop_tidy <- read_rds("data/pop_df.RDS")
le_tidy <- read_rds("data/le_df.RDS")
gdp_tidy <- read_rds("data/gdp_df.RDS")
gap_tidy <- read_rds("data/gap.RDS")

#' Make and save messy
le_mess <- le_tidy %>% spread(key = year, value = life_exp) %>%
  filter(rowSums(is.na(.)) < 30)
write_csv(le_mess, "data/messy/le_mess.csv")

data(who)
write_dta(who, "data/messy/who.dta")

#' Converted csv to xlsx for county-pop

ed <- read_excel("data/Education.xls")
sink("data/messy/education.json")
toJSON(ed, pretty = TRUE)
sink()

potus12 <- read_csv("data-raw/US presidential election results by county.csv")
write_sav(potus12, "data/messy/potus12.sav")

#' Output gap to Google Sheets
readr::write_csv(x = gap, path = "data/gap_tidy.csv")
#' On chester.ismay@gmail.com account
gs_title(x = "Updated Gapminder")
