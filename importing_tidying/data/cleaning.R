#' Install needed packages
dl_repo <- "http://cran.rstudio.org"
install.packages("tidyverse", repos = dl_repo)
install.packages("googlesheets", repos = dl_repo)

#' Load packages
library(googlesheets)
library(tidyr)
library(dplyr)
library(readr)

#' Links from Gapminder data page
##' Population - https://spreadsheets.google.com/pub?key=phAwcNAVuyj0XOoBL_n5tAQ&output=xls
##' Life Expectancy - https://spreadsheets.google.com/pub?key=phAwcNAVuyj2tPLxKvvnNPA&output=xls
##' GDP per Capita - https://spreadsheets.google.com/pub?key=phAwcNAVuyj1jiMAkmq1iMg&output=xls
##' Democracy Score - https://spreadsheets.google.com/pub?key=0ArfEDsV3bBwCdGQ2YlhDSWVIdXdpMmhLY2ZZRHdNNnc&output=xls
##' Fertility Rate - https://spreadsheets.google.com/pub?key=phAwcNAVuyj0TAlJeCEzcGQ&output=xls

##' After directing to those pages you can extract the actual Google Sheets key as I did below

#' Read in raw data
pop <- gs_key("1IbDM8z5XicMIXgr93FPwjgwoTTKMuyLfzU6cQrGZzH8") %>% gs_read()
le <- gs_key("1H3nzTwbn8z4lJ5gJ_WfDgCeGEXK3PVGcNjQ_U5og8eo") %>% gs_read()
gdp <- gs_key("1PybxH399kK6OjJI4T2M33UsLqgutwj3SuYbk7Yt6sxE") %>% gs_read()
dem <- gs_key("1MgJAijU4I4WnB8JDu6uPmS9lGYaPUkCt1k-RYTZ4nSE") %>% gs_read()
fert <- gs_key("1oq3r8W7ajenKFgoAYoOf2MXeTWWNPpudR-Fo5m2-o30") %>% gs_read()

#' Different numbers of observations
nrow(dem) # 275
nrow(fert) # 260
nrow(gdp) #275
nrow(le) #260
nrow(pop) #275

#' Change names of first column to be `country`
names(dem)[1] <- "country"
names(fert)[1] <- "country"
names(gdp)[1] <- "country"
names(le)[1] <- "country"
names(pop)[1] <- "country"

#' Convert to factors to look for overlaps between datasets
dem <- dem %>% mutate(country = factor(country))
fert <- fert %>% mutate(country = factor(country))
gdp <- gdp %>% mutate(country = factor(country))
le <- le %>% mutate(country = factor(country))
pop <- pop %>% mutate(country = factor(country))

#' From Jenny Bryan (https://github.com/jennybc/gapminder/blob/master/data-raw/04_merge-pop-lifeExp-gdpPercap.R)
country_levels <- function(df) levels(df$country)
country_subs <- c("Bahamas, The" = "Bahamas",
                  "Central African Rep." = "Central African Republic",
                  "Cook Is" = "Cook Islands",
                  "Czech Rep." = "Czech Republic",
                  "Dominican Rep." = "Dominican Republic",
                  "Egypt, Arab Rep." = "Egypt",
                  "Gambia, The" = "Gambia",
                  "Iran, Islamic Rep." = "Iran",
                  "Russian Federation" = "Russia",
                  "Syrian Arab Republic" = "Syria",
                  "Venezuela, RB" = "Venezuela")
revalue_country <- function(x) plyr::revalue(x, country_subs)
dem <- dem %>%
  mutate(country = revalue_country(country))
fert <- fert %>%
  mutate(country = revalue_country(country))
pop <- pop %>%
  mutate(country = revalue_country(country))
le <- le %>%
  mutate(country = revalue_country(country))
gdp <- gdp %>%
  mutate(country = revalue_country(country))

union_country <- country_levels(dem) %>%
  union(country_levels(fert)) %>%
  union(country_levels(gdp)) %>%
  union(country_levels(le)) %>%
  union(country_levels(pop)) %>%
  sort()

c_dat <- data_frame(country = union_country,
                    dem = country %in% levels(dem$country),
                    fert = country %in% levels(fert$country),
                    pop = country %in% levels(pop$country),
                    le = country %in% levels(le$country),
                    gdp = country %in% levels(gdp$country),
                    total = dem + fert + pop + le + gdp)
c_dat %>% count(total)

#' Population only recorded every year after 1950
#' Tidy data
dem_df <- dem %>% gather(key = "year", value = "dem_score", -country) %>%
  filter(year > 1950)
fert_df <- fert %>% gather(key = "year", value = "fert_rate", -country) %>%
  filter(year > 1950)
pop_df <- pop %>% gather(key = "year", value = "population", -country) %>%
  filter(year > 1950)
le_df <- le %>% gather(key = "year", value = "life_exp", -country) %>%
  filter(year > 1950)
gdp_df <- gdp %>% gather(key = "year", value = "gdp_per_cap", -country) %>%
  filter(year > 1950)

#' Export to RDS for potential workshop work
write_rds(dem_df, "data/dem_df.RDS")
write_rds(fert_df, "data/fert_df.RDS")
write_rds(pop_df, "data/pop_df.RDS")
write_rds(le_df, "data/le_df.RDS")
write_rds(gdp_df, "data/gdp_df.RDS")

#' Join data sets
gap_df <- pop_df %>%
  inner_join(dem_df, by = c("country", "year")) %>%
  inner_join(fert_df, by = c("country", "year")) %>%
  inner_join(le_df, by = c("country", "year")) %>%
  inner_join(gdp_df, by = c("country", "year")) %>%
  droplevels() %>%
  arrange(country, year) %>%
  mutate(missing = rowSums(is.na(.))) %>%
  filter(missing < 3) %>%
  select(-missing) %>% 
  # Only one entry
  filter(country != "Kosovo")

#' Get sub-region and region
countries <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
countries$name[countries$name == "Bolivia (Plurinational State of)"] <- "Bolivia"
countries$name[countries$name == "Congo"] <- "Congo, Rep."
countries$name[countries$name == "Congo (Democratic Republic of the)"] <- "Congo, Dem. Rep."
countries$name[countries$name == "Hong Kong"] <- "Hong Kong, China"
countries$name[countries$name == "Côte d'Ivoire"] <- "Cote d'Ivoire"
countries$name[countries$name == "Iran (Islamic Republic of)"] <- "Iran"
countries$name[countries$name == "Korea (Democratic People's Republic of)"] <- "Korea, Dem. Rep."
countries$name[countries$name == "Korea (Republic of)"] <- "Korea, Rep."
countries$name[countries$name == "Réunion"] <- "Reunion"
countries$name[countries$name == "Slovakia"] <- "Slovak Republic"
countries$name[countries$name == "Syrian Arab Republic"] <- "Syria"
countries$name[countries$name == "Taiwan, Province of China"] <- "Taiwan"
countries$name[countries$name == "Tanzania, United Republic of"] <- "Tanzania"
countries$name[countries$name == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
countries$name[countries$name == "United States of America"] <- "United States"
countries$name[countries$name == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
countries$name[countries$name == "Viet Nam"] <- "Vietnam"
countries$name[countries$name == "Yemen"] <- "Yemen, Rep."
countries$name[countries$name == "Russian Federation"] <- "Russia"
countries$name[countries$name == "Macedonia (the former Yugoslav Republic of)"] <- "Macedonia, FYR"
countries$name[countries$name == "Micronesia (Federated States of)"] <- "Micronesia, Fed. Sts."
countries$name[countries$name == "Moldova (Republic of)"] <- "Moldova"
countries <- tbl_df(countries) %>%
  select(name, region, `sub-region`)

gap <- gap_df %>%
  left_join(countries, by = c("country" = "name")) %>%
  rename(sub_region = `sub-region`) %>%
  mutate(sub_region = ifelse(country == "West Bank and Gaza", "Western Asia", sub_region)) %>%
  mutate(region = ifelse(country == "West Bank and Gaza", "Asia", region)) %>%
  mutate(sub_region = ifelse(country == "Brunei", "South-Eastern Asia", sub_region)) %>%
  mutate(region = ifelse(country == "Brunei", "Asia", region)) %>%
  mutate(sub_region = ifelse(country == "Cape Verde", "Western Africa", sub_region)) %>%
  mutate(region = ifelse(country == "Cape Verde", "Africa", region)) %>% 
  mutate(sub_region = ifelse(country == "Channel Islands", "Western Europe", sub_region)) %>%
  mutate(region = ifelse(country == "Channel Islands", "Europe", region)) %>% 
  mutate(sub_region = ifelse(country == "Macao, China", "Eastern Asia", sub_region)) %>%
  mutate(region = ifelse(country == "Macao, China", "Asia", region)) %>% 
  mutate(sub_region = ifelse(country == "Netherlands Antilles", "Caribbean", sub_region)) %>%
  mutate(region = ifelse(country == "Netherlands Antilles", "Americas", region)) 

#' Only keep countries with all 61 data points 
#' (Andorra, Bermuda, Dominica, and Marshall Islands only have 22)
#' From Jenny Bryan (https://github.com/jennybc/gapminder/blob/master/data-raw/08_filter-every-five-years.md)
country_freq <- gap %>% count(country)

gap <- country_freq %>% 
  filter(n == 61) %>% 
  left_join(gap, by = "country") %>% 
  select(-n) %>% 
  droplevels() %>%
  arrange(country, year)

#' Export to RDS for workshop work
write_rds(gap, "data/gap.RDS")
