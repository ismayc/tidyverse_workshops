#' Load pkgs
library(tidyr)
library(dplyr)

#' Retrieve tidied datasets
dem_tidy <- read_rds("data/dem_df.RDS")
fert_tidy <- read_rds("data/fert_df.RDS")
pop_tidy <- read_rds("data/pop_df.RDS")
le_tidy <- read_rds("data/le_df.RDS")
gdp_tidy <- read_rds("data/gdp_df.RDS")
gap_tidy <- read_rds("data/gap.RDS")

#' Make and save messy
pop_mess <- pop_tidy %>% spread(key = year, value = population) %>%
  filter(rowSums(is.na(.)) < 50)
write_csv(pop_mess, "data/messy/pop_mess.csv")

le_mess <- le_tidy %>% spread(key = year, value = life_exp) %>%
  filter(rowSums(is.na(.)) < 30)
write_csv(pop_mess, "data/messy/le_mess.csv")


fert_gdp_mess <- gap %>% 
  select(country, year, region, sub_region, fert_rate, gdp_per_cap) %>%
  spread(key = year, value = fert_rate) %>%
  select(-gdp_per_cap, -region, -sub_region) %>%
  na.omit() %>% 
  spread(key = region, value = gdp_per_cap)
  
who_fin <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

who_mess <- who_fin %>%
  unite(sexage, sex, age, sep = "") %>%
  mutate(new = "new") %>% 
  unite(code, new, var, sexage) %>% 
  mutate(code = stringr::str_replace(code, "new_rel", "newrel")) %>% 
  spread(code, value)