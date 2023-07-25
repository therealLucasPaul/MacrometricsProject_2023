#### Statistical Analysis - Macrometrics 2023
### Fynn Lohre - Lucas Unterweger - Sophia Oberbrinkmann

## Necessary Packages
rm(list=ls())
library(brms)
library(readxl)
library(tidyverse)

## Read in / Set Up Data
military_data <- read_xlsx("../02_Raw_Data/SIPRI_MillexData.xlsx",sheet = "Share of GDP_Adjusted", range="A6:BX180") %>% ### Data on military expenditure
  select(Country, `2021`) %>%
  #slice(124:175) %>%
  filter(!(`2021` %in% c("xxx","...", NA))) %>%
  mutate(pct2020 = as.numeric(`2021`)*100) %>%
  select(-`2021`) %>%
  arrange(Country)

country_data <- read_xls("../02_Raw_Data/geo_cepii.xls") %>% ### ISO Codes of Countries (necessary link)
  select(iso2, iso3, country, continent, city_en) %>%
  rename(Country = country) %>%
  rename(capital = city_en) %>%
  add_row(iso2 = "ME", iso3 = "MNE", Country = "Montenegro", continent = "Europe", capital = "Podgorica")

distance_data <- read_xls("../02_Raw_Data/dist_cepii.xls") %>% ### Distance of Capitals to Moskow
  filter(iso_o == "RUS") %>%
  select(iso_d, dist) %>%
  add_row(iso_d = "MNE", dist = 1983)

democracyindex_data <- read_csv("../02_Raw_Data/electoral-democracy-index.csv") %>% ### Democracy Index
  filter(Year == 2021) %>%
  rename(Country = Entity) %>%
  rename(iso3 = Code)

border_alliance_data <- read_xlsx("../02_Raw_Data/Border_Data.xlsx") %>% ### Data on Borders and political/mitliary alliances
  rename(DirectBorder = `Direct Border`)%>%
  rename(SecondaryBorder = `Secondary Boarder`)%>%
  mutate(Nato = as.factor(Nato)) %>%
  mutate(DirectBorder = (DirectBorder)) %>%
  mutate(SecondaryBorder = as.factor(SecondaryBorder)) %>%
  mutate(OKVS = as.factor(OKVS)) %>%
  mutate(BRICS = as.factor(BRICS))

## Merge Data Set
totaldata <- left_join(left_join(left_join(left_join(military_data, country_data, by="Country"), border_alliance_data, by="Country"),democracyindex_data, by="iso3"), distance_data, by=c("iso3" = "iso_d")) %>%
  filter(!(Country.x %in% c("Russia","Kosovo", "South Sudan")))

head(totaldata)
View(totaldata)



## OLS Estimation
mod_test <- lm(pct2020 ~ dist+Nato+BRICS+OKVS+electdem_vdem_owid+continent+DirectBorder+SecondaryBorder, data=totaldata)
summary(mod_test)
