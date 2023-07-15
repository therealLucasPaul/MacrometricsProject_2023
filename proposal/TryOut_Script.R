### Init
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggthemes)

### Proposal Plot Creation
data <- read_xlsx("data/SIPRI_MillexData.xlsx",sheet = "Share of GDP", range="A6:BX199") %>%
  select(Country, `2021.0`) %>%
  slice(124:175) %>%
  filter(!(`2021.0` %in% c("xxx","...", NA))) %>%
  mutate(pct2020 = as.numeric(`2021.0`)*100) %>%
  select(-`2021.0`)

# Here it is necessary to manually change some country names in "geo_cepii.xlsx" 
NATO <- read.table("data/NATO_members.txt",sep="\t", col.names = c("Country")) %>%
  mutate(NATO = 1)
country_data <- read_xls("data/geo_cepii.xls") %>%
  select(iso2, iso3, country, continent, city_en)
dist_data <- read_xls("data/dist_cepii.xls") %>%
  filter(iso_o == "RUS") %>%
  select(iso_o, iso_d, dist)

tmp_data <- left_join(dist_data, country_data, by=c("iso_d" = "iso3"), keep)
tmp_data <- left_join(data, tmp_data, by=c("Country"="country"))
final_data <- left_join(tmp_data, NATO, by=c("Country"))%>%
  mutate_if(is.numeric,coalesce,0) %>%
  mutate(NATO = as.factor(NATO))

ggplot(final_data, aes(x=dist, y=pct2020, color = NATO))+
  geom_point()+
  geom_label(label=final_data$Country, 
             nudge_x = 0.25, nudge_y = 0.25, 
             check_overlap = T)+
  xlab("Distance to Moscow in km")+
  ylab("Military Spending in % of GDP (2021)")+
  labs(title = "Relationship between Military Spending and Distance to Moscow", subtitle = "as % of GDP in 2021")+
  scale_color_manual(values = c("0" = "red",
                                "1"="blue"))+
  theme_minimal()
