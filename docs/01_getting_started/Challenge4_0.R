library(data.table)
library(ggplot2)
library(scales)
library(tidyverse)
library(maps)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% as.data.table()

# Data Manipulation
covid_data_cum_tbl <- covid_data_tbl[,.(dateRep, day, month, countriesAndTerritories, cases)][order(month, day)][, cumulativeCases := cumsum(cases), by = "countriesAndTerritories"][order(countriesAndTerritories)]

covid_data_cum_selected_tbl <- covid_data_cum_tbl[countriesAndTerritories %in% c("Germany", "United_Kingdom", "France", "Spain", "United_States_of_America")]

covid_data_cum_selected_tbl <- covid_data_cum_selected_tbl[,.(dateRep, day, month, countriesAndTerritories, cumulativeCases)]
covid_data_cum_selected_tbl <- covid_data_cum_selected_tbl %>%
  pivot_wider(names_from  = "countriesAndTerritories",
              values_from = "cumulativeCases") %>% 
        na.omit()

yscale <- c(2.5, 5.0, 7.5, 10, 12.5, 15)
covid_data_cum_selected_tbl %>%
  ggplot(aes(x = as.Date(dateRep, format="%d/%m/%Y"))) +
  geom_line(aes(y = Germany, color = "Germany"), size = 1.5, linetype = 1) +
  geom_line(aes(y = France, color = "France"), size = 1.5, linetype = 1) +
  geom_line(aes(y = United_Kingdom, color = "UK"), size = 1.5, linetype = 1)  +
  geom_line(aes(y = Spain, color = "Spain"), size = 1.5, linetype = 1) +
  geom_line(aes(y = United_States_of_America, color = "USA"), size = 1.5, linetype = 1) +
  scale_y_continuous(labels = paste0(yscale, "M"),
                     breaks = 10^6 * yscale) +
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%B"),
               limits = as.Date(c('2020-01-01','2020-12-31'))) + 
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(color = 'black'),
    legend.key.size = unit(2, "line"),
    legend.key = element_rect(fill = "white"),
    axis.text = element_text(color = 'black', size = 7.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    title = element_text(colour = 'black', size = 9),
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'grey90', size = 0.5),
    panel.grid.minor = element_line(color = 'grey80', size = 0.5), 
    plot.background = element_rect(fill = 'white')) +
  labs(title    = "COVID-19 confirmed cases worldwide",
       subtitle = "As of 06/12/2020 USA is the leading country in cumulated cases",
       x = "Year 2020",
       y = "Cumulative Cases",
       color = "")


covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories))

world <- map_data("world")

covid_data_deaths_tbl <- covid_data_tbl %>% 
  group_by(countriesAndTerritories) %>%
  summarise(total_deaths = sum(deaths/popData2019), .groups = 'keep') %>% 
  merge(world, by.x = "countriesAndTerritories", by.y = "region") %>% 
  rename(region = countriesAndTerritories)

covid_data_deaths_tbl %>% ggplot() +
  geom_map(aes(fill = total_deaths, x = long, y = lat, map_id = region), map = world, color = 'grey10') +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(color = 'black'),
    legend.key = element_rect(fill = "grey70"),
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'grey80', size = 0.5),
    panel.grid.minor = element_line(color = 'grey90', size = 0.5), 
    plot.background = element_rect(fill = 'white'),
    text = element_text(color = 'black')
  ) +
  labs(title    = "COVID-19 mortality",
       subtitle = "COVID-19 worldwide mortality",
       x = "",
       y = "",
       color = "") + 
  guides(fill = guide_colorbar()) +
  scale_fill_continuous(name = "Mortality Rate", low = "#0012FF", high = "black", labels = percent)

