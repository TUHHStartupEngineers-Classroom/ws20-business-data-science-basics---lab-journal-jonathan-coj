library(tidyverse)
library(httr)
library(glue)


api_key <- "00983b00961d78e6db02be765c604c7b"
base_url <- "https://api.openweathermap.org/data/2.5/weather"


city_list <- c("Hamburg","Bonn","Berlin")
weather_tbl <- tibble(City = character() ,Temperature = numeric())
for (city in city_list) {
  weather_path <- "?q={city}&appid={api_key}"
  api_url <- glue(base_url, weather_path)
  weather <- GET(url = api_url)
  weather_data <- weather %>% content(as = 'parsed')
  temperature <-  weather_data$main$temp - 273.16
  weather_tbl <- weather_tbl %>% add_row(City = city, Temperature = temperature)
}

