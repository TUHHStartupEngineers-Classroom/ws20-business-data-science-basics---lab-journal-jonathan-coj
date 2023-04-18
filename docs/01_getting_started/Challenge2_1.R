library(tidyverse)
library(rvest)     
library(glue)
library(stringr)
library(purrr)

base_url <- "https://www.rosebikes.de"
bike_path <- "fahrräder"

bike_url <- glue(base_url, "/",bike_path)
bike_html <- bike_url %>% read_html()

category_path <- bike_html %>% html_nodes(css = ".catalog-navigation__list-item > a") %>% html_attr("href") %>% discard(.p = ~stringr::str_detect(.x,"zoovu|sale" ))  %>% enframe(value = "Path")
categorys <- category_path %>%  mutate(category = category_path$Path %>% map( str_extract,"(?<=./).+"))
i = 1
for (category in categorys$category) {
  categorys$category[[i]] <- glue(bike_url,"/", category)
  i <- i+1
}

category_html <- categorys$category %>% map(read_html)
bike_name <- category_html %>% map(html_nodes, css = c(".catalog-category-bikes__title-text") ) %>%  map(html_text) %>% enframe(value = "Name")
bike_price <- category_html %>% map(html_nodes, css = c(".catalog-category-bikes__price-title") ) %>%  map(html_text) %>% enframe(value = "Price")
bike_info <- merge(bike_name,bike_price)
bike_info <- bike_info %>% mutate(Name_str = map(.$Name, toString) %>% str_remove_all("[\"c()]|\\n")) %>% 
  mutate(Price_str = map(.$Price, toString) %>% str_remove_all("[\"c()]|\\n|ab "))
bike_info_name <- strsplit(bike_info$Name_str, split = ", ") %>% unlist() %>% enframe(value = "Name")
bike_info_price <- strsplit(bike_info$Price_str, split = ", ") %>% unlist() %>% enframe(value = "Price")
bike_info <- merge(bike_info_name, bike_info_price)

  