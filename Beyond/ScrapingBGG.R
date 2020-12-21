# Install packages:
library(rvest)
library(glue)
library(tidyverse)


baseurl <- "https://www.boardgamegeek.com/xmlapi2/thing?id=181&ratingcomments=1&page="
# base url to get individual ratings for game with id=181 (risk)
page_max <- 301
page <- seq.int(1,page_max)
# list of integers from 1 to page_max inclusive

url <- glue("{baseurl}{1}")
urls <- page %>% 
  map(~glue("{baseurl}{.x}")) %>% 
  unlist()
# list of urls from page=1 to page_max

raw.result <- read_xml(urls[1])

# raw.result.getElementsByTagName("comments")
# read page 1
  