library(rvest)
library(xml2)
library(pdftools)
library(tidyverse)
library(magrittr)
library(gghighlight)

source("function_niid.R", encoding = "CP932")


# run only one time -------------------------------------------------------


# links_csv <- map_chr(paste0("https://www.niid.go.jp/niid/ja/data.html?start=", 1:496), 
#         get_csvLink)
# csv_name <- links_csv %>% 
#   str_extract("\\d{4,4}-\\d\\d-teiten\\.csv$")
# 
# walk(links_csv, download_csv)
# 
# df <- map_dfr(paste0("download_niid/", dir("download_niid/")),
#               format_csv_flu) %>%
#   separate(num_week, c("year", "week"), sep = 5, remove = FALSE) %>%
#   mutate(year = year %>% str_remove("\\D") %>% as.integer,
#          week = week %>% str_remove("\\D") %>% as.integer)

# write_csv(df, "data.csv")


# road data ---------------------------------------------------------------

df <- read_csv("data.csv")

# update data -------------------------------------------------------------
update_niid()

df <- read_csv("data.csv")

# -----------------------------------------------------------------------

now_week <- get_new_week() %>% str_remove("^.*-")
now_date <- df %>% 
  filter(week == now_week, year == date() %>% str_sub(-4, str_length(.))) %>% 
  pull(date) %>% .[1]