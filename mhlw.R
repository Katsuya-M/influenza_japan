library(rvest)
library(xml2)
library(pdftools)
library(tidyverse)
library(magrittr)



# extract links of pdf files ----------------------------------------------


links_year <-
  read_html("https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/kenkou/kekkaku-kansenshou01/houdou.html") %>%
  html_nodes(xpath = "//div[@class = 'm-grid__col1']") %>%
  html_nodes("li") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  {paste0("https://www.mhlw.go.jp", .)}

links_pdf <- map(links_year, ~ read_html(.) %>%
                          html_nodes("a") %>%
                          html_attr("href") %>%
                          str_subset("^\\/content\\/.*\\.pdf$")) %>%
  unlist() %>%
  {paste0("https://www.mhlw.go.jp", .)}


# download all pdf files --------------------------------------------------

# download_pdf_flu <- function(url, time_sleep = 3) {
#  download.file(url, path.expand(paste0("download/", str_extract(url, "[:digit:]*\\.pdf$"))), mode = "wb"
#   )
#   Sys.sleep(time_sleep)
# }
# 
# walk(links_pdf, download_pdf_flu, 1)



# download pdf that has not got yet-------------------------------------------------------------------------


correct_fileName <- links_pdf %>%
  str_extract(., "[:digit:]*\\.pdf$")

if (setdiff(correct_fileName, dir("download/")) %>% length() > 0) {# check if all files are successfuly downloaded
  setdiff(correct_fileName, dir("download/")) %>% 
    map_chr( ~ str_subset(links_pdf, .)) %>% 
    walk(download_pdf_flu, 1)
}


# convert pdf to tibble-------------------------------------------------------------------------

convert_pdf2tibble <- function(path) {
  data_chr <- pdf_text(path) %>% 
    str_subset("^\\s*インフルエンザ定点当たり報告数・") %>% 
    read_lines() %>% str_squish() %>% 
    .[c(2, 6:53)] 
  
  `報告数` <- data_chr[-1] %>% str_extract(" ([:digit:]|-|\\.|,)+") %>% 
    str_replace("-", "0") %>% 
    str_remove_all(",") %>% 
    as.integer()
  
  `定点当たり` <- data_chr[-1] %>% str_extract_all(" ([:digit:]|-|\\.|,)+") %>% 
    map_chr(~.[-1]) %>% 
    str_replace("-", "0") %>% 
    str_remove_all(",") %>% 
    as.integer()
  
  prefecture <- data_chr[-1] %>% str_remove(" ([:digit:]|-|\\.| |,)+") %>% 
    str_remove_all(" ")
  
  date <- data_chr[1] %>% 
    str_remove("\\(.*\\)")
  
  tibble(prefecture, `報告数`, `定点当たり`, date)
}

df <- map_dfr(paste0("download/", dir("download/")), 
        convert_pdf2tibble) %>% 
  separate(date, c("year", "week"), 5, remove = FALSE) %>% 
  mutate(year = year %>% str_remove("年") %>% as.integer, 
         week = week %>% str_remove("週") %>% as.integer %>% 
           formatC)

# visualization -----------------------------------------------------------

df %>% 
  filter(prefecture != "総数") %>% 
  group_by(year, week) %>% 
  summarise(`全国` = mean(`定点当たり`)) %>% 
  drop_na() %>% 
  ggplot(aes(week, `全国`, colour = as.factor(year))) +
  geom_line(aes(group = as.factor(year)), size = 1) +
  geom_point() + 
  facet_grid(rows = vars(year), scales = "free")+
  scale_y_continuous(limits = c(0, NA))


df %>% 
  filter(prefecture != "総数") %>% 
  group_by(year, week) %>% 
  summarise(`全国` = sum(`報告数`)) %>% 
  drop_na() %>% 
  ggplot(aes(week, `全国`, colour = as.factor(year))) +
  geom_line(aes(group = as.factor(year)), size = 1) +
  geom_point() + 
  facet_grid(rows = vars(year), scales = "free")+
  scale_y_continuous(limits = c(0, NA))

df %>% filter(prefecture != "総数") %>%
  drop_na() %>% 
  mutate(week = week  %>% formatC(width = 2, flag = "0")) %>% 
  unite("year_week", year, week, sep = "") %>%
  mutate()
  group_by(year_week) %>% 
  summarise(`全国` = mean(`定点当たり`)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  ggplot(aes(year_week, `全国`)) + 
  geom_line(aes(group = 1), size = 1) +
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
  