get_csvLink <- function(path) {
  Sys.sleep(1)
  read_html(path) %>%
    html_nodes(xpath = "//p[@class = 'body1']") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("-teiten\\.csv$") %>%
    {paste0("https://www.niid.go.jp", .)}
}

download_csv <- function(path) {
  Sys.sleep(1)
  download.file(path, paste0("download_niid/", path %>%
                               str_extract("\\d{4,4}-\\d\\d-teiten\\.csv$")))
}

format_csv_flu <- function(path) {
  raw_csv <- read.csv(path, header = TRUE, sep = ",")
  raw_csv %>% as_tibble() %>% 
    select(1:3) %>% 
    .[-c(1:3),] %>% 
    .[-49,] %>% 
    set_colnames(c("prefecture", "number", "fixed_point")) %>% 
    mutate(number = number %>% str_replace("(-|?c)", "0") %>% as.integer, 
           fixed_point = fixed_point %>% str_replace("(-|?c)", "0") %>% as.double) %>% 
    mutate(date = raw_csv[1,1] %>% as.character) %>% 
    separate(date, c("num_week", "date"), sep = 8) %>% 
    mutate(date = str_remove_all(date, "(\\(|\\))"))
  
}

update_niid <- function() {
  
  link_latest <- get_csvLink("https://www.niid.go.jp/niid/ja/data.html")
  
  links_csv_candi <- map_chr(paste0("https://www.niid.go.jp/niid/ja/data.html?start=", 1:6), 
                             get_csvLink) %>% 
    c(link_latest)
  
  new_csv <- links_csv_candi %>% str_extract("\\d{4,4}-\\d\\d-teiten\\.csv$") %>% setdiff(dir("download_niid/"))
  
  if (new_csv %>% length > 0) {
    walk(new_csv %>% 
           map_chr(~str_subset(links_csv_candi, .)), 
         download_csv)
  
  
  df_old <- df
  
  df <- 
    df %>% 
    bind_rows(
      map_dfr(paste0("download_niid/",new_csv), 
              format_csv_flu) %>%
        separate(num_week, c("year", "week"), sep = 5, remove = FALSE) %>%
        mutate(year = year %>% str_remove("\\D") %>% as.integer,
               week = week %>% str_remove("\\D") %>% as.integer)
    ) %>% 
    arrange(year, week) %>% 
    distinct(prefecture, number, fixed_point, num_week, year, week, date)
  
  write_csv(df, "data.csv")
  write_csv(df_old, paste0("data_old/data_", Sys.time() %>% str_remove(":\\d\\d \\D+") %>% str_remove_all(":"), ".csv"))
  }
}

get_new_week <- function() {
  get_csvLink("https://www.niid.go.jp/niid/ja/data.html") %>% 
    str_extract("\\d{4,4}-\\d\\d-teiten\\.csv$") %>% str_remove("-teiten\\.csv")
    
}



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
