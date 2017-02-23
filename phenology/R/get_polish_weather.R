get_Polish_weather_data <- function(siteCode, variableCode, startDate, endDate, baseURL = "https://dane.imgw.pl/1.0/pomiary/cbdh/", authentic){
  require("RCurl")
  require("readr")
  url0 <- paste0(baseURL, siteCode, "-", variableCode, "/tydzien/")
  res <- plyr::ldply(seq(from = startDate, to = endDate, by = 7), function(D){#D is date
    url <- paste0(url0, D,"?format=csv")
    Sys.sleep(0.9)#don't hit server too hard
    read_delim(getURL(url, userpwd = authentic), delim = ";")
  }, .progress = "text")
  
  res %>%  
    rename(value = wartosc, date = data) %>% 
    filter(date >= startDate, date <= endDate) %>%
    mutate(date = as.Date(format(date, "%Y-%m-%d")))
}
