get_Polish_weather_data <- function(siteCode, variableCode, startDate, endDate, baseURL = "https://dane.imgw.pl/1.0/pomiary/cbdh/", authentic){
  require("RCurl")
  require("readr")
  require("assertthat")
  require("dplyr")
  
  assert_that(is.Date(startDate))
  assert_that(is.Date(endDate))
  assert_that(startDate < endDate)# check dates in order

  url0 <- paste0(baseURL, siteCode, "-", variableCode, "/tydzien/")
  res <- plyr::ldply(seq(from = startDate, to = endDate, by = 7), function(D){#D is date
    url <- paste0(url0, D,"?format=csv")
    Sys.sleep(0.9)#don't hit server too hard
    read_delim(getURL(url, userpwd = authentic), delim = ";")
  }, .progress = "text")
  
  res %>%  
    rename(value = wartosc, date = data) %>%
    mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>% 
    filter(date >= startDate, date <= endDate) 
}
