##' @title Download Polish Historical Climate Data
##'
##' @description Download data for Polish climate stations by station ID.
##'
##' @param siteCode character; Station code - list at \url{https://dane.imgw.pl/1.0/stacje}.
##' @param variableCode character; Variable code - list at \url{https://dane.imgw.pl/1.0/klasyfikacje}.
##' @param startDate Date.
##' @param endDate Date.
##' @param baseURL character; probably doen't need changing.
##' @param authentic character; authentification 
##' @param sleep numeric; How long to sleep between each request.

##' @details Authentification can be obtained from
##' \url{https://dane.imgw.pl/rejestracja/} by provinding your email address.
##' The authentification string should be formatted as
##' "my.email@host.no:Pa55w0rd"

##' The server limits the number of queries per client to 1000 per 10 minutes.
##' Default value of sleep gives a rate well below this
##' 
##' @return A \code{data.frame} with climate data
##'   
##' @import dplyr
##' @importFrom assertthat assert_that
##' @importFrom readr read_delim
##' @importFrom RCurl getURL
##'   
##' @export
##' 
##' @author Richard J. Telford


get_Polish_weather_data <- function(siteCode, variableCode, startDate, endDate, baseURL = "https://dane.imgw.pl/1.0/pomiary/cbdh/", authentic, sleep = 0.9){
get_Polish_weather_data <- function(siteCode, variableCode, startDate, endDate, baseURL = "https://dane.imgw.pl/1.0/pomiary/cbdh/", authentic, sleep = 0.9){
get_Polish_weather_data <- function(siteCode, variableCode, startDate, endDate, baseURL = "https://dane.imgw.pl/1.0/pomiary/cbdh/", authentic, sleep = 0.9){
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
    Sys.sleep(sleep)#don't hit server too hard
    read_delim(getURL(url, userpwd = authentic), delim = ";")
  }, .progress = "text")
  
  res %>%  
    rename(value = wartosc, date = data) %>%
    mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>% 
    filter(date >= startDate, date <= endDate) 
}
