
library(httr)
library(tidyverse)
library(jsonlite)

resp <- GET("https://www.breakingbadapi.com/api/episodes")
# Wrapped into a function
sw_api <- function(episodeUrl) {
  resp <- httr::GET(episodeUrl)
  httr::stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("https://www.breakingbadapi.com/api/episodes")
resp

rawToChar(resp$content)


resp %>% 
  .$content %>% 
  rawToChar() %>% 
  jsonlite::fromJSON()


