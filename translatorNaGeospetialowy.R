x <- read.csv("data.csv")

adres_tmp <- paste(x$address, x$city) %>% 
  gsub(pattern = "ul. ", replacement = "") %>% 
  gsub(pattern = " ", replacement = "+") %>% 
  tolower()

for(i in 1:nrow(x)) {
  
  data_tmp <- httr::GET("https://geocoder.api.here.com/6.2/geocode.json",
                        query = list(app_id = app_id,
                                     app_code = app_code,
                                     searchtext = adres_tmp[i]))

  data_parsed <- httr::content(data_tmp, as = "parsed")
  unlisted_file <- lapply(data_parsed, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  x$lat[i] <- unlisted_file$Response["View.Result.Location.DisplayPosition.Latitude"]
  x$long[i] <- unlisted_file$Response["View.Result.Location.DisplayPosition.Longitude"]
}

write.csv(x, "data_updated.csv", row.names = F)
