app_id <- "va6Psz5oPSUztoDEu1NV"
app_code <- "xeULHwPc_ab9QHZ2PMZmBA"

possible_ways <- data.frame(Type = rep(x = c("car", "pedestrian"), times = 3), AED = c(rep(x = 1:3, each =2)), stringsAsFactors = F)

cut_locations <- read.csv("data_updated.csv")

data_parsed <- list()
time_tmp <- data.frame(time = rep(NA, 6), id = rep(NA, 6), type = rep(NA, 6))
  
data_tmp <- httr::GET("https://route.api.here.com/routing/7.2/calculateroute.json",
                        query = list(app_id = app_id,
                                     app_code = app_code,
                                     waypoint0 = paste0("geo!", 50.02583, ",", 19.90379),
                                     waypoint1 = paste0("geo!", cut_locations$lat[possible_ways$AED[i]], ",", cut_locations$long[possible_ways$AED[i]]),
                                     mode = "fastest;pedestrian",
                                     routeattributes = "sh"))


  
  data_parsed <- httr::content(data_tmp, as = "parsed")
  
  shape2 <- cbind(data_parsed$response$route[[1]]$shape) %>% 
    stringr::str_split_fixed(pattern = ",", n = 2) %>%
    base::as.data.frame() %>%
    stats::setNames(c("lat", "long")) %>%
    dplyr::mutate_all(.funs = function(x) { as.numeric(as.character(x)) })
  
  if(possible_ways$Type[i] == "car") {
    time_tmp$time[i] <- as.numeric(data_parsed[[i]]$response$route[[1]]$summary$trafficTime)
    time_tmp$id[i] <- i
    time_tmp$type[i] <- possible_ways$Type[i]
  } else { 
    time_tmp$time[i] <- as.numeric(data_parsed[[i]]$response$route[[1]]$summary$travelTime)
    time_tmp$id[i] <- i
    time_tmp$type[i] <- possible_ways$Type[i]
  }
}


############# WEZCIE TO PRZEKMINCIE ###############
# w skorice chodzi o to, żeby wybierac na podstawie tego najlepsza opcje transportu (na nogach, czy samochodem i ktory stalin)
# sądzę, żę najlepiej bedzie wyswietlic obie opcje do najblizszego defibrylatora, z tego wzgledu, ze od razu dajemy opcje uzytkownikowi,
# a nie musi sie zastanawiac i wybierac na poczatku, ze musi jechac tyle, a tyle
# jak wroce, dajcie znac na discordzie co myslicie, jestesmy w kontakcie
# bede gdzies do 8 spowrotem.
# GL&HFs

poss_ways <- data.frame(AED = 1:leng,
                        Type = rep("pedestrian", times = leng))
data_parsed_tmp <- list()
time_tmp <- data.frame(time = rep(NA, 6), id = rep(NA, 6), type = rep(NA, 6))
for(i in 1:nrow(poss_ways)) {
  
  data_tmp <- httr::GET("https://route.api.here.com/routing/7.2/calculateroute.json",
                        query = list(app_id = app_id,
                                     app_code = app_code,
                                     waypoint0 = paste0("geo!", 50.02583, ",", 19.90379),
                                     waypoint1 = paste0("geo!", cut_locations$lat[poss_ways$AED[i]], ",", cut_locations$long[poss_ways$AED[i]]),
                                     mode = paste0("fastest;", possible_ways$Type[i]),
                                     routeattributes = "sh"))
  
  data_parsed_tmp[[i]] <- httr::content(data_tmp, as = "parsed")

  if(possible_ways$Type[i] == "car") {
    time_tmp$time[i] <- as.numeric(data_parsed_tmp[[i]]$response$route[[1]]$summary$trafficTime)
    time_tmp$id[i] <- i
    time_tmp$type[i] <- possible_ways$Type[i]
  } else { 
    time_tmp$time[i] <- as.numeric(data_parsed_tmp[[i]]$response$route[[1]]$summary$travelTime)
    time_tmp$id[i] <- i
    time_tmp$type[i] <- possible_ways$Type[i]
  }
  
  time_tmp %<>% tidyr::drop_na()
}
