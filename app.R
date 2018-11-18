#### Libraries ####

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(magrittr)
library(leaflet)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

##### Functions ######
filterLocations <- function(aed_locations, current_lat, current_long, search_radius = 25, increment_radius = 25){ ### Filter locations of AED within 5km from users location
  
  tmp <- 
    aed_locations %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(., distance = 0.001*geosphere::distHaversine(c(current_lat, current_long),c(lat, long))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(distance <= search_radius) %>%
    dplyr::arrange(., distance)
  
  if(nrow(tmp) != 0){
    return(tmp)
  }
  else{
    sprintf("Incrementing search radius, currently %f", search_radius) %>% print()
    return(filterLocations(aed_locations, current_lat, current_long, search_radius + increment_radius))
  }
  
}

###### User Interface #######
ui <- fluidPage(
  
  title = "first AED kit",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customcss.css")
  ),
  
  tags$script(' $(document).ready(function () { navigator.geolocation.getCurrentPosition(onSuccess, onError);
              function onError (err) { Shiny.onInputChange("geolocation", false); }
              function onSuccess (position) { 
              setTimeout(function () { var coords = position.coords; console.log(coords.latitude + ",
              " + coords.longitude); Shiny.onInputChange("geolocation", true); Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude); }, 1100) } }); '), ### Get users location
  
  
  fluidRow(
    h1(icon("heartbeat"),"first AED kit")),
  
  sidebarPanel(
    
    radioButtons(inputId = "type_of_transport", label = "Do you have a car?", choices = c("Yes", "No"), selected = "Yes"), ### Users input communication type
    actionButton(inputId = "plot_route", label = "HELP!", style="color: #fff; background-color: crimson; border-color: crimson"),
    htmlOutput("dist"),
    uiOutput("time"),
    fluidRow(
      valueBoxOutput("distBox"),
      valueBoxOutput("timeBox")
    )
  ),
  
  mainPanel(
    leafletOutput("basic_map"),
    h2(textOutput(("desc_tit"))),
    uiOutput("description"),
    textOutput("test")
  )
  )
##### Server Side ##########
server <- function(input, output) {
  
  AEDdata <- reactive({ ### Create object that contains AED adress locations from csv file
    
    data_aed <- read.csv("data_updated.csv")
    return(data_aed)
  })
  
  my_location <- reactive({ ### Get users locations
    
    if (is.null(input$lat) & is.null(input$long)){
      
      my_loc <- data.frame(lat = 50.02583, long = 19.90379)
      return(my_loc)
    } else {
      
      my_loc <- data.frame(lat = input$lat, long = input$long) 
      return(my_loc)
    }
  })
  
  cut_locations <- reactive({
    
    nearest_locations <- filterLocations(AEDdata(), my_location()$lat, my_location()$long)
    return(nearest_locations)
  })
  
  map_inputs <- reactive({
    
    mapIcons <- iconList(
      green_icon <- leaflet::makeIcon("Green.png",iconWidth = 32, iconHeight = 32),
      red_icon <- leaflet::makeIcon("Red.png",iconWidth = 32, iconHeight = 32)
    )
    
    # Reading your location
    your_pos <- data_frame(name = "You are here", lat = my_location()$lat, long = my_location()$long)
    
    # Binding location
    aed_pos <- filterLocations(AEDdata(), my_location()$lat, my_location()$long, 5)
    final_pos <- bind_rows(aed_pos, your_pos) %>% 
      mutate(address = address %>%
               as.character())
    
    labs <- lapply(seq(nrow(final_pos)), function(i) {
      paste0( '<p><h3>', "AED's location: </h3></p><p>", 
              final_pos[i, "name"],"</p>") 
    })
    labs_actual_pos <- lapply(seq(nrow(final_pos)), function(i) {
      paste0( '<p><h3>', "You are here! </h3></p>")
    })
    
    map_inputs <- list(mapIcons = mapIcons,
                       final_pos = final_pos,
                       labs = labs,
                       labs_actual_pos = labs_actual_pos)
    return(map_inputs)
  })
  
  output$basic_map <- renderLeaflet({
    
    basic_map_tmp <- leaflet(map_inputs()$final_pos) %>%
      setView(lng = map_inputs()$final_pos$long[nrow(map_inputs()$final_pos)], lat = map_inputs()$final_pos$lat[nrow(map_inputs()$final_pos)], zoom = 13) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng = ~long[1:(nrow(map_inputs()$final_pos)-1)], lat = ~lat[1:(nrow(map_inputs()$final_pos)-1)], popup = paste("<b>","AED's location: ", "</b>", "<br>", 
                                                                                                                                map_inputs()$final_pos$name[1:(nrow(map_inputs()$final_pos)-1)],"<b><br>","Address: ", 
                                                                                                                                "</b>", "<br>", map_inputs()$final_pos$address[1:(nrow(map_inputs()$final_pos)-1)]),
                 label =  lapply(map_inputs()$labs[1:(nrow(map_inputs()$final_pos)-1)], HTML), icon = map_inputs()$mapIcons[[1]]) %>%
      addMarkers(lng = ~long[nrow(map_inputs()$final_pos)], lat = ~lat[nrow(map_inputs()$final_pos)], popup = map_inputs()$final_pos$name[nrow(map_inputs()$final_pos)], 
                 label =  lapply(map_inputs()$labs_actual_pos[nrow(map_inputs()$final_pos)], HTML), icon = map_inputs()$mapIcons[[2]]) 
    
    return(basic_map_tmp)
  })
  
  fastest_way <- reactive({
    
    app_id <- "va6Psz5oPSUztoDEu1NV"
    app_code <- "xeULHwPc_ab9QHZ2PMZmBA"
    
    if(input$type_of_transport == "Yes") {
      
      leng <- nrow(cut_locations())
      
      if(leng > 3) {
        leng <- 3
      } 
      
      poss_ways <- data.frame(AED = rep(1:leng, each = 2),
                              Type = rep(c("car", "pederastian"), times = leng))
    } else {
      
      leng <- nrow(cut_locations())
      
      if(leng > 3) {
        leng <- 3
      }
      
      poss_ways <- data.frame(AED = 1:leng,
                              Type = rep("pedestrian", times = leng))
    }
    
    data_parsed_tmp <- list()
    time_tmp <- data.frame(time = rep(NA, 6), id = rep(NA, 6), type = rep(NA, 6))
    
    for(i in 1:nrow(poss_ways)) {
      
      data_tmp <- httr::GET("https://route.api.here.com/routing/7.2/calculateroute.json",
                            query = list(app_id = app_id,
                                         app_code = app_code,
                                         waypoint0 = paste0("geo!", my_location()$lat, ",", my_location()$long),
                                         waypoint1 = paste0("geo!", cut_locations()$lat[poss_ways$AED[i]], ",", cut_locations()$long[poss_ways$AED[i]]),
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
    }
    
    time_tmp %<>% tidyr::drop_na()
    
    data_parsed <- data_parsed_tmp[[time_tmp$id[which(min(time_tmp$time) == time_tmp$time)]]]
    
    shape <- cbind(data_parsed$response$route[[1]]$shape) %>% 
      stringr::str_split_fixed(pattern = ",", n = 2) %>%
      base::as.data.frame() %>%
      stats::setNames(c("lat", "long")) %>%
      dplyr::mutate_all(.funs = function(x) { as.numeric(as.character(x)) })
    
    start_point <- data.frame(lat = data_parsed$response$route[[1]]$waypoint[[1]]$originalPosition$latitude,
                              long = data_parsed$response$route[[1]]$waypoint[[1]]$originalPosition$longitude) %>%
      dplyr::mutate_all(.funs = function(x) { as.numeric(as.character(x)) })
    
    end_point <- data.frame(lat = data_parsed$response$route[[1]]$leg[[1]]$end$originalPosition$latitude,
                            long = data_parsed$response$route[[1]]$leg[[1]]$end$originalPosition$longitude) %>%
      dplyr::mutate_all(.funs = function(x) { as.numeric(as.character(x)) })
    
    all_points <- rbind(start_point, shape, end_point)
    
    mapIcons <- iconList(
      green_icon <- leaflet::makeIcon("Green.png",iconWidth = 32, iconHeight = 32),
      red_icon <- leaflet::makeIcon("Red.png",iconWidth = 32, iconHeight = 32)
    )
    
    distance <- as.numeric(data_parsed$response$route[[1]]$summary$distance) / 1000
    exp_time <- ceiling(x = as.numeric(data_parsed$response$route[[1]]$summary$trafficTime) / 60)
    description <- sapply(1:length(data_parsed$response$route[[1]]$leg[[1]]$maneuver), function(x) { data_parsed$response$route[[1]]$leg[[1]]$maneuver[[x]]$instruction }) %>%
      paste0(collapse = " ")
    
    navigation <- list(start_point = start_point,
                       end_point = end_point,
                       all_points = all_points,
                       mapIcons = mapIcons,
                       distance = distance,
                       exp_time = exp_time,
                       description = description)
    
    return(navigation)  
  })
  
  observeEvent(input$plot_route, {
    
    leafletProxy("basic_map", data = fastest_way()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addTiles() %>%
      addPolylines(lat = fastest_way()$all_points$lat, lng = fastest_way()$all_points$long) %>%
      addMarkers(lat = fastest_way()$start_point$lat, lng = fastest_way()$start_point$long, icon = fastest_way()$mapIcons[[2]]) %>%
      addMarkers(lat = fastest_way()$end_point$lat, lng = fastest_way()$end_point$long, icon = fastest_way()$mapIcons[[1]])
    
    #    output$time <- renderText({
    #     
    #      distance <- fastest_way()$distance
    #      expected_time <- fastest_way()$exp_time
    #      
    #      if (!is.null(distance) & !is.null(expected_time)) {
    #        
    #        txt_tb_displayed <- paste0("You will be there in ", expected_time, " minutes. Distance to go through equals ", distance, "km.")
    #        return(expected_time)
    #        #return(txt_tb_displayed)
    #      } else {
    #        return(NULL)
    #      }
    #    })
    
    
    output$timeBox <- renderInfoBox({
      expected_time <- fastest_way()$exp_time
      if (!is.null(expected_time)) {
        infoBox(
          "Estimated time:", paste(expected_time, "mins"), icon = icon("clock"),
          color = "yellow"
        )
      } else {
        return(NULL)
      }
    })  
    #    output$dist <- renderText({
    #      
    #      distance <- fastest_way()$distance
    #      expected_time <- fastest_way()$exp_time
    #      
    #      if (!is.null(distance) & !is.null(expected_time)) {
    #        
    #        txt_tb_displayed <- paste0("You will be there in ", expected_time, " minutes. Distance to go through equals ", distance, "km.")
    #        return(distance)
    #        #return(txt_tb_displayed)
    #      } else {
    #        return(NULL)
    #      }
    #    })
    
    output$distBox <- renderInfoBox({
      distance <- fastest_way()$distance
      if (!is.null(distance)) {
        infoBox(
          "Distance:", paste(distance, "km"), icon = icon("road"),
          color = "yellow"
        )
      } else {
        return(NULL)
      }
    })
    
    output$description <- renderUI({
      
      description_tmp <- fastest_way()$description
      
      if (!is.null(description_tmp)) {
        route_title <- "Your route:"
        des_tb_displayed <- description_tmp
        return(HTML(des_tb_displayed))
      } else {
        return(NULL)
      }
    })
    output$desc_tit <- renderText({
      
      description_tmp <- fastest_way()$description
      
      if (!is.null(description_tmp)) {
        route_title <- "Your route:"
        return(route_title)
      } else {
        return(NULL)
      }
    })
    
  })
  
} 

shinyApp(ui = ui, server = server)