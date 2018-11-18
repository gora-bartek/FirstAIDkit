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
ui <- dashboardPage(
  dashboardHeader(title = "helpR"), ### Display projects name
  dashboardSidebar(
    radioButtons(inputId = "type_of_transport", label = "Chose mode of transportation", choices = c("Foot", "Bike", "Public Transport", "Car")), ### Users input communication type
    actionButton(inputId = "plot_route", label = "HELP!")
  ),
  dashboardBody(
    fluidPage(
      leafletOutput("basic_map"),
      textOutput("fastest_way")### Displays leaflet map
    )
  ),
  
  tags$script(' $(document).ready(function () { navigator.geolocation.getCurrentPosition(onSuccess, onError);
                function onError (err) { Shiny.onInputChange("geolocation", false); }
                function onSuccess (position) { 
              setTimeout(function () { var coords = position.coords; console.log(coords.latitude + ",
              " + coords.longitude); Shiny.onInputChange("geolocation", true); Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude); }, 1100) } }); ') ### Get users location
  
)
##### Server Side ##########
server <- function(input, output) {
  
  AEDdata <- reactive({ ### Create object that contains AED adress locations from csv file
    
    data_aed <- read.csv("data_updated.csv")
    return(data_aed)
  })

  my_location <- reactive({ ### Get users locations
    
    my_loc <- data.frame(lat = 50.0892785, long = 19.9795336)
    return(my_loc)
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
    
    map_inputs <- list(mapIcons = mapIcons,
                       final_pos = final_pos,
                       labs = labs)
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
                 label =map_inputs()$final_pos$name[nrow(map_inputs()$final_pos)], icon = map_inputs()$mapIcons[[2]]) 
    
    return(basic_map_tmp)
  })
  
  fastest_way <- reactive({
    
    app_id <- "va6Psz5oPSUztoDEu1NV"
    app_code <- "xeULHwPc_ab9QHZ2PMZmBA"
    
    data_tmp <- httr::GET("https://route.api.here.com/routing/7.2/calculateroute.json",
                          query = list(app_id = app_id,
                                       app_code = app_code,
                                       waypoint0 = paste0("geo!", my_location()$lat[1], ",", my_location()$long[1]),
                                       waypoint1 = paste0("geo!", cut_locations()$lat[1], ",", cut_locations()$long[1]),
                                       mode = "fastest;car",
                                       routeattributes = "sh"))
    
    data_parsed <- httr::content(data_tmp, as = "parsed")
    
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
    
    navigation <- list(start_point = start_point,
                       end_point = end_point,
                       all_points = all_points,
                       mapIcons = mapIcons)
    
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
  })
}

shinyApp(ui = ui, server = server)