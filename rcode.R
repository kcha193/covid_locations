

library(tidyverse)
library(lubridate)
library(leaflet)


locations_of_interest <- 
 rgdal::readOGR("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/locations-of-interest/august-2021/locations-of-interest.geojson") %>% 
 as_tibble()



locations_of_interest$Start_time <-
 as.POSIXct(locations_of_interest$Start,
            format = "%d/%m/%Y, %I:%M %p")
           
locations_of_interest$End_time <-
 as.POSIXct(locations_of_interest$End,
            format = "%d/%m/%Y, %I:%M %p")
                    
      
locations_of_interest$info <- paste0(
 "<H3>Event infomation</H3>",
 "<strong>Event</strong>: ",
 locations_of_interest$Event,
 "<br>",
 "<strong>Location</strong>: ",
 locations_of_interest$Location,
 "<br>",
 
 "<strong>City</strong>: ",
 locations_of_interest$City,
 "<br>",
 
 "<strong>Start date/time</strong>: ",
 locations_of_interest$Start,
 "<br>",
 
 "<strong>End date/time</strong>: ",
 locations_of_interest$End,
 "<br>",
 
 "<strong>Advice</strong>: ",
 locations_of_interest$Advice,
 "<br>"
)




locations_of_interest %>%
 leaflet() %>%
 addTiles() %>%
 addCircleMarkers(lng = ~ coords.x1,
                  lat = ~ coords.x2,
                  popup = ~ info)




















