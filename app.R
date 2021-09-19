

library(shiny)
library(shinymaterial)
library(dplyr)
library(reactable)
library(leaflet)


file_path <- "https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/locations-of-interest/august-2021/locations-of-interest.csv"

locations_of_interest <-
  readr::read_csv(pins::pin(file_path)) %>%
  mutate(
    Start_time = as.POSIXct(Start, format = "%d/%m/%Y, %I:%M %p"),
    End_time = as.POSIXct(End, format = "%d/%m/%Y, %I:%M %p"),
    Added_Date = lubridate::dmy(sapply(strsplit(Added, " "), function(x)
      x[1])),
    Added_Time = lubridate::hm(sapply(strsplit(Added, " "), function(x)
      x[2])),
    info =  paste0(
      "<H3>Event infomation</H3>",
      "<strong>Event</strong>: ", Event,
      "<br>",
      "<strong>Location</strong>: ", Location,
      "<br>",
      "<strong>City</strong>: ", City,
      "<br>",
      "<strong>Start date/time</strong>: ", Start,
      "<br>",
      "<strong>End date/time</strong>: ", End,
      "<br>",
      "<strong>Advice</strong>: ", Advice,
      "<br>"
    ),
    public_transport = ifelse(is.na(LAT), TRUE, FALSE)
  ) 

# For different date formats 
locations_of_interest$Added_Date[is.na(locations_of_interest$Added_Date)] <-
  lubridate::ymd(sapply(strsplit(locations_of_interest$Added[
    is.na(locations_of_interest$Added_Date)], " "), function(x) x[1]))

locations_of_interest$Added_Time[is.na(locations_of_interest$Added_Time)] <-
  lubridate::hms(sapply(strsplit(locations_of_interest$Added[
    is.na(locations_of_interest$Added_Time)], " "), function(x) x[2]))

# Use the initial date for missing entries
locations_of_interest$Added_Date[is.na(locations_of_interest$Added_Date)] <-
  min(locations_of_interest$Added_Date, na.rm = TRUE) - 1


# Define UI for application that draws a histogram
ui <- material_page(
    title = "COVID-19",

    nav_bar_color = "teal lighten-1", 

    h4("Contact tracing locations of interest"),
    
    material_side_nav(
        image_source = "https://cdn.auckland.ac.nz/aem/content/auckland/en/news/2021/05/19/lessons-from-our-covid-tracing-app/jcr:content/leftpar/imagecomponent/image.img.1024.medium.jpg/1621379069999.jpg",
        p("Please refer to the official ",
            a("Ministry of Health website's locations of interest",
              href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-health-advice-public/contact-tracing-covid-19/covid-19-contact-tracing-locations-interest"),
            " for the most up to date list of locations."
        ),
        
        
        p(
            "Map and table are based from the data in the Ministry of Health's COVID-19: Contact tracing locations of interest in ",
            a("here",
              href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-health-advice-public/contact-tracing-covid-19/covid-19-contact-tracing-locations-interest"),
            ", which is under the Ministry of Health's ",
            a("creative commons license",
              href = "https://www.health.govt.nz/about-site/copyright")
        ),
        h5("Contact:"),
        h6(a("Kevin Chang", href = "mailto:kevin.ct.chang@gmail.com")),
        p(
            "Source code can be founded in ",
            a("here",
              href = "https://github.com/kcha193/covid_locations")
        ),
    ), 
    
    p("Ministry of Health Data was last updated at", 
      strong(max(locations_of_interest$Added_Date + locations_of_interest$Added_Time, na.rm = TRUE))),


    material_row(
        material_column(
            width = 3,
            material_dropdown(
                input_id ="added",
                label = "Date new location added",
                choices = c("All",
                            as.character(sort(
                                unique(locations_of_interest$Added_Date),
                                decreasing = TRUE
                            ))),
                selected = "All",
                color = "blue"
            )
        ),
        material_column(
            width = 3,
            material_dropdown(
                input_id =  "city",
                label = "City",
                choices = c("All", sort(unique(locations_of_interest$City))),
                selected = "All",
                color = "blue"
            )
        ),
       
            
        material_column(
            width = 3,
            material_dropdown(
                input_id = "start_time",
                label = "Event Start Date",
                choices = c("All",
                            as.character(sort(
                                unique(as.Date(locations_of_interest$Start_time)),
                                decreasing = TRUE
                            ))),
                selected = "All"
            )
        )
        ,
        
        material_column(
            width = 3,
            
            material_checkbox(
                "public_transport", 
                strong("Public Transport only (Nothing will display on the map)"), 
                initial_value = FALSE)
        )
        
    ),
    
    material_row(
        material_column(
            width = 6,
            material_card(
                title = "Locations of interest map (Click on circle markers for more info)",
                leafletOutput("map")
            )
        ),
        material_column(
            width = 6,
            material_card(
                title = "Locations of interest table",
                reactableOutput("table")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  
    new_location_data <-
        reactive({
            req(input$start_time)
            
            if (input$public_transport) {
                locations_of_interest <-
                    locations_of_interest %>%
                    filter(public_transport == TRUE)
            }
            
            
            if (input$city != "All") {
                locations_of_interest <-
                    locations_of_interest %>%
                    filter(City == input$city)
            }
            
            if (input$added != "All") {
                locations_of_interest <-
                    locations_of_interest %>%
                    filter(Added_Date == input$added)
            }
            
            
            if (input$start_time != "All") {
                locations_of_interest <-
                    locations_of_interest %>%
                    filter(as.Date(Start_time) == input$start_time)
            }
            
            return(locations_of_interest)
        })
    
     
    
    
    output$map <-
        renderLeaflet({
            
            # material_spinner_show(session, "map")
            # 
            # Sys.sleep(2.5) # sleep to show spinner example longer 
            
            new_location_data() %>%
                leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
                addTiles() %>%
                addCircleMarkers(lng = ~ LNG,
                                 lat = ~ LAT,
                                 popup = ~ info)
        })
    
    
    output$table <-
        renderReactable({
            # material_spinner_show(session, "table")
            # 
            # Sys.sleep(2.5) # sleep to show spinner example longer 
            
            new_location_data() %>%
                select(Added_Date, Event:End) %>%
                arrange(desc(Added_Date)) %>%
                reactable(defaultPageSize = 5, searchable = TRUE)
        })      

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
