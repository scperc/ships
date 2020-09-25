#load required packages
library(shiny)
library(dplyr)
library(geosphere)
library(leaflet)

#read data: only required fields to decrease memory load
dat = read.csv(
    "data/ships.csv",
    colClasses = c(
        NA,
        NA,
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        NA,
        NA,
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        NA,
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL",
        "NULL"
    )
)


#UI for dropdown module
dropdownUI <- function(id) {
    tagList(
        selectInput(NS(id,"shiptype"), "Select Vessel Type",
                    sort(unique(dat$SHIPTYPE))),
        selectInput(NS(id,"shipname"), "Select a Vessel",
                    sort(unique(subset(dat,SHIPTYPE=="0")$SHIPNAME)), selected = NULL)
    )
}


#Server for dropdown module
dropdownServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        #update shipname input based on shiptype
        observe({
            updateSelectInput(session, "shipname", 
            choices = sort(unique(
                subset(dat, SHIPTYPE %in% input$shiptype)$SHIPNAME
            )))
        })
        
        #output shipname as moduleServer result
        reactive(input$shipname)
    })
}

#create User Interface
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            dropdownUI("prim")
        ),
        mainPanel(
            leafletOutput("map"),
            h3(textOutput("text"))
        )
    )
)

server <- function(input, output, session) {
    #bring in shipname form module
    shipname <- dropdownServer("prim")
    
    #row with shop observations
    shipObs <- reactive({
        ship_dat = subset(dat, SHIPNAME == shipname()) %>%
            arrange(DATETIME) %>%
            mutate(NEXT_LAT = c(LAT[-1], NA),
                   NEXT_LON = c(LON[-1], NA))   #create lagged lat and lon variable
        
        #calculate the haversine distance between the points, in meters
        ship_dat$DIST = distHaversine(ship_dat[c("LON", "LAT")], ship_dat[c("NEXT_LON", "NEXT_LAT")])
        
        #return furthest distance, break tie with datetime
        arrange(ship_dat, desc(DIST), desc(DATETIME))[1,]
    })
    
    #create map
    output$map <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers(
                lng = c(shipObs()$LON, shipObs()$NEXT_LON),
                lat = c(shipObs()$LAT, shipObs()$NEXT_LAT),
                label = c("Start Location", "Stop Location")
            )
        
    })
    
    #format text and check for edge cases
    output$text <-
        renderText({
            ifelse(
                is.na(shipObs()$DIST),
                paste0(
                    shipObs()$SHIPNAME,
                    " had only one observation, so no distance can be calculated"
                ),
                paste0(
                    shipObs()$SHIPNAME,
                    " traveled  ",
                    format(round(shipObs()$DIST, 2),  big.mark = ","),
                    " meters."
                )
            )
        })
}


#launch app
shinyApp(ui = ui, server = server)
