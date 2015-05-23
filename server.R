#server.r
# Server for sun_calc program. Displays the time of day (in local timezone) that 
# the sun rises and falls through a given angle above the horizon, for 
# different days of the year, based on location
library(shiny)
library(data.table)
library(dygraphs)
library(xts)
cities <- read.table('data/city_list.txt', header=TRUE, sep='\t',stringsAsFactors=FALSE)
cities = data.table(cities)

source('get_crossings.R')


# Define server logic 
shinyServer(function(input, output) {
    
    # Drop-down selection box for cities
    output$choose_city <- renderUI({        
        selectInput("cityChosen", "Choose a city", choices=cities$label)                  
    })
    
    
    # Generate text for the case when the sun never reaches the given
    # angle on any day of the year
    output$resultText <- renderText({
        if(is.null(input$cityChosen))
            return()
        
        if (is.null(crossings())) {
            i <- which(cities$label == input$cityChosen)
            angle <- input$angle
            city <- cities[i]$name
            paste0("The sun never gets as high as ",angle,'° in ',city,'!')
        }
    })
    
    # Actually produce the plot of the crossing times through the angle
    # for different dates throughout the year. 
    output$sunPlot <- renderDygraph({
        if (is.null(crossings())) {
            return()
        }
        dygraph(crossings())  %>% dyOptions(useDataTimezone = TRUE,connectSeparatedPoints = TRUE) %>%
            dyLegend(show="onmouseover")
    })

    # Reactive function to calculate the crossing time for the given
    # angle and location. This function is the workhorse of the entire app
    crossings <- reactive({
        if(is.null(input$cityChosen))
            return()
        
        i <- which(cities$label == input$cityChosen)
        lat <- cities[i]$lat
        long <- cities[i]$long
        timezone <- cities[i]$timezone
        angle <- input$angle
        yearstart <- as.POSIXct(strptime("01-01 00:00" , "%m-%d %H:%M"), tz=timezone)
        yearend  <- as.POSIXct(strptime("12-31 23:59" , "%m-%d %H:%M"), tz=timezone)

        
        # Call the zero finding function, defined in another file
        crossings <- data.table(get_crossings(angle,lat,long,timezone,yearstart, yearend))
        # If there are no crossings found, explicitly return NULL result
        if (nrow(crossings) < 1) {
            return() 
        }
        
        # The rest of this is just munging the date formatting to work 
        # with the plotting program. Different plotting packages would
        # require different munging steps at this point!
        crossings[, numtime := as.numeric(format(timestamp,format='%H',tz=timezone)) + as.numeric(format(timestamp,format='%M',tz=timezone))/60]
        
        # For dygraphs it worked best to split results into two data sets
        early <- crossings[partition=='early',.(timestamp,numtime)]
        late <- crossings[partition=='late',.(timestamp,numtime)]
        setnames(early,'numtime','rising')
        setnames(late,'numtime','setting')
        
        # dygraphs wants the output keyed by an "xts" data type
        # Finally it needs it cbinded back into a single object
        earlyxts <- xts(early,order.by=as.Date(early$timestamp,tz=timezone))
        latexts <- xts(late,order.by=as.Date(late$timestamp))
        cbind(earlyxts,latexts)

    })  # end of crossings reactive function definition
    
    
}) # end of shinyServer()
#end of server.R