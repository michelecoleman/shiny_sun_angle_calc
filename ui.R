# UI for sun_calc program. Displays the time of day (in local timezone) that 
# the sun rises and falls through a given angle above the horizon, for 
# different days of the year, based on location

library(shiny)
library(dygraphs)

# Define UI 
shinyUI(fluidPage(
    titlePanel("Sun height calculator"),
    
    fluidRow(
        column(8, p("Choose a location and an angle above the horizon to see when the sun passes through that height (rising and setting) thoughout the year, in the local timezone."))
    ),
    
    sidebarLayout(
    
    # Sidebar with controls 
    sidebarPanel(
        uiOutput("choose_city"),
        helpText("Location help: If the city you're interested in isn't on the list, choose a place with a similar latitude — it will have a similar profile (modulo daylight saving time practices)."),
        
        sliderInput("angle", "Angle above horizon:", 
                    min=0, max=85, value=50, ticks=FALSE),
        helpText("Angle help: 0° means the sun is at the horizon, corresponding to sunrise/sunset. 90° would be straight overhead. (To avoid degenerate result, maximum angle is 85°.)")
        
    ), # end of sidebarPanel
    
    ## MAIN PANEL ##
    mainPanel(
        h4("Sun crossings, 2015:"),
        textOutput("resultText"),
        dygraphOutput("sunPlot")
    ) # end of mainPanel  
    
))) #end of ui.R