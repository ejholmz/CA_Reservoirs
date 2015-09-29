library(shiny)
library(leaflet)
library(htmlwidgets)
library(shiny)

headerPanel_2 <- function(title, h, windowTitle=title){    
  tagList(
    tags$head(tags$title(windowTitle)),
    h(title)
  )
}

shinyUI(fluidPage(
  verticalLayout(
    headerPanel_2(
      HTML(
        '<div id="stats_header">
        <a href="http://watershed.ucdavis.edu" target="_blank">
        <left>
        <img id="cws_logo" alt="CWS Logo" src="https://watershed.ucdavis.edu/files/cwsheader_0.png" />
        </left>
        </a>
        </div>'
      ), h3, NULL),
    
    titlePanel("Major California Reservoirs"), 
    
    ##Input Options
    
    mainPanel(tabsetPanel(
      
      tabPanel("Interactive Map",
               div(class="outer",
                   
                   leafletOutput("map", width="100%", height="730"),
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                 width = 250, height = 400,
                                 plotOutput("Hist")
                   )
               )
               
               
               
      ),
      
      ## Time series graphs
      
      tabPanel("Time Series Graphs",
               verticalLayout(
                 wellPanel(selectInput(inputId = "selectRes", label = "Choose Reservoir", unique(resdata$Reservoir)),
                           dateRangeInput("range", "Date Range:", start = "1998-01-01", end = Sys.Date(),
                                          min = "1998-01-01",max = Sys.Date(), startview = "year"),
                           checkboxInput(inputId = "check", label = "Display vertical line", value = FALSE),
                           
                           conditionalPanel("input.check == true", 
                                            checkboxInput(inputId = "level", label = "Display horizontal line", value = FALSE),
                                            dateInput(inputId = "vline", label = "Vertical line date",value = Sys.Date() - 11)
                           )
                 ),
                 
                 mainPanel(plotOutput("Res",height = 450), width=12),
                 
                 textOutput("Current"),
                 conditionalPanel("input.check == true", textOutput("vlinetext"))
                 
               )
      )
      
    ), 
    width = 12)
  )
)
)



