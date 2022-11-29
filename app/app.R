# Libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)     #interactive map
library(htmlwidgets) #labels on interactive map
library(sf)
library(data.table)
library(randomForest)

# background data
all_modzcta <- readRDS("map.rds")
all_modzcta$DISTANCE <- round(all_modzcta$DISTANCE,3)
all_modzcta <- all_modzcta %>% st_as_sf(crs = 4326, sf_column_name = "geometry")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
    # Application title
    titlePanel("Trip distance calculator for Bogota's residents"),
    
    # Sidebar with a date input 
    sidebarLayout(
        sidebarPanel(
          tags$a(href="https://github.com/jpb2288/DS_project", "Data Repository", target="_blank"),
          
          h5("For prediction of your most likely trip distance choose the tap 'Distance calculator' on the right panel, select your profile with the controls below and press the 'Submit' button"),
          
          sliderInput("AGE", 
                      label = "Choose your age:", value = 35.0,
                      min = 15,
                      max = 80),
          
          selectInput("GENDER",
                      "Choose your gender:",
                      choices = unique(all_modzcta$GENDER)),
          
          selectInput("OCCUPATION",
                      "Choose your main activity:",
                      choices = unique(all_modzcta$OCCUPATION)),
          
          selectInput("MODE_T",
                      "Choose your preferred transportation mode:",
                      choices = unique(all_modzcta$MODE_T)),
          
          actionButton("submitbutton", "Submit", class = "btn btn-primary")
        ),
        
    # Main manel
    mainPanel(
      tabsetPanel(
        tabPanel("About", icon=icon("address-card"),
                 fluidRow(
                   column(width = 4, tags$br(),
                          tags$img(src="city.jpg", width =300 , height = 250),
                          tags$a("Photo from smart cities forum"), align = "center"),
                   column(width = 8, tags$br(),
                          tags$p("With this tool you can calculate your average trip distance, taking into account the mode of transport you have available (or you use most frequently).
                                 The distance is calculated according to the responses given in the Bogota mobility survey by people with sociodemographic variables similar to yours."),
                          tags$p("The map in the tap 'Distance calculator' shows the distance according to your characteristics and your house location in the city."),
                          tags$p("This project is inspired on the author previous work published as a paper on the journal of transport geography. It can be downloaded from https://doi.org/10.1016/j.jtrangeo.2020.102844"),
                          tags$p("In adition, this project was developed as final project for the Fall 2022 course CS551- Data Science of the University of Alabama"),
                          tags$img(src="UA_logo.png", width =200 , height = 40)
                   ))),
        tabPanel("Distance calculator", icon=icon("map"), 
                     leafletOutput("DISTANCE"),
                     tags$label(h3('Trip distance (km)')),
                     verbatimTextOutput('contents'),
                     tableOutput('tabledata')
                     )          
            )
          )
        )
    )

# Define server logic



# Read in the RF model
model <- readRDS("model1.rds")

server <- function(input, output) {
  
  week_zcta <- reactive({
    w <- all_modzcta %>% filter(GENDER     == input$GENDER,
                                OCCUPATION == input$OCCUPATION,
                                MODE_T     == input$MODE_T)
    return(w)
  })
  
  datasetInput <- reactive({
    
    df <- data.frame(
      Name = c("GENDER",      "AGE",     "OCCUPATION",     "INCOME", "MODE_T",     "EntScore"),
      Value = c(input$GENDER, input$AGE, input$OCCUPATION, "Middle", input$MODE_T, 0.7629203),
      stringsAsFactors = FALSE)

    input <- data.table::transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    OUT <- data.frame(Prediction=round(predict(model,test), 3))
    print(OUT)
  })
           
  output$DISTANCE <- renderLeaflet({
    pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$DISTANCE)
    
    labels = sprintf(
      "<strong>%s</strong><br/>%g Trip distance (km)",
      week_zcta()$zones, week_zcta()$DISTANCE) %>% 
      lapply(htmltools::HTML)
    
    week_zcta() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-74.06, 4.62,zoom = 11) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(week_zcta()$DISTANCE),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright", 
                pal = pal, 
                values = ~ DISTANCE,
                title = "Trip distance (km)",
                opacity = 0.7)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
