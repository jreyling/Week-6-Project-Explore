#set up for the shiny app
library(tmap)
library(sf)
library(dplyr)
library(shiny)

# read in data
load("data/shinyDemoData.RData")

# set tmap mode to interactive
tmap_mode("view")

ui <- fluidPage(
  #App title
  titlePanel("Species of Colorado"),
  
  # Add some informational text using and HTML tag (i.e., a level 5 heading)
  h5(
    "In this app you can filter occurrences by species, type of observation, and elevation. You can also click on individual occurrences to view metadata."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: select species shown on map
      checkboxGroupInput(
        inputId = "species",
        label = "Species",
        # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
        choices = list("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander"),
        # selected = sets which are selected by default
        selected = c("Elk", "Yellow-bellied Marmot", "Western Tiger Salamander")
      ),
      
      # Input: Filter points by observation type
      checkboxGroupInput(
        inputId = "obs",
        label = "Observation Type",
        choiceNames = list(
          "Human Observation",
          "Preserved Specimen",
          "Machine Observation"
        ),
        choiceValues = list(
          "HUMAN_OBSERVATION",
          "PRESERVED_SPECIMEN",
          "MACHINE_OBSERVATION"
        ),
        selected = c("HUMAN_OBSERVATION",
                     "PRESERVED_SPECIMEN",
                     "MACHINE_OBSERVATION"
        )
      ),
      
      
      # Input: Filter by elevation
      sliderInput(
        inputId = "elevation",
        label = "Elevation",
        min = 1000,
        max = 4500,
        value = c(1000, 4500)
      )
      
    ),
    
    # Main panel for displaying output (our map)
    mainPanel(# Output: interactive tmap object
      tmapOutput("map"))
    
  )
  
)

server <- function(input, output){
  
  # Make a reactive object for the occ data by calling inputIDs to extract the values the user chose
  occ_react <- reactive(
    occ %>%
      filter(Species %in% input$species) %>%
      filter(basisOfRecord %in% input$obs) %>%
      filter(elevation >= input$elevation[1] &
               elevation <= input$elevation[2])
  )
  
  # Render the map based on our reactive occurrence dataset
  output$map <- renderTmap({
    tm_shape(occ_react()) +
      tm_dots(
        col = "Species",
        size = 0.1,
        palette = "Dark2",
        title = "Species Occurrences",
        popup.vars = c(
          "Species" = "Species",
          "Record Type" = "basisOfRecord",
          "Elevation (m)" = "elevation"
        )
      ) +
      tm_shape(ROMO) +
      tm_polygons(alpha = 0.7, title = "Rocky Mountain National Park")
    
    
  })
}

shinyApp(ui, server)
