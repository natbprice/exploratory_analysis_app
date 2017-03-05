
# Set java options
options(java.parameters = "- Xmx1024m")

# Load libraries
library(shiny)
library(dplyr)
library(tibble)
library(ggplot2)

# Load dataset names and descriptions
datasetNames <- as_tibble(read.csv("data/datasetNames.csv")) %>% 
  mutate_each(funs(as.character)) %>% 
  filter(timeSeries == FALSE,
         name %in% c("ChickWeight", "Orange", "Titanic", "ToothGrowth", 
                     "UCBAdmissions", "USArrests", "airquality", "iris",
                     "mtcars", "swiss", "trees"))


# Define UI for application
shinyUI(fluidPage(
  # Application title
  titlePanel("Exploratory Anaysis Demo"),
  
  # Add custom CSS and Javascript
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(type = "text/javascript", src = "busy.js")
  )),
  
  # Add busy display panel
  absolutePanel(
    fixed = TRUE,
    bottom = 0,
    right = 0,
    width = 380,
    height = 50,
    div(class = "busy",
        p(strong(
          'Calculation in progress..'
        )),
        img(src = "busy.gif", width = 25))
  ),
  
  # Sidebar with data filtering controls and action buttons
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "dataset",
        label = "Dataset",
        choices = datasetNames$name,
        selected = datasetNames$name[1]
      ),
      
      # Header for buttons
      h4("Summarize dataset"),
      
      # Select grouping variables
      uiOutput("grouping"),
      
      # Select variable to summarize
      uiOutput("summVar"),
      
      # Select summarize function
      selectizeInput(
        'summFun',
        label = "summarize function",
        choices = c("sum", "mean", "median", "Mode",
                    "min", "max", "sd"),
        selected = NULL,
        multiple = FALSE
      ),
      
      # Header for buttons
      h4("Plot controls"),
      
      # Select x-variable
      uiOutput("selectX"),
      
      # Select y-variable
      uiOutput("selectY"),
      
      uiOutput("contDiscrete"),
      
      # Select geom
      uiOutput("geom"),
      
      # Select fill variable
      uiOutput("fill"),
      
      # Select facet variables
      uiOutput("facet"),
      
      # Select scaling
      radioButtons(
        "scales",
        label = "Scales",
        choices = c("fixed", "free", "free_x", "free_y"),
        selected = c("free_y")
      )
      
    ),
    
    # Main panel with visualizations
    mainPanel(
      h3(textOutput("description")),
      plotOutput(
      "countPlot", width = "100%", height = "600px"
    )
    )
    
  )
))
