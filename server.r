# Load names and descriptions of datasets ---------------------------------
datasetNames <- as_tibble(read.csv("data/datasetNames.csv")) %>% 
  mutate_each(funs(as.character)) %>% 
  filter(timeSeries == FALSE)


# Define mode function ----------------------------------------------------
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Define server logic -----------------------------------------------------
shinyServer(function(input, output, session) {
  
  # # Stop app when browser is closed -----------------------------------------
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  
  # Load data ---------------------------------------------------------------
  myData <- reactive({
    loadedData <- as_tibble(get(input$dataset))
  })
  
  # Output description of dataset -------------------------------------------
  output$description <- renderText(
    datasetNames$description[match(input$dataset, datasetNames$name)]
  )
  
  # Select grouping variables -----------------------------------------------
  output$grouping <- renderUI({
    loadedData <- myData()
    selectizeInput(
      "grouping",
      label = "grouping variables",
      choices = names(loadedData),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  # Select variable to summarize --------------------------------------------
  output$summVar <- renderUI({
    loadedData <- myData()
    selectizeInput(
      'summVar',
      label = "variable to summarize",
      choices = names(loadedData),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  # Summarize dataset -------------------------------------------------------
  summData <- reactive({
    origData <- myData()
    
    if (is.null(input$grouping) | 
        is.null(input$summVar)) {
      summData <- origData
    } else {
      newNames <- paste(input$summVar, input$summFun, sep = "_")
      
      summData <- origData %>%
        group_by_(.dots = input$grouping) %>%
        summarize_each_(funs = funs_(dots = input$summFun, env = environment()),
                        vars = input$summVar) %>% 
        rename_(.dots = setNames(input$summVar, newNames))
      
    }
    
    return(summData)
  })
  
  # Select x-variable -------------------------------------------------------
  output$selectX <- renderUI({
    loadedData <- summData()
    selectizeInput(
      'xvar',
      label = "x-variable",
      choices = names(loadedData),
      selected = names(loadedData)[1],
      multiple = FALSE
    )
  })
  
  # Select y-variable -------------------------------------------------------
  output$selectY <- renderUI({
    loadedData <- summData()
    selectizeInput(
      'yvar',
      label = "y-variable",
      choices = c("None", names(loadedData)),
      selected = "None",
      multiple = FALSE
    )
  })
  
  # Specify which variables are continuous ----------------------------------
  output$contDiscrete <- renderUI({
    loadedData <- summData()
    
    if(input$yvar == "None"){
      boxChoices <- "xvar"
      if(is.numeric(loadedData[[input$xvar]])){
        boxSelected <-  "xvar"
      } else {
        boxSelected <- ""
      }
    } else {
      boxChoices <-  c("xvar", "yvar")
      if(is.numeric(loadedData[[input$xvar]])){
        boxSelected <- "xvar"
      } else {
        boxSelected <- ""
      }
      if(is.numeric(loadedData[[input$yvar]])){
        boxSelected <- c(boxSelected, "yvar")
      } else {
        boxSelected <- boxSelected
      }
    }
    checkboxGroupInput(
      'contDiscrete',
      label = "continuous variables",
      choices = boxChoices,
      selected = boxSelected
    )
    
  })

  # Select fill variable ----------------------------------------------------
  output$fill <- renderUI({
    loadedData <- summData()
    selectizeInput(
      'fill',
      label = "fill variable",
      choices = c("None", names(loadedData)),
      selected = "None",
      multiple = FALSE
    )
  })

  # Select facet variable ---------------------------------------------------
  output$facet <- renderUI({
    loadedData <- summData()
    selectizeInput(
      "facet",
      label = "faceting variables",
      choices = names(loadedData),
      selected = NULL,
      multiple = TRUE
    )
  })

  # Select geom for ggplot  ---------------------------------------------------
  output$geom <- renderUI({
    loadedData <- summData()

    # One variable
    if (input$yvar == "None" & ("xvar" %in% input$contDiscrete)) {
      # Continuous X
      geomChoices = c("geom_histogram",
                      "geom_area",
                      "geom_density",
                      "geom_dotplot",
                      "geom_freqpoly")

    } else if (input$yvar == "None" &
               !("xvar" %in% input$contDiscrete)) {
      # Discrete X
      geomChoices = c("geom_bar")

    } else if (input$yvar != "None") {
      if (("xvar" %in% input$contDiscrete) &
          ("yvar" %in% input$contDiscrete)) {
        # Continuous X, Continuous Y
        geomChoices = c(
          "geom_point",
          "geom_jitter",
          "geom_quantile",
          "geom_rug",
          "geom_smooth",
          "geom_bin2d",
          "geom_hex",
          "geom_area",
          "geom_line",
          "geom_step"
        )
      } else if (!("xvar" %in% input$contDiscrete) &
        ("yvar" %in% input$contDiscrete)) {
        # Discrete X, Continuous Y
        geomChoices = c(
          "geom_bar",
          "geom_boxplot",
          "geom_dotplot",
          "geom_violin"
        )
      } else if (!("xvar" %in% input$contDiscrete) &
                 !("yvar" %in% input$contDiscrete)) {
        # Discrete X, Discrete Y
        geomChoices = c(
          "geom_count"
        )
      }
    }

    # Drop down menu for geoms
    selectInput(
      'geom',
      label = "geom",
      choices = geomChoices,
      selected = geomChoices[1]
    )
  })

  # Create plot -------------------------------------------------------------
  output$countPlot <- renderPlot({

    # Load data
    loadedData <- summData()
    
    if (!("xvar" %in% input$contDiscrete)){
      loadedData[[input$xvar]] <- as.factor(loadedData[[input$xvar]])
    }
    
    if (!("yvar" %in% input$contDiscrete) & input$yvar != "None"){
      loadedData[[input$yvar]] <- as.factor(loadedData[[input$yvar]])
    }
    
    if(input$fill == "None"){
      loadedData <- loadedData %>%
        mutate(None = "")
    }

    # Get function for selected geom
    selectedGeom <- get(input$geom)

    # Some logic for selecting stat based on data types
    if (input$yvar == "None" & is.numeric(loadedData[[input$xvar]])) {
      selectedStat = "bin"
    } else if (input$yvar == "None" &
               !is.numeric(loadedData[[input$xvar]])) {
      selectedStat = "count"
    } else if (input$yvar != "None" & input$geom == "geom_bar") {
      selectedStat <- "identity"
    } else if (input$geom == "geom_boxplot") {
      selectedStat <- "boxplot"
    } else if (input$geom == "geom_violin") {
      selectedStat <- "ydensity"
    } else {
      selectedStat <- "identity"
    }

    # Check length of faceting variable
    validate(
      need(length(input$facet) < 3 | is.null(input$facet),
           "Please select 2 or fewer faceting variables.")
    )

    # Create one or two dimensional plot
    if (input$yvar == "None") {
      myPlot <- ggplot(data = loadedData, aes(
        x = eval(as.name(paste(input$xvar))),
        fill = eval(as.name(paste(input$fill))),
        color = eval(as.name(paste(input$fill)))
        )) +
          selectedGeom(aes(y = ..count..), stat = selectedStat)

      yvar <- "count"
    } else {
      myPlot <- ggplot(data = loadedData, aes(
        x = eval(as.name(paste(input$xvar))),
        y = eval(as.name(paste(input$yvar))),
        fill = as.factor(eval(as.name(paste(input$fill)))),
        color = as.factor(eval(as.name(paste(input$fill))))
        )) +
          selectedGeom(stat = selectedStat)

      yvar <- input$yvar
    }

    # Add facet wrap
    if (length(input$facet) != 0) {
      myPlot <- myPlot +
        facet_wrap(as.formula(paste("~", input$facet, collapse = " ")), scales = input$scales)
    }

    # Labels and formatting
    myPlot <- myPlot +
      labs(x = input$xvar,
           y = yvar,
           fill = input$fill,
           color = input$fill) +
      # scale_fill_brewer(palette = "Paired") +
      # scale_color_brewer(palette = "Paired") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 16),
        title = element_text(size = 20, face = "bold"),
        # aspect.ratio = 0.75
      )
    
    if(input$fill == "None"){
      myPlot <- myPlot + guides(fill=FALSE, color = FALSE)
    }

    return(myPlot)
  })

})
