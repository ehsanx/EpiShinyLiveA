library(shiny)
library(nhanesA)
library(DataExplorer)
library(Hmisc)
library(dplyr)
library(tableone)

# UI with sidebar layout
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML("<p>We will work with the Demographic data component from the National Health and Nutrition Examination Survey (NHANES). We will download them directly from the US CDC website: <a href='https://wwwn.cdc.gov/Nchs/Nhanes/'>https://wwwn.cdc.gov/Nchs/Nhanes/</a>.</p>"),
      selectInput("dataset", "Select NHANES Cycle:",
                  choices = c("1999-2000" = "DEMO",
                              "2001-2002" = "DEMO_B",
                              "2003-2004" = "DEMO_C",
                              "2005-2006" = "DEMO_D",
                              "2007-2008" = "DEMO_E",
                              "2009-2010" = "DEMO_F",
                              "2011-2012" = "DEMO_G",
                              "2013-2014" = "DEMO_H",
                              "2015-2016" = "DEMO_I",
                              "2017-2018" = "DEMO_J"),
                  selected = "DEMO_H"),
      actionButton("loadData", "Download Selected Dataset"),
      uiOutput("variableCheckboxes")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Subset Data Info",
                 textOutput("subsetStatus")
        ),
        tabPanel("Visually Exploring",
                 tags$p("DataExplorer package is used."),
                 selectInput("plotType", "Select Plot Type:",
                             choices = c("Introduction" = "intro",
                                         "Missing Data" = "missing",
                                         "Histogram" = "histogram",
                                         "Density Plot" = "density",
                                         "Bar Plot" = "bar")),
                 uiOutput("selectedPlot")
        ),
        tabPanel("Numerically Exploring",
                 tags$p("Hmisc package is used."),
                 verbatimTextOutput("hmiscOutput")
        ),
        tabPanel("Table 1",
                 tags$p("Create Table 1 using the tableone package."),
                 selectInput("strataVar", "Select Strata Variable:", choices = c("None" = "None"), selected = "None"),
                 actionButton("createTable", "Create Table 1"),
                 verbatimTextOutput("tableOneText")  # Add this line to display the printed TableOne object
        )
        
      )
    )
  )
)




# Server
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, subset = NULL, translatedSubset = NULL, downloading = FALSE)
  
  observeEvent(input$loadData, {
    withProgress(message = 'Downloading selected dataset. Please wait...', {
      rv$downloading <- TRUE
      incProgress(0.1)
      Sys.sleep(2)  # Simulate a download delay
      rv$data <- nhanes(input$dataset)
      rv$downloading <- FALSE
      incProgress(0.9)
      
      # Pre-select certain variables if they exist in the data
      target_names <- c("RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDBORN4", "DMDMARTL", "RIDEXPRG", "DMDHHSIZ", "INDHHIN2", "INDFMPIR")
      available_names <- target_names[target_names %in% names(rv$data)]
      
      output$variableCheckboxes <- renderUI({
        if (!is.null(rv$data)) {
          checkboxGroupInput("selectedVars", "Choose Variables:",
                             choices = names(rv$data),
                             selected = available_names)
        }
      })
    })
  })
  
  # Reactive expression for the subset based on selected variables
  reactiveSubset <- reactive({
    req(rv$data)
    req(input$selectedVars)
    subset <- rv$data %>% select(all_of(input$selectedVars))
    return(subset)
  })
  
  # Automatically translate the subset when the selected variables change
  # Automatically translate the subset when the selected variables change
  translatedSubset <- reactive({
    req(reactiveSubset())
    
    # Show progress bar during translation
    withProgress(message = 'Translating (recoding) the selected variables. Please wait...', {
      incProgress(0.3)
      Sys.sleep(1)  # Simulate translation delay
      
      translated <- nhanesTranslate(input$dataset, names(reactiveSubset()), data = reactiveSubset())
      
      incProgress(0.7)
      return(translated)
    })
  })
  
  output$subsetStatus <- renderText({
    paste("Subset created with", length(input$selectedVars), "variables. Selecting more variables will result in more translation/recoding time.")
  })
  
  output$selectedPlot <- renderUI({
    req(translatedSubset())
    plotType <- input$plotType
    if (plotType == "intro") plotOutput("introPlot")
    else if (plotType == "missing") plotOutput("missingPlot")
    else if (plotType == "histogram") plotOutput("histogramPlot")
    else if (plotType == "density") plotOutput("densityPlot")
    else if (plotType == "bar") plotOutput("barPlot")
  })
  
  # DataExplorer plots using the translated subset
  output$introPlot <- renderPlot({ plot_intro(translatedSubset()) })
  output$missingPlot <- renderPlot({ plot_missing(translatedSubset()) })
  output$histogramPlot <- renderPlot({ plot_histogram(translatedSubset()) })
  output$densityPlot <- renderPlot({ plot_density(translatedSubset()) })
  output$barPlot <- renderPlot({ plot_bar(translatedSubset(), maxcat = 20) })
  
  # Hmisc summary using the translated subset
  output$hmiscOutput <- renderPrint({ describe(translatedSubset()) })
  
  observe({
    req(input$selectedVars)  # Ensure that the selected variables are available
    req(translatedSubset())
    
    # Identify categorical variables by checking if they are factors or if they have a limited number of unique values
    categoricalVars <- sapply(translatedSubset()[, input$selectedVars, drop = FALSE], function(x) {
      is.factor(x) || length(unique(x)) <= 10  # Adjust the threshold as needed
    })
    
    # Filter the selected variables to include only those identified as categorical
    categoricalVarNames <- names(categoricalVars)[categoricalVars]
    
    # Update the choices for the strataVar selectInput to include "None" and the filtered categorical variables
    updatedChoices <- c("None", categoricalVarNames)
    updateSelectInput(session, "strataVar", choices = updatedChoices, selected = "None")
  })
  
  
  # Create Table 1 when the "createTable" button is clicked
  observeEvent(input$createTable, {
    req(translatedSubset())
    strata <- if(input$strataVar != "None" && input$strataVar %in% names(translatedSubset())) {
      input$strataVar
    } else {
      NULL  # Explicitly set strata to NULL if "None" is selected or the variable isn't in the data frame
    }
    
    vars <- setdiff(names(translatedSubset()), strata)  # Exclude strata variable from vars
    
    # Call CreateTableOne with or without stratification based on strata variable selection
    tab1 <- if(is.null(strata)) {
      CreateTableOne(data = translatedSubset(), vars = vars, includeNA = TRUE, test = FALSE, smd = FALSE)
    } else {
      CreateTableOne(data = translatedSubset(), vars = vars, strata = strata, includeNA = TRUE, test = FALSE, smd = FALSE)
    }
    
    # Use renderPrint to display the output of print(tab1, showAllLevels = TRUE)
    output$tableOneText <- renderPrint({
      print(tab1, showAllLevels = TRUE)
    })
  })
  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
