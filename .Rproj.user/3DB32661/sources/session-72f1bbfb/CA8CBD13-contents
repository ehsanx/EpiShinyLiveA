library(shiny)
library(nhanesA)
library(DataExplorer)
library(Hmisc)
library(psych)
library(DT)

# UI with sidebar layout
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Use HTML or tags$ functions for static notes
      HTML("<p>We will work with Demographic data component from the National Health and Nutrition Examination Survey (NHANES). We will download them directy from the US CDC website: https://wwwn.cdc.gov/Nchs/Nhanes/. </p>"),
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
                 # Use tags$ functions for static notes within tab panels
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
                 # Use tags$ functions for static notes within tab panels
                 tags$p("Hmisc package is used."),
                 verbatimTextOutput("hmiscOutput")
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, downloading = FALSE)
  
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
    paste("Subset created with", length(input$selectedVars), "variables.")
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
  
}

# Run the app
shinyApp(ui = ui, server = server)
