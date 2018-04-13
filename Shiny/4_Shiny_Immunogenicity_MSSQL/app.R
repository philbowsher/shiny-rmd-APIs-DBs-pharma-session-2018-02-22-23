library(shinydashboard)
library(shiny)
library(dplyr)
library(dbplyr)
library(odbc)
library(DBI)
library(dbplot)
library(ggplot2)

load("samples.Rdata")

blood <- samples %>%
  group_by(Blood_Type) %>%
  summarise() %>%
  pull()
# Getting all ranges in one SQL "trip"
ranges <- samples %>%
  summarise(
    max_weight = max(Weight, na.rm = TRUE),
    min_weight = min(Weight, na.rm = TRUE),
    max_sreening = max(Signal_Response_Drug, na.rm = TRUE),
    max_confirmatory = max(Signal_Response_No_Drug, na.rm = TRUE)
  ) %>%
  collect()

ui <- dashboardPage(
  dashboardHeader(title = "Quick Example"),
  dashboardSidebar(
    selectInput("blood", "Blood Type:",
                blood,
                multiple = TRUE,
                size = 5,
                selected = blood,
                selectize = FALSE),
    sliderInput("min_weight", 
                "Minimum Weight:",
                min = ranges$min_weight, 
                max = ranges$max_weight, 
                value = ranges$min_weight),
    sliderInput("max_weight", 
                "Maximum Weight:",
                min = ranges$min_weight, 
                max = ranges$max_weight, 
                value = ranges$max_weight),
    sliderInput("screening_cutoff", 
                "Screening Cutoff:",
                min = 0, 
                max = ranges$max_sreening, 
                value = 0),
    sliderInput("confirmatory_cutoff", 
                "Confirmatory Cutoff:",
                min = 0, 
                max = ranges$max_sreening, 
                value = 0)
    ),
  dashboardBody(
    valueBoxOutput("no_samples"),
    valueBoxOutput("true_positives"),
    plotOutput("hist_drug"),
    plotOutput("hist_no_drug")
  )
)
server <- function(input, output) {
  
  sample_data <- reactive({
    samples %>%
      filter(
        Blood_Type %in% input$blood,
        Weight >= input$min_weight,
        Weight <= input$max_weight) %>%
      mutate(
        Response_Drug = ifelse(Signal_Response_Drug >= input$screening_cutoff, "Positive", "Negative"),
        Response_No_Drug = ifelse(Signal_Response_No_Drug >= input$confirmatory_cutoff, "Positive", "Negative")
      ) %>%
      mutate(
        True_Positive = ifelse(Response_Drug == Response_No_Drug, "Yes", "No")
      )
      
       
  })
  
  output$hist_drug <- renderPlot({
    sample_data() %>%
      dbplot_histogram(Signal_Response_Drug) +
      labs(title = "Signal Response Drug", x = "", y = "")
  })
  
  output$hist_no_drug <- renderPlot({
    sample_data() %>%
      dbplot_histogram(Signal_Response_No_Drug) +
      labs(title = "Signal Response No Drug", x = "", y = "")
  })
  
  output$no_samples <- renderValueBox({
   valueBox(
     sample_data() %>% 
       tally() %>% 
       pull(),
     "No. of samples"
   )})
  
  output$true_positives <- renderValueBox({
    valueBox(
      sample_data() %>% 
        filter(True_Positive == "Yes") %>%
        tally() %>% 
        pull(),
      "True Positives"
    )})
  
  output$test <- renderText(length(input$blood))
}
shinyApp(ui, server)