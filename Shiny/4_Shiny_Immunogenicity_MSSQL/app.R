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

ui <- function(request){
  dashboardPage(
    #skin = "yellow",
    dashboardHeader(title = "Study Results",
                    dropdownMenuOutput("studies")
    ),
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
                  value = 0),
      bookmarkButton()
    ),
    dashboardBody(
      fluidRow(
        valueBoxOutput("no_samples"),
        valueBoxOutput("true_positives"),
        valueBoxOutput("avg_weight")
      ),
      fluidRow(
        box(
          plotOutput("hist_drug", 
                     height = 250,
                     click = "plot_click",
                     dblclick = "plot_dblclick",
                     hover = "plot_hover",
                     brush = "plot_brush"),
          title = "Signal Response Drug", 
          width = 6, height = 320,
          background = "light-blue"
        ),
        box(
          plotOutput("hist_no_drug", height = 250),
          title = "Signal Response No Drug", 
          width = 6, height = 320,
          collapsible = TRUE,
          background = "green"
        )
      ),
      fluidRow(
        box(
          plotOutput("hist_percent", height = 250),
          title = "Percent Signal Inhibition Drug", 
          width = 6, height = 320,
          background = "light-blue"
        ),
        box(
          plotOutput("hist_weight", height = 250),
          title = "Weight", 
          width = 6, height = 320,
          background = "green"
        )
      )
    )
  )
}

server <- function(input, output, session) {
  
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
      labs(title = "", x = "", y = "")
  })
  
  output$hist_no_drug <- renderPlot({
    sample_data() %>%
      dbplot_histogram(Signal_Response_No_Drug) +
      labs(title = "", x = "", y = "")
  })
  
# ----------------------------
  
  output$hist_percent <- renderPlot({
    sample_data() %>%
      dbplot_histogram(Percent_Signal_Inhibition_Drug) +
      labs(title = "", x = "", y = "")
  })
  
  output$hist_weight <- renderPlot({
    sample_data() %>%
      dbplot_histogram(Weight) +
      labs(title = "", x = "", y = "")
  })
#---------------------------
  
  
  output$no_samples <- renderValueBox({
   valueBox(
     sample_data() %>% 
       tally() %>% 
       pull(),
     "No. of samples",
     icon = icon("flask")
   )})
  
  output$true_positives <- renderValueBox({
    valueBox(
      sample_data() %>% 
        filter(True_Positive == "Yes") %>%
        tally() %>% 
        pull(),
      "True Positives",
      icon = icon("check"),
      color = "green"
    )})
  
  output$avg_weight <- renderValueBox({
    valueBox(
      sample_data() %>% 
        summarise(w = mean(Weight, na.rm = TRUE)) %>%
        pull() %>%
        round(., 0),
      "Avg. Weight",
      icon = icon("user"),
      color = "yellow"
    )})
  
  output$studies <- renderMenu({
    msgs <- list(
      notificationItem(text = "study_01", icon = icon("address-card"), href = ".?_inputs_&min_weight=152"),
      notificationItem(text = "study_02", icon = icon("address-card"), href = ".?_inputs_&min_weight=0")
      )
    
    dropdownMenu(
      type = "notifications", 
      headerText = "Completed Studies",
      icon = icon("clipboard"),
      #badgeStatus = NULL,
      .list = msgs)
  })
}
shinyApp(ui, server, enableBookmarking = "url")