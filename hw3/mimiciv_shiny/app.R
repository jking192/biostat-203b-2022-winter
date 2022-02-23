library(shiny)
library(tidyverse)

icu_data <- readRDS("icu_cohort.rds")


ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort Data Analysis"),
  tabsetPanel(
    tabPanel(
      "Demographics",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_demo",
            label = "Choose a demographic:",
            choices = c("Ethnicity","Language",
                        "Maritial Status", "Insurance",
                        "Gender", "Admission Age"),
            selected = "Ethnicity")),
        mainPanel(
          plotOutput("demo_plot"),
          verbatimTextOutput("demo_summary")
        )
      )
    ),
    tabPanel(
      "Lab Measurements",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_lab",
                      label = "Choose a lab measurement:",
                      choices = c("Bicarbonate", "Calcium", 
                                  "Chloride", "Creatinine", "Glucose", 
                                  "Hematocrit", "Magnesium", "Potassium",
                                  "Sodium", "White Blood Cell Count"),
                        selected = "Bicarbonate"),
          sliderInput("lab_bins", 
                      label = "Number of bins:",
                      min = 1, max = 100, value = 20)),
        mainPanel(
          plotOutput("lab_plot"),
          verbatimTextOutput("lab_summary")
        )
      )
    ),
    tabPanel(
      "Vital Measurements",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_vital",
                      label = "Choose a vital measurement:",
                      choices = c("Body Temperature in Fahrenheit",  
                                  "Heart Rate",
                                  "Mean non-invasive Blood Pressure",
                                  "Respiratory Rate",
                                  "Systolic non-invasive Blood Presure"),
                      selected = "Body Temperature in Fahrenheit"),
          sliderInput("vital_bins", 
                      label = "Number of bins:",
                      min = 1, max = 100, value = 20)),
        mainPanel(
          plotOutput("vital_plot"),
          verbatimTextOutput("vital_summary")
        )
      )
    )
  )
)
# Server
server <- function(input, output) { 
  dem <- reactive({
     choice_demo <- switch(
      input$choice_demo,
      "Ethnicity" = icu_data$ethnicity,
      "Language" = icu_data$language,
      "Maritial Status" = icu_data$marital_status,
      "Insurance" = icu_data$insurance,
      "Gender" = icu_data$gender,
      "Admission Age" = icu_data$admit_age
    )
  })
  output$demo_plot <- renderPlot({
    if (input$choice_demo != "Admission Age") {
      ggplot(data = icu_data) + 
        geom_bar(mapping = aes(x = dem()), fill = "skyblue") +
        coord_flip() +
        labs(x = input$choice_demo) 
    }  
    else {
      ggplot(data = icu_data) +
        geom_boxplot(mapping = aes(y = dem()), fill = "skyblue") +
        labs(y = input$choice_demo) +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
  })
  output$demo_summary <-renderPrint({
    if (input$choice_demo != "Admission Age") {
      table(dem())
    }
    else {
      summary(dem())
    }
  })
  lab <- reactive({
    choice_lab <- switch(
      input$choice_lab,
      "Bicarbonate" = icu_data$Bicarbonate,
      "Calcium" = icu_data$Calcium,
      "Chloride" = icu_data$Chloride,
      "Creatinine" = icu_data$Creatinine,
      "Glucose" = icu_data$Glucose,
      "Hematocrit" = icu_data$Hematocrit,
      "Magnesium" = icu_data$Magnesium,
      "Potassium" = icu_data$Potassium,
      "Sodium" = icu_data$Sodium,
      "White Blood Cell Count" = icu_data$WBC
    )
  })
  output$lab_plot <- renderPlot({
    ggplot(data = icu_data) +
      geom_histogram(
        mapping = aes(x = lab()), 
        fill = "turquoise", 
        bins = input$lab_bins) +
      labs(x = "Measurement Value") 
  })
  output$lab_summary <-renderPrint({
    summary(lab())
  })
  vital <- reactive({
    choice_vital <- switch(
      input$choice_vital,
      "Body Temperature in Fahrenheit" = icu_data$Temperature_Fahrenheit,
      "Heart Rate" = icu_data$Heart_Rate,
      "Mean non-invasive Blood Pressure" = 
      icu_data$Non_Invasive_Blood_Pressure_mean,
      "Respiratory Rate" = icu_data$Respiratory_Rate,
      "Systolic non-invasive Blood Presure" = 
      icu_data$Non_Invasive_Blood_Pressure_systolic,
    )
  })
  output$vital_plot <- renderPlot({
    ggplot(data = icu_data) +
      geom_histogram(mapping = aes(
        x = vital()), 
        fill = "red3", 
        bins = input$vital_bins) +
      scale_x_continuous(trans = "sqrt") +
      labs(x = "Measurement Value") 
  })
  output$vital_summary <-renderPrint({
    summary(vital())
  })
      
}
shinyApp(ui, server)