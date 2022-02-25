library(shiny)
library(tidyverse)

# Load the data
icu_data <- readRDS("icu_cohort.rds")


ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort Data Explorer"),
  tabsetPanel(
    # Make demographics panel
    tabPanel(
      "Demographics",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_demo",
            label = "Choose a demographic to analyze:",
            choices = c("30 Day Mortality", 
                        "Admission Age", "Ethnicity",
                        "Gender", "Insurance",
                        "Language", "Maritial Status"),
            selected = "Ethnicity")),
        mainPanel(
          plotOutput("demo_plot"),
          h3("Summary Table", align = "center"),
          verbatimTextOutput("demo_summary")
        )
      )
    ),
    # Make lab measurements panel
    tabPanel(
      "Lab Measurements",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_lab",
                      label = "Choose a lab measurement to analyze:",
                      choices = c("Bicarbonate", "Calcium", 
                                  "Chloride", "Creatinine", "Glucose", 
                                  "Hematocrit", "Magnesium", "Potassium",
                                  "Sodium", "White Blood Cell Count"),
                        selected = "Bicarbonate"),
          h4("Histogram Options", align = "center"),
          sliderInput("lab_bins", 
                      label = "Number of bins:",
                      min = 1, max = 50, value = 20),
          checkboxInput("lab_transform",
                        label = "Log transformation",
                        value = FALSE)),
        mainPanel(
          plotOutput("lab_plot"),
          h3("Summary Statistics", align = "center"),
          verbatimTextOutput("lab_summary")
        )
      )
    ),
    # Make vitals panel
    tabPanel(
      "Vital Measurements",
      sidebarLayout(
        sidebarPanel(
          selectInput("choice_vital",
                      label = "Choose a vital measurement to analyze:",
                      choices = c("Body Temperature in Fahrenheit",  
                                  "Heart Rate",
                                  "Mean non-invasive Blood Pressure",
                                  "Respiratory Rate",
                                  "Systolic non-invasive Blood Presure"),
                      selected = "Body Temperature in Fahrenheit"),
          h4("Histogram Options", align = "center"),
          sliderInput("vital_bins", 
                      label = "Number of bins:",
                      min = 1, max = 50, value = 20),
          checkboxInput("vital_transform",
                        label = "Log transformation",
                        value = FALSE)),
        mainPanel(
          plotOutput("vital_plot"),
          h3("Summary Statistics", align = "center"),
          verbatimTextOutput("vital_summary")
        )
      )
    )
  )
)
# Server
server <- function(input, output) { 
  # Read user's choice and match with correct variable
  dem <- reactive({
     choice_demo <- switch(
      input$choice_demo,
      "Ethnicity" = icu_data$ethnicity,
      "Language" = icu_data$language,
      "Maritial Status" = icu_data$marital_status,
      "Insurance" = icu_data$insurance,
      "Gender" = icu_data$gender,
      "Admission Age" = icu_data$admit_age,
      "30 Day Mortality" = icu_data$thirty_day_mort
    )
  })
  # Make bar chart for all demographics beside age
  output$demo_plot <- renderPlot({
    if (input$choice_demo != "Admission Age") {
      ggplot(data = icu_data) + 
        geom_bar(mapping = aes(x = dem()), fill = "skyblue") +
        coord_flip() +
        labs(x = input$choice_demo) 
    } else {
    # Make box plot for age
      ggplot(data = icu_data) +
        geom_boxplot(mapping = aes(y = dem()), fill = "skyblue") +
        labs(y = input$choice_demo) +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
  })
  # Generate summary table
  output$demo_summary <-renderPrint({
    if (input$choice_demo != "Admission Age") {
      table(dem())
    } else {
      summary(dem())
    }
  })
  # Read user's choice and match with correct variable
  lab <- reactive({
    choice_lab <- switch(
      input$choice_lab,
      "Bicarbonate" = icu_data$bicarbonate,
      "Calcium" = icu_data$calcium,
      "Chloride" = icu_data$chloride,
      "Creatinine" = icu_data$creatinine,
      "Glucose" = icu_data$glucose,
      "Hematocrit" = icu_data$hematocrit,
      "Magnesium" = icu_data$magnesium,
      "Potassium" = icu_data$potassium,
      "Sodium" = icu_data$sodium,
      "White Blood Cell Count" = icu_data$wbc
    )
  })
  # Make histogram and determine whether to log transform
  output$lab_plot <- renderPlot({
    if (input$lab_transform) {
      ggplot(data = icu_data) +
        geom_histogram(
          mapping = aes(x = log(lab())), 
          fill = "turquoise", 
          bins = input$lab_bins
        ) +
        labs(x = "ln(Measurement Value)")   
    } else {
      ggplot(data = icu_data) +
        geom_histogram(
          mapping = aes(x = lab()), 
          fill = "turquoise", 
          bins = input$lab_bins
        ) +
        labs(x = "Measurement Value") 
    }  
  })
  # Generate summary stats
  output$lab_summary <-renderPrint({
    summary(lab())
  })
  # Read user's choice and match with correct variable
  vital <- reactive({
    choice_vital <- switch(
      input$choice_vital,
      "Body Temperature in Fahrenheit" = icu_data$temperature_fahrenheit,
      "Heart Rate" = icu_data$heart_rate,
      "Mean non-invasive Blood Pressure" = 
      icu_data$non_invasive_blood_pressure_mean,
      "Respiratory Rate" = icu_data$respiratory_rate,
      "Systolic non-invasive Blood Presure" = 
      icu_data$non_invasive_blood_pressure_systolic,
    )
  })
  # Make histogram and determine whether to log transform
  output$vital_plot <- renderPlot({
    if(input$vital_transform)
    {
      ggplot(data = icu_data) +
        geom_histogram(
          mapping = aes(x = log(vital())), 
          fill = "red3", 
          bins = input$vital_bins
        ) +
        labs(x = "ln(Measurement Value)") 
    } else
    {
      ggplot(data = icu_data) +
        geom_histogram(
          mapping = aes(x = vital()), 
          fill = "red3", 
          bins = input$vital_bins
        ) +
        labs(x = "Measurement Value") 
    }
  })
  # Generate summary stats
  output$vital_summary <-renderPrint({
    summary(vital())
  })
      
}
# Run the app
shinyApp(ui, server)