library(shiny)
library(tidyverse)

icu_data <- as.data.frame(readRDS("icu_cohort.rds"))


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
                            "Gender"),
                selected = "Ethnicity")),
            mainPanel(
              plotOutput("demo_plot"),
              verbatimTextOutput("demo_summary")
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
      "Gender" = icu_data$gender
    )
  })
  output$demo_plot <- renderPlot({
    ggplot(data = icu_data) + 
      geom_bar(mapping = aes(x = dem()), fill = "skyblue") +
      coord_flip() +
      labs(x = input$choice_demo) 
  })
   output$demo_summary <-renderPrint({
   table(dem())
   })
      
}
shinyApp(ui, server)