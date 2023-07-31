# Load required libraries
library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(tidyr)

# Read the data
claims <- read.csv("data/processed/claims.csv")
age <- read.csv("data/processed/age.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Health Dashboard"),
  dashboardSidebar(tags$style(".left-side, .main-sidebar { width: 235px }"),
                   sidebarMenu(
                     id = "sidebar",
                     # Add dropdown menu input for selecting year
                     selectInput(
                       "year",
                       "Select Year:",
                       choices = unique(claims$year)
                     ),
                     # Add checkbox input for selecting plan
                     checkboxGroupInput(
                       "plan", 
                       "Plan",
                       choices = c("Plan I", "Plan B", "Plan C", "Plan D", "Plan F", "Plan G", "Plan P"),
                       selected = c("Plan I")
                     ),
                     radioButtons(
                       "service",
                       "Select Service:",
                       choices = c("Medical", "Other Health Practioners"),
                       selected = "Medical"
                     )
                   )
  ),
  dashboardBody(
    # Added a row for summary statistics
    fluidPage(
    fluidRow(
      tags$head(
        tags$link(
          rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Avant+Garde&display=swap")
      ),
      column(width = 12,
             tags$h2(class = "text-center", style = "font-size: 20px; font-weight: bold; font-family: 'Avant Garde', sans-serif;",
                     textOutput("summaryTitle")
             )
      )
    ),
    fluidRow(
      valueBoxOutput("avgBox"),
      valueBoxOutput("beneficiaryBox"),
      valueBoxOutput("claimBox"),
      valueBoxOutput("paidBox")
    ),
    fluidRow(
      plotlyOutput("barChart")
    ),
    
    fluidRow(
      # Empty row to create more space
      column(width = 2, tags$hr()), # Adds a horizontal rule to separate rows
    ),
    
    fluidRow(
      column(width = 6, plotlyOutput("pieCount")),
      column(width = 6, plotlyOutput("pieExpend"))
    )
  )
)
)

# Define server logic
server <- function(input, output, session) {
  
  output$summaryTitle <- renderText({
    # Format the input$year to display as "Year 2013-2014"
    year_range <- gsub("year_(\\d+)_(\\d+)", "Year \\1-\\2", input$year)
    paste("Summary Statistics in", year_range)
  })
  
  # Data processing for summary statistics
  stat_data <- reactive({
    summary_data <- claims %>%
      filter(year == input$year) %>%
      group_by(feature) %>%
      summarize(total_amount = sum(value), .groups = 'drop') %>%
      pivot_wider(names_from = feature, values_from = total_amount)
    
    return(summary_data)
  })
  
  # Create summary statistic box for total claims
  output$avgBox <- renderValueBox({
    valueBox(
      paste0(stat_data()$"Avg Number Of Claims Per Beneficiary"), "Average Claims per Beneficiary", icon = icon("fa-light fa-face-smile"),
      color = "green")
  })
  
  output$beneficiaryBox <- renderValueBox({
    valueBox(
      paste0(stat_data()$"Number Of Beneficiaries Millions"), "Total Beneficiaries (millions)", icon = icon("fa-light fa-user"),
      color = "light-blue")
  })
  
  output$claimBox <- renderValueBox({
    valueBox(
      paste0(stat_data()$"Number Of Claims Millions"), "Total Claims (millions)", icon = icon("fa-light fa-plus"),
      color = "light-blue")
  })
  
  output$paidBox <- renderValueBox({
    valueBox(
      paste0(stat_data()$"Total Amount Paid Millions"), "Total Amount Paid (millions)", icon = icon("fa-light fa-dollar-sign"),
      color = "red")
  })
  
  # Create bar chart for selected plans and year
  output$barChart <- renderPlotly({
    # Filter data based on selected plans
    filtered_data <- claims %>%
      filter(year == input$year, plan %in% input$plan) %>%
      group_by(feature, plan) %>%
      summarize(amount = sum(value), .groups = 'drop')
    
    # Select only the desired features for the bar chart
    filtered_data <- filtered_data %>%
      filter(feature %in% c("Number Of Claims Millions", "Number Of Beneficiaries Millions", "Total Amount Paid Millions", "Avg Number Of Claims Per Beneficiary"))
    
    # Create the bar chart
    p <- ggplot(filtered_data, aes(x = feature, y = amount, fill = plan)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Feature", y = "Amount", title = "The Value Amount of Each Plan in This Year") +
      theme_minimal()
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
  
  
  # Create pie count
  output$pieCount <- renderPlotly({
    
    filtered_data <- age %>%
      filter(year == input$year, service == input$service, type == "Patient Count") %>%
      select(age, value)
    
    total_patients <- sum(filtered_data$value)
    
    pie_data <- filtered_data %>%
      mutate(percentage = value/total_patients * 100)
    
    plot_ly(pie_data, labels = ~age,
            values = ~percentage, type = 'pie',
            textinfo = "label+percent", textposition = "inside",
            insidetextorientation = "radial") %>%
      layout(title = "Percentage of Patients in Each Age Group in This Year",
             showlegend = TRUE)
  })
  
  # Create pie count
  output$pieExpend <- renderPlotly({
    
    filtered_data <- age %>%
      filter(year == input$year, service == input$service, type == "Expenditures") %>%
      select(age, value)
    
    total_expend <- sum(filtered_data$value)
    
    pie_data <- filtered_data %>%
      mutate(percentage = value/total_expend * 100)
    
    plot_ly(pie_data, labels = ~age,
            values = ~percentage, type = 'pie',
            textinfo = "label+percent", textposition = "inside",
            insidetextorientation = "radial") %>%
      layout(title = "Percentage of Expenditures in Each Age Group in This Year",
             showlegend = TRUE)
  })
}

# Run app
shinyApp(ui, server)
