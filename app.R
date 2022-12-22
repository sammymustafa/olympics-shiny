# Load Packages
library(tidyverse)
library(janitor)
library(shiny)
library(janitor) 
library(ggthemes) 
library(patchwork) 
library(sf)
library(scales)
library(dplyr)

# Load Data
olympics <- read_csv("data/olympics.csv") %>% 
  na.omit() %>%
  clean_names() %>%
  mutate(year = as.character(year))
olympics$medal <- factor(olympics$medal, levels = c("Gold", "Silver", "Bronze"))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Distribution of Summer Olympics Medals (1976-2008)"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Introduction 
      p("The Summer Olympic Games are an international, multi-sport held every four years. However, each Olympic Games has different end-statistics and trends that are interesting to study!"),
      p("Thus, please select which year's distribution of medals you would like to analyze by either country, sport, or gender."),
      br(),
      # Year
      selectInput("year", 
                  label = "Year:",
                  choices = c("1976", "1980", "1984",
                              "1988", "1992", "1996",
                              "2000", "2004", "2008"),
                  selected = "2000"),
      # Fill
      radioButtons("x", 
                   label = "X-Variable of Interest:",
                   choices = c("Top 5 Countries",
                               "Top 5 Sports",
                               "Gender"),
                   selected = "Top 5 Sports"),
      # Background Information about Visualization
      br(),
      strong("Citation:"),
      p("Agrawal, D. (2020, February 3). Summer Olympics medals (1976-2008).
        Kaggle. Retrieved from
        https://www.kaggle.com/datasets/divyansh22/summer-olympics-medals"),
      img(src = "olympics_logo.png", height = 100, width = 225)
    ),
    # Output
    mainPanel(plotOutput("graphic"))
  )
)

# Define server logic 
server <- function(input, output) {
  # Data Filtering 
  yearInput <- reactive({
    if (input$year == "1976") return(olympics %>% filter(year == "1976"))
    if (input$year == "1980") return(olympics %>% filter(year == "1980"))
    if (input$year == "1984") return(olympics %>% filter(year == "1984"))
    if (input$year == "1988") return(olympics %>% filter(year == "1988"))
    if (input$year == "1992") return(olympics %>% filter(year == "1992"))
    if (input$year == "1996") return(olympics %>% filter(year == "1996"))
    if (input$year == "2000") return(olympics %>% filter(year == "2000"))
    if (input$year == "2004") return(olympics %>% filter(year == "2004"))
    if (input$year == "2008") return(olympics %>% filter(year == "2008"))
  })
  countData <- reactive({
    if (input$x == "Top 5 Countries") return(yearInput() %>% count(medal, country, sort = TRUE))
    if (input$x == "Top 5 Sports") return(yearInput() %>% count(medal, sport, sort = TRUE))
    if (input$x == "Gender") return(yearInput() %>% count(medal, gender, sort = TRUE))
  })
  finalData <- reactive({
    if (input$x == "Top 5 Countries") return(countData() %>% filter(country %in% (head(unique(countData()$country), 5))))
    if (input$x == "Top 5 Sports") return(countData() %>% filter(sport %in% (head(unique(countData()$sport), 5))))
    if (input$x == "Gender") return(countData())
  })
  output$graphic <- renderPlot({
    # X-Axis Variable
    x <- switch(input$x,
                "Top 5 Countries" = finalData()$country,
                "Top 5 Sports" = finalData()$sport,
                "Gender" = finalData()$gender
    )
    # Title
    title <- switch(input$x,
                    "Top 5 Countries" = "Distribution of Medals by Country",
                    "Top 5 Sports" = "Distribution of Medals by Sport",
                    "Gender" = "Distribution of Medals by Gender"
    )
    # Caption
    caption <- switch(input$year,
                      "1976" = "Montreal 1976 Summer Olympics",
                      "1980" = "Moscow 1980 Summer Olympics",
                      "1984" = "Los Angeles 1984 Summer Olympics",
                      "1988" = "Seoul 1988 Summer Olympics",
                      "1992" = "Barcelona 1992 Summer Olympics",
                      "1996" = "Atlanta 1996 Summer Olympics",
                      "2000" = "Sydney 2000 Summer Olympics",
                      "2004" = "Athens 2004 Summer Olympics",
                      "2008" = "Beijing 2008 Summer Olympics"
    )
    # Plot
    ggplot(finalData(), aes(x = x, y = n, fill = medal)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = title,
           x = input$x,
           y = "Count",
           fill = "Medal Type",
           caption = caption) + 
      scale_fill_manual(values = c("#DAA520", "#A9A9A9", "#B9722D")) +
      theme_classic() + 
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.caption = element_text(size = 11, vjust = -1.5),
            axis.title = element_text(size = 14),
            axis.title.x = element_text(vjust = -2),
            axis.title.y = element_text(vjust = 1.5),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)