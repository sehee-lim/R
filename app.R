library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)


# load data
players <- read.csv("data/nba2018.csv")


ui <- fluidPage(

  # application title
  titlePanel("NBA 2018/19 Player Stats"),

  sidebarLayout(

    # sidebar
    sidebarPanel(
      "Exploring all player stats from the NBA 2018/19 season",
      h3("Filters"),

      sliderInput(
        inputId = "VORP",
        label = "Player VORP rating at least",
        min = -3, max = 10,
        value = c(0, 10)
      ),

      selectInput(
        inputId = "Team",
        label = "Team",
        choices = unique(players$Team),
        selected = "Golden State Warriors",
        multiple = TRUE
      ),

      h3("Plot options"),
      selectInput(
        inputId = "variable",
        label = "Variable",
        choices = c("VORP", "Salary", "Age", "Height", "Weight"),
        selected = "Salary"),

      radioButtons(
        inputId = "plot_type",
        label = "Plot type",
        choices = c("histogram", "density")),

      checkboxInput(
        inputId = "log",
        label = "Log scale",
        value = TRUE),

      numericInput(
        inputId = "size",
        label = "Font Size",
        value = 10
      ),

      colourInput(
        inputId = "col",
        label = "Linecolour",
        value = "orange"
      )
    ),

    # main area
    mainPanel(
      strong(
        "There are",
        textOutput("num_players", inline = TRUE),
        "players in the dataset"
      ),

      plotOutput("nba_plot"),
      DTOutput("players_data")
    )
  )
)


server <- function(input, output, session) {

  # reactive variable
  filtered_data <- reactive({
    players %>%
      filter(VORP >= input$VORP[1],
             VORP <= input$VORP[2],
             Team %in% input$Team)
  })

  output$players_data <- renderDT({
    filtered_data()
  })

  output$num_players <- renderText({
    nrow(filtered_data())
  })

  output$nba_plot <- renderPlot({
    p <- ggplot(data = filtered_data(), aes_string(x = input$variable)) +
      theme_classic(base_size = input$size)

    if (input$plot_type == "histogram") {
      p <- p + geom_histogram(fill = input$col)
    } else if (input$plot_type == "density") {
      p <- p + geom_density(col = input$col, size = 2)
    }

    if (input$log == TRUE) {
      p <- p + scale_x_log10(labels = scales::comma)
    } else if (input$log == FALSE) {
      p <- p + scale_x_continuous(labels = scales::comma)
    }

    p
  })
}

shinyApp(ui, server)
