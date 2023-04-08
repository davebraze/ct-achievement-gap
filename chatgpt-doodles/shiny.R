## Write an R shiny app with a barbell plot that displays 2019 NAEP reading score for all 50 states from a .csv file on my computer called 'state-reading-scores.csv'. The upper point in each barbell should be average reading score for white students. The lower point in each barbell should be average reading score for black students. Show your answer as a code snippet.

# Load required libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  # Define title
  titlePanel("2019 NAEP Reading Scores for All 50 States"),

  # Define sidebar layout
  sidebarLayout(
    # Define sidebar panel
    sidebarPanel(
      # Define file input for CSV file
      fileInput("file", "Choose CSV File", accept = ".csv")
    ),
    # Define main panel
    mainPanel(
      # Define plot output
      plotOutput("barbell")
    )
  )
)

# Define server
server <- function(input, output) {
  # Read in CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Define plot
  output$barbell <- renderPlot({
    ggplot(data(), aes(x = reorder(state, -white), y = white, ymin = black, ymax = white)) +
      geom_point(size = 3, color = "black") +
      geom_linerange(size = 1, aes(ymin = black, ymax = white)) +
      geom_segment(aes(xend = x, yend = black), size = 1) +
      coord_flip() +
      labs(x = "State", y = "Reading Score", title = "2019 NAEP Reading Scores for All 50 States") +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
  })
}

# Run the app
shinyApp(ui, server)


## My input data includes a 'year' variable with a range of values. How would I let the user select one value from among those available?

library(shiny)

ui <- fluidPage(
  sidebarPanel(
    selectInput("selected_year", "Select a year:",
                choices = unique(input_data$year))
  ),
  mainPanel(
    # add your main panel UI elements here
  )
)

server <- function(input, output) {
  # add your server logic here
}

shinyApp(ui, server)


## How would I make use of the selected year in the shiny 'server' function in order to restrict plotted data to just that year?

server <- function(input, output) {
  # filter data by selected year
  selected_data <- input_data[input_data$year == input$selected_year, ]

  # generate plot using selected data
  output$my_plot <- renderPlot({
    # create your plot using the filtered data
  })
}
