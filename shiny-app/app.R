library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(MetBrewer)
library(plotly)

## Read data file
data <-
    read.csv(here::here("data", "D2.csv")) %>%
    filter(year >= 2003)

ylimits <- range(data$avg.scale.score, na.rm = TRUE)

source("ui.R")

source("server.R")

# Run the app
shinyApp(ui, server)

