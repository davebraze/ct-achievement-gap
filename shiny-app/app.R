library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(MetBrewer)
library(plotly)

source("./R/ui.R")

source("./R/server.R")

# Run the app
shinyApp(ui, server)

