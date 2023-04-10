
# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(MetBrewer)
library(plotly)

## Read data file
data <- read.csv(here::here("00-data", "01-xls-processed-data", "D2.csv"))
ylimits <- range(data$avg.scale.score, na.rm = TRUE)

## Define UI
ui <- fluidPage(
    ## Define title
    titlePanel("2019 NAEP Reading Scores for All 50 States"),

    ## Define sidebar layout
    sidebarLayout(
        ## Define sidebar panel
        sidebarPanel(
            selectInput("selected_year", "Choose a year:",
                        choices = unique(data$year),
                        selected = max(data$year))
          , actionButton("quit", "Quit")
        ),
        ## Define main panel
        mainPanel(
            ## Define plot output
            plotOutput("barbell")
        )
    )
)

## Define server
server <- function(input, output) {

    observeEvent(input$quit, {
        stopApp()
    })

    data_filtered <- reactive({

        data %>%
            filter(race.ethnicity %in% c("Black", "White"),
                   year==input$selected_year) %>%
            group_by(jurisdiction) %>%
            mutate(wscore = max(avg.scale.score),
                   bscore = min(avg.scale.score),
                   wbdiff = diff(avg.scale.score)) %>%
            ungroup() %>%
            mutate(jurisdiction = fct_reorder(jurisdiction, wbdiff),
                   rank.gap = ceiling(rank(wbdiff)/2)) %>%
            mutate(picks = case_when(jurisdiction == "Connecticut" ~ "B",
                                     jurisdiction == "National Avg." ~ "A",
                                     !jurisdiction %in% c("Connecticut", "National Avg.") ~ "C")) %>%
            group_by(jurisdiction)

    })

    ## Define plot
    output$barbell <- renderPlot({

        p1 <-
            ggplot(data_filtered(), aes(x=jurisdiction, y=avg.scale.score, alpha=picks,
                                        group=jurisdiction, color=race.ethnicity,
                                        text=paste0(as.character(jurisdiction),
                                                    "\n   Black: ", formatC(bscore, format="f", digits=2),
                                                    "\n   White: ", formatC(wscore, format="f", digits=2),
                                                    "\n   Gap: ", formatC(wbdiff, format="f", digits=2),
                                                    "\n   Rank: ", formatC(rank.gap, format="d")
                                                    ))) +
            geom_line(color="black") +
            geom_point(size=2) +
            scale_color_manual(values=rev(met.brewer("Cassatt2", 2))) +
            scale_alpha_manual(values=c(A = 1, B = 1, C = 1/4)) +
            guides(color="none", alpha="none") +
            scale_y_continuous(limits = ylimits) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.4)) +
            labs(title=paste0("Black/White Gap in NAEP 4th Grade Reading Scores by State -- ", input$selected_year)) +
            ylab ("Average 4th Grade Reading Scores") +
            xlab ("")

        p1

        ## ggplotly(p1, tooltip = c("text"), layerData=1) %>%
        ##     hide_legend() %>%
        ##     config(modeBarButtonsToRemove = c('zoomIn2d', 'zoomOut2d', 'pan2d',
        ##                                       'select2d', 'lasso2d',
        ##                                       'hoverCompare', 'hoverClosestCartesian')) %>%
        ##     layout(xaxis = list(fixedrange = TRUE),
        ##            yaxis = list(fixedrange = TRUE))

    })
}

# Run the app
shinyApp(ui, server)
