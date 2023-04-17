
# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(MetBrewer)
library(plotly)

## Read data file
data <-
    read.csv(here::here("00-data", "01-xls-processed-data", "D2.csv")) %>%
    filter(year >= 2003)

ylimits <- range(data$avg.scale.score, na.rm = TRUE)

ui <- fluidPage(
    titlePanel("4th Grade NAEP Reading Score Gap by State"),

    tabsetPanel(
        tabPanel("Race Gap",
                 fluidRow(
                     column(2, ## controls
                            h3("Controls"),
                            selectInput("selected_year", "YEAR:",
                                        choices = unique(data$year),
                                        selected = max(data$year))
                          , selectInput("selected_juris", "JURISDICTION:",
                                        choices = unique(data$jurisdiction),
                                        selected = "Connecticut")
                          , actionButton("quit", "QUIT")
                            ),
                     column(7, ## plots
                            plotlyOutput("naep_by_state")
                          , plotlyOutput("ct_naep_by_year")
                            ),
                     column(3, ## Explainer
                            h4("Upper Panel"),
                            p("Use the 'jurisdiction' dropdown on the left to select a US state or other jurisdiction. The selected jurisdiction and the National Average will be highlighted."),
                            p("States are ordered from smallest gap (leftmost) to largest (rightmost). Lower rank as displayed in tooltip indicates smaller gap in achievement. Lower rank is better."),
                            p("Samples of black students for states at the far right were too small to reliably estimate state averages for the group; no gap can be computed for these states."),

                            h4("Lower Panel"),
                            p(paste0("The lower figure shows the magnitude of the achievement gap for the selected jurisdiction (NAEP 4th grade reading scores / White vs. African American students) over the most recent 18 year period."))
                            )
                 )
                 ),
        tabPanel("About",
                 fluidRow(
                     column(6,
                            h3("About the NAEP"),
                            p("The National Assessment of Educational Progress (NAEP) is a federally mandated project administered by the National Center for Education Statistics (NCES) within the Department of Education's Institute of Education Sciences. It was initiated in 1969 to serve as a nationally representative assessment of what U.S. students know and can do in various academic subjects, and how that changes over time. The NAEP is designed primarily to provide group-level data on student achievement across subjects."),
                            p("Periodic reports based on NAEP results are commonly known as ",
                              strong("The Nation's Report Card. "),
                              "These reports  aggregate data at the state level. The NCES does not release NAEP results for individual students, classrooms, or schools. For each state, NCES does report aggregate NAEP results for various demographic groups, including divisions by gender, socioeconomic status, and race/ethnicity."),
                            p("Assessment of mathematics, reading, and science takes place in odd numbered calendar years. Test results for these core subjects are collected for grades 4, 8, and 12. Assessment of other subjects occurs less frequently and in fewer grades, and typically in even numbered years. Subjects include the arts, civics, economics, geography, technology and engineering, and U.S. history.")
                            ),
                     column(3,
                            h3("About the Author"),
                            p("David Braze is a researcher and consultant with a background in linguistics and reading research. He has more than 25 years experience investigating the cognitive foundations of language, literacy, and educational achievement, including 17 years as a Senior Research Scientist at Haskins Laboratories. His research at Haskins, funded by the National Institutes of Health, emphasized the neurobiology of language and reading and applications to education. Dr. Braze consults for business, government, and non-profit sectors.")
                            ),
                     column(3,
                            h3("About the Software"),
                            p("All data summaries in this dashboard were produced with the",
                              strong("R"), " statistical environment, version ",
                              paste(version$major, version$minor, sep="."),
                              ". The dashboard itself was made using an Rmarkdown workflow. The following",
                              " table lists the non-base R packages used in building the dashboard. ",
                              "To see a full citation for a specific package, assuming you have both",
                              strong("R"),
                              "and the particular package installed, call (e.g.) `citation(\"dplyr\")` from the",
                              strong("R"), " prompt. ")
                            )
                 )
                 )
    )
)

##### server
    server <- function(input, output) {

        ## update current jurisdiction
        juris <- reactive({
            sprintf("%s", input$selected_juris)
        })

        ## "quit" button
        observeEvent(input$quit, {
            stopApp()
        })

        ## reactive object for main data
        data_all_states <- reactive({
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
                mutate(picks = case_when(jurisdiction == input$selected_juris ~ "B",
                                         jurisdiction == "National Avg." ~ "A",
                                         !jurisdiction %in% c(input$selected_juris, "National Avg.") ~ "C")) %>%
                group_by(jurisdiction)
        })

        ## Define upper panel plot
        output$naep_by_state <- renderPlotly({

            p1 <-
                ggplot(data_all_states(), aes(x=jurisdiction, y=avg.scale.score, alpha=picks,
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

            ggplotly(p1, tooltip = c("text"), layerData=1) %>%
                hide_legend() %>%
                config(modeBarButtonsToRemove = c('zoomIn2d', 'zoomOut2d', 'pan2d',
                                                  'select2d', 'lasso2d',
                                                  'hoverCompare', 'hoverClosestCartesian')) %>%
                layout(xaxis = list(fixedrange = TRUE),
                       yaxis = list(fixedrange = TRUE))

        })

        ## Define lower panel plot
        output$ct_naep_by_year <- renderPlotly({

            D2.selected_juris <-
                data %>%
                filter(race.ethnicity %in% c("Black", "White"),
                       jurisdiction == input$selected_juris,
                       year >= 2003) %>%
                group_by(year) %>%
                mutate(wscore = max(avg.scale.score),
                       bscore = min(avg.scale.score),
                       wbdiff = diff(avg.scale.score)) %>%
                ungroup() %>%
                mutate(picks = case_when(year == input$selected_year ~ "A",
                                         year != input$selected_year ~ "B"))
                p3 <-
                    D2.selected_juris %>%
                    ggplot(aes(x=year, y=avg.scale.score, alpha=picks,
                               group=year, color=race.ethnicity,
                               text=paste0(as.character(jurisdiction),
                                           "\n  Black: ", formatC(bscore, format="f", digits=2),
                                           "\n  White: ", formatC(wscore, format="f", digits=2),
                                           "\n  Gap: ", formatC(wbdiff, format="f", digits=2)
                                           ))) +
                    geom_line(color="black") +
                    geom_point(size=2) +
                    scale_color_manual(values=rev(met.brewer("Cassatt2", 2))) +
                    scale_alpha_manual(values=c(A = 1, B = 1/4)) +
                    scale_x_continuous(breaks=seq(2003, 2019, 2), name="") +
                    scale_y_continuous(limits = ylimits) +
                    theme_minimal() +
                    labs(title=paste0("Black/White Gap in ", input$selected_juris,
                                      " 4th Grade Reading Scores by Year")) +
                    ylab ("Average 4th Grade Reading Scores") +
                    xlab ("")

                    ggplotly(p3, tooltip="text") %>%
                        hide_legend() %>%
                        config(modeBarButtonsToRemove = c('zoomIn2d', 'zoomOut2d', 'pan2d',
                                                          'select2d', 'lasso2d',
                                                          'hoverCompare', 'hoverClosestCartesian')) %>%
                        layout(xaxis = list(fixedrange = TRUE),
                               yaxis = list(fixedrange = TRUE))

        })
    }

# Run the app
shinyApp(ui, server)
