ui <- fluidPage(
    titlePanel("4th Grade NAEP Reading Score Gap by State"),
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
        column(10,
               tabsetPanel(
                   tabPanel("Race Gap",
                            fluidRow(
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
                                column(4,
                                       h3("About the NAEP"),
                                       p("The National Assessment of Educational Progress (NAEP) is a federally mandated project administered by the National Center for Education Statistics (NCES) within the Department of Education's Institute of Education Sciences. It was initiated in 1969 to serve as a nationally representative assessment of what U.S. students know and can do in various academic subjects, and how that changes over time. The NAEP is designed primarily to provide group-level data on student achievement across subjects."),
                                       p("Periodic reports based on NAEP results are commonly known as ",
                                         strong(em("The Nation's Report Card. ")),
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
                                         strong(em("R")), " statistical environment, version ",
                                         paste(version$major, version$minor, sep="."),
                                         ". The dashboard itself was made using an Rmarkdown workflow. The following",
                                         " table lists the non-base R packages used in building the dashboard. ",
                                         "To see a full citation for a specific package, assuming you have both",
                                         strong(em("R")),
                                         "and the particular package installed, call (e.g.) `citation(\"dplyr\")` from the",
                                         strong(em("R")), " prompt. "),

                                       tableOutput("about_r_table")
                                       )
                            )
                            )
               ))))
