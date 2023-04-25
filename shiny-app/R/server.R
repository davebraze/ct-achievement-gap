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
        df0 %>%
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

    ## NAEP by state barbell plot
    output$naep_gap_by_state <- renderPlotly({

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

    ## selected state by year barbell plot
    output$selected_juris_naep_gap_by_year <- renderPlotly({

        D2.selected_juris <-
            df0 %>%
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

    ## NAEP by state spaghetti plot
    output$naep_by_state <- renderPlotly({

        D2.ctx  <-
            df0 %>%
            filter(race.ethnicity == "All students",
                   year >= 2003) %>%
            mutate(picks = case_when(jurisdiction == input$selected_juris ~ "B",
                                     jurisdiction == "National Avg." ~ "A",
                                     !jurisdiction %in% c(input$selected_juris, "National Avg.") ~ "C")) %>%
            group_by(year) %>%
            mutate(rank = min_rank(desc(avg.scale.score))) %>%
            ungroup()

            p1 <-
                D2.ctx %>%
                ggplot(aes(x=year, y=avg.scale.score,
                           group=jurisdiction, color=picks, alpha=picks,
                           text=paste0(as.character(jurisdiction),
                                       "\n   Year: ", formatC(year, format="d"),
                                       "\n   Score: ", formatC(avg.scale.score, format="f", digits=2),
                                       "\n   Rank: ", formatC(rank, format="d")))) +
                geom_point() +
                geom_line(size=1.1) +
                scale_color_manual(values=met.brewer("Tiepolo", 8)[c(1,8,4)]) +
                scale_alpha_manual(values=c(A = .9, B = .9, C = 1/8)) +
                scale_x_continuous(breaks=seq(2003, 2019, 2), name="") +
                scale_y_continuous(limits = ylimits) +
                labs(title="NAEP 4th Grade Reading 2003-19") +
                theme_minimal() +
                ylab ("Average 4th Grade Reading Scores")

                ggplotly(p1, tooltip = c("text"), layerData=1) %>%
                    hide_legend() %>%
                    config(modeBarButtonsToRemove = c('zoomIn2d', 'zoomOut2d', 'pan2d',
                                                      'select2d', 'lasso2d',
                                                      'hoverCompare', 'hoverClosestCartesian')) %>%
                    layout(xaxis = list(fixedrange = TRUE),
                           yaxis = list(fixedrange = TRUE))


    })

    ## table of R packages used in this shiny app
    output$about_r_table <-
        sessioninfo::package_info() %>%
        unclass() %>%
        as_tibble() %>%
        filter(attached == TRUE) %>%
        select(package, ondiskversion, date) %>%
        rename(version = ondiskversion) %>%
        renderTable()

}
