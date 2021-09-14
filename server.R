server <- function(input, output, session) {
  rv <- reactiveValues()
  observeEvent(input$DELWP_REGION, {
    if (input$DELWP_REGION != "ALL") {
      updateSelectInput(session, inputId = "FIRE_REGION", selected = "ALL")
      updateSelectInput(session, inputId = "FIRE_DISTRICT", selected = "ALL")
    }
  })
  observeEvent(input$FIRE_REGION, {
    if (input$FIRE_REGION != "ALL") {
      updateSelectInput(session, inputId = "DELWP_REGION", selected = "ALL")
      updateSelectInput(session, inputId = "FIRE_DISTRICT", selected = "ALL")
    }
  })
  
  observeEvent(input$FIRE_DISTRICT, {
    if (input$FIRE_DISTRICT != "ALL") {
      updateSelectInput(session, inputId = "DELWP_REGION", selected = "ALL")
      updateSelectInput(session, inputId = "FIRE_REGION", selected = "ALL")
    }
  })
  



  #  Observer for TFI Status plots ----
  observe({
    tfiFiltered <- TFI %>%
      filter(SEASON %in% input$SEASONS[1]:input$SEASONS[2]) %>%
      {
        if (input$DELWP_REGION != "ALL") {
          filter(., DELWP_REGION == input$DELWP_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_REGION != "ALL") {
          filter(., FIRE_REGION_NAME == input$FIRE_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_DISTRICT != "ALL") {
          filter(., DISTRICT_N == input$FIRE_DISTRICT)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FMZ != "ALL") {
          filter(., FIRE_FMZ_NAME == input$FMZ)
        } else {
          (.)
        }
      } %>%
      {
        if (input$EFG_NAME != "ALL EFG") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(EFG_NAME, TFI_STATUS, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))

    tfiOverall <- tfiFiltered %>%
      group_by(TFI_STATUS, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))
  # TFI status plot ----
    tfiPlot <- tfiFiltered %>%
      {
        if (input$SEASONS[1] < input$SEASONS[2]) {
          ggplot(., aes(x = SEASON, y = Hectares, fill = TFI_STATUS)) +
            geom_col() +
            tfiScale() +
            facet_wrap(
              facets = ~EFG_NAME,
              scales = "free",
              ncol = 6
            )
        } else {
          ggplot(., aes(x = EFG_NAME, y = Hectares, fill = TFI_STATUS)) +
            geom_col(position = "fill") +
            tfiScale() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        }
      } +
      labs(x = "Fire Year") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    # TFI status  overall summary plot ----
    tfiOverallPlot <- ggplot(tfiOverall, aes(x = SEASON, y = Hectares, fill = TFI_STATUS)) +
      geom_col(position = "fill") +
      tfiScale() +
      theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1)) +
      labs(x = "Fire Year", y = "Proportion of total area")


    output$tfiPlot <- renderPlot(tfiPlot)
    output$tfiOverallPlot <- renderPlot(tfiOverallPlot)
  })

  #  Observer for BBTFI plots  -----
  observe({
    bbtfiFiltered <- BBTFI %>%
      filter(SEASON %in% input$SEASONS[1]:input$SEASONS[2]) %>%
      filter(!is.na(EFG_NAME)) %>%
      {
        if (input$DELWP_REGION != "ALL") {
          filter(., DELWP_REGION == input$DELWP_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_REGION != "ALL") {
          filter(., FIRE_REGION_NAME == input$FIRE_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_DISTRICT != "ALL") {
          filter(., DISTRICT_N == input$FIRE_DISTRICT)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FMZ != "ALL") {
          filter(., FIRE_FMZ_NAME == input$FMZ)
        } else {
          (.)
        }
      } %>%
      {
        if (input$EFG_NAME != "ALL EFG") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(EFG_NAME, TBTFI, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))

    bbtfiOverall <- bbtfiFiltered %>%
      group_by(TBTFI, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))

    bbtfiPlot <- bbtfiFiltered %>%
      ggplot(aes(x = SEASON, y = Hectares, fill = TBTFI)) +
      geom_col() +
      scale_fill_brewer(palette = "YlOrRd", direction = -1) +
      facet_wrap(
        facets = ~EFG_NAME,
        scales = "free_y",
        ncol = 6
      ) +
      labs(x = "Fire Year") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )

    bbtfiOverallPlot <- bbtfiOverall %>%
      ggplot(aes(x = SEASON, y = Hectares, fill = TBTFI)) +
      geom_col() +
      scale_fill_brewer(palette = "YlOrRd", direction = -1) +
      labs(x = "Fire Year") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(fill = "grey70"),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )

    output$bbtfiPlot <- renderPlot(bbtfiPlot)
    output$bbtfiOverallPlot <- renderPlot(bbtfiOverallPlot)
  })

  #  Observer for RA plots and summary  -----
  observe({
    sppSumm <- RA %>%
      filter(SEASON %in% input$SEASONS[1]:input$SEASONS[2]) %>%
      filter(!is.na(EFG_NAME)) %>%
      {
        if (input$DELWP_REGION != "ALL") {
          filter(., DELWP_REGION == input$DELWP_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_REGION != "ALL") {
          filter(., FIRE_REGION_NAME == input$FIRE_REGION)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FIRE_DISTRICT != "ALL") {
          filter(., DISTRICT_N == input$FIRE_DISTRICT)
        } else {
          (.)
        }
      } %>%
      {
        if (input$FMZ != "ALL") {
          filter(., FIRE_FMZ_NAME == input$FMZ)
        } else {
          (.)
        }
      } %>%
      {
        if (input$EFG_NAME != "ALL EFG") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(SEASON, TAXON_ID, COMMON_NAME) %>%
      summarise(sumRA = sum(sumRA, na.rm = T), .groups = "drop")
    rv$baslineText <- ifelse(length(input$BASELINE[1]:input$BASELINE[2]) == 1, as.character(input$BASELINE[1]), paste(input$BASELINE[1], "to", input$BASELINE[2]))
    baselines <- sppSumm %>%
      dplyr::filter(SEASON %in% input$BASELINE[1]:input$BASELINE[2]) %>%
      group_by(TAXON_ID) %>%
      summarise(baseline = mean(sumRA))

    rv$sppSumm <- sppSumm %>%
      left_join(baselines) %>%
      left_join(TaxonList) %>%
      mutate(deltaRA = sumRA / baseline) %>%
      mutate(belowThresh = deltaRA < CombThreshold)



    # Calculation of number of species declining below thresholds--------------------
    # sort order for Conservation_Status columns
    belowThrshSummNames <- c(
      "Critically endangered", "Endangered", "Vulnerable",
      "Near threatened", "Data deficient", "Least Concern", "Total"
    )
    # table of number below threshold for each SEASON by Conservation_Status and total
    belowThreshSummLong <- rv$sppSumm %>%
      group_by(SEASON, VIC_ADVISORY_STATUS) %>%
      summarise(nBelowThresh = sum(belowThresh, na.rm = T)) %>%
      # rbind(rv$sppSumm %>%
      # group_by(SEASON) %>%
      # summarise(nBelowThresh = sum(belowThresh,na.rm=T)) %>%
      # mutate(VIC_ADVISORY_STATUS = "Total")) %>%
      mutate(VIC_ADVISORY_STATUS = factor(VIC_ADVISORY_STATUS, levels = belowThrshSummNames)) %>%
      rename(Conservation_Status = VIC_ADVISORY_STATUS)

    belowThreshSumm <- belowThreshSummLong %>%
      pivot_wider(
        names_from = "Conservation_Status",
        values_from = nBelowThresh, names_sort = TRUE
      )


    # filtering just the last season for total decreasing and  biggest increases and decreases on summary---
    finalSeason <- max(levels(belowThreshSumm$SEASON))

    finalSeasonBelowThreshold <- belowThreshSumm %>%
      filter(SEASON %in% finalSeason) %>%
      select(-SEASON)

    biggestIncreasers <- rv$sppSumm %>%
      filter(SEASON %in% finalSeason) %>%
      slice_max(order_by = deltaRA, n = 5) %>%
      select(COMMON_NAME, deltaRA) %>%
      filter(deltaRA > 1)

    biggestDecreasers <- rv$sppSumm %>%
      filter(SEASON %in% finalSeason) %>%
      slice_min(order_by = deltaRA, n = 5) %>%
      select(COMMON_NAME, deltaRA) %>%
      filter(deltaRA < 1)

    output$finalSeasonBelowThreshold <- renderTable(finalSeasonBelowThreshold)
    output$finalSeasonIncreasers <- renderTable(biggestIncreasers)
    output$finalSeasonDecreasers <- renderTable(biggestDecreasers)

    rm(baselines)

    rv$spChoices <- sort(unique(rv$sppSumm$COMMON_NAME))
    
    # delta RA  plot ----
    deltaRAPlot <- rv$sppSumm %>%
      filter(COMMON_NAME %in% input$raSpChoices) %>%
      ggplot() +
      geom_line(aes(x = SEASON, y = deltaRA, group = COMMON_NAME, color = COMMON_NAME)) +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ),
        text = element_text(size = 16)
      )
    if (length(input$raSpChoices>0)){
      output$deltaRAPlot <- renderPlot(deltaRAPlot)
      output$deltaRAPlotText<-renderText(NULL)
    }else{
      output$deltaRAPlot <- renderPlot(NULL)
      output$deltaRAPlotText<-renderText("Select species to show trends")
    }
  # GMRA calculation ----
    gma <-
      rv$sppSumm %>%
      # drop_na() %>%
      group_by(SEASON) %>%
      summarise(GMRA = geoMean(deltaRA)) %>%
      ungroup()
    
    rv$gma <- gma
    if (nrow(rv$gma) > 0) {
      output$gmaTitle <- renderText(paste(
        "Geometric mean of species' abundances (GMRA)\nrelative to baseline of",
        rv$baslineText,
        "\n and count of species below threshold abundance\n"
      ))
      # gmaPlot <-ggplot()+
      #   ggtitle(label = gmaTitle)+
      #   xlab("Fire Year")+
      #   #ylab("Geometric mean of species' relative abundances")+
      #
      #   geom_line(data=rv$gma,aes(x=SEASON,y=GMRA, group = 1))+
      #   geom_col(data =belowThreshSummLong %>% filter(Conservation_Status!="Total"),aes(x=SEASON,y=nBelowThresh,fill=Conservation_Status))+
      #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),text = element_text(size=16))

      # summary page plot of GMRA and number of species below threshold
      #GMRA and count of number os species declining plot ----
      gmaPlot <- plot_ly(data = belowThreshSummLong) %>%
        add_bars(
          data = belowThreshSummLong,
          x ~ SEASON,
          y = ~nBelowThresh,
          color = ~Conservation_Status,
          yaxis = "y"
        ) %>%
        add_lines(
          data = gma,
          x = ~SEASON,
          y = ~GMRA,
          name = "GMRA",
          type = "scatter",
          yaxis = "y2"
        ) %>%
        layout(
          barmode = "stack",
          yaxis = list(title = "Number species below threshold",rangemode="tozero"),
          yaxis2 = list(overlaying = "y", side = "right", automargin = T, title = "GMRA"),
          xaxis = list(title = "Fire Year")
        )


      output$gmaPlot <- renderPlotly({
        gmaPlot
      })
      output$gmaMessage <- renderText(NULL)
    } else {
      output$gmaPlot <- renderPlotly({NULL})
      output$gmaMessage <- renderText("There are no data to calculate a GMRA for this selection")
    }
  })
  observeEvent(rv$sppSumm, {
    updateSelectInput(session, "raSpChoices", choices = rv$spChoices)
  })
}
