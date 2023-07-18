server <- function(input, output, session) {
  rv <- reactiveValues()
  
  # read in data , process to find possible values of filter fields and update UI as required----
  #myInData <- selectFileServer(id = "selectInputData",root_dirs = c(data = "./data"),filetypes = "qs" )
  myInData<-selectFileServer(id = "selectInputData",root_dirs = c(home = ".."),filetypes = "qs" )
  observe(rv$myDataPath<-myInData$datapath)
  output$myDataPath<-renderText(rv$myDataPath)
  observeEvent(input$LoadDataFile, ignoreInit = T, {
    inData <- qread(rv$myDataPath)
    
    efgNames <- sort(unique(inData$TFI$EFG_NAME))
    fireRegions <- sort(unique(inData$TFI$FIRE_REGION_NAME))
    delwpRegions <- sort(unique(inData$TFI$DELWP_REGION))
    
    fireFMZ <- sort(unique(inData$TFI$FIRE_FMZ_NAME))
    fireDistricts <- levels(sort(unique(inData$TFI$DISTRICT_N)))
    spNames <- sort(unique(inData$RA$COMMON_NAME))
    
    updateSelectInput(session,
                      inputId = "FIRE_REGION",
                      choices = c("ALL", fireRegions))
    if (!is.null(fireDistricts)){
      updateSelectInput(session,
                        inputId = "FIRE_DISTRICT",
                        choices = c("ALL",fireDistricts))}
    updateSelectInput(session,
                      inputId = "DELWP_REGION",
                      choices = c("ALL", delwpRegions))
    updateSelectInput(session,
                      inputId = "FMZ",
                      choices = c("ALL", fireFMZ))
    updateSelectInput(session,
                      inputId = "EFG_NAME",
                      choices = c("ALL", efgNames))
    updateSelectInput(session, inputId = "raSpChoices", choices = c(spNames))
    
    seasons <- sort(unique(inData$TFI$SEASON))
    myMin = min(seasons)
    myMax = max(seasons)
    updateSliderInput(
      session,
      inputId = "SEASONS",
      min = myMin,
      max = myMax,
      value = c(myMin, myMax)
    )
    updateSliderInput(
      session,
      inputId = "BASELINE",
      min = myMin,
      max = myMax,
      value = c(myMin, myMin)
    )
    
    
    
    rv$TFI <- inData$TFI %>%
      ungroup() %>%
      mutate(TFI_STATUS = as.character(TFI_STATUS)) %>%
      mutate(TFI_STATUS = replace_na(TFI_STATUS, "NO DATA")) %>%
      mutate(TFI_STATUS = factor(x = TFI_STATUS,
                                 levels = tfiLevels)) %>%
      filter(!is.na(EFG_NAME))
    
    
    rv$BBTFI <- inData$BBTFI %>%
      ungroup() %>%
      filter(!is.na(EFG_NAME)) %>%
      filter(!is.na(TBTFI)) %>%
      mutate(TBTFI = ifelse(TBTFI > 4, "5+", as.character(TBTFI))) %>%
      mutate(TBTFI = factor(x = TBTFI, levels = c("5+", as.character(4:1)))) %>%
      rename(SEASON = SEAS)
    
    rv$RA <- inData$RA
    
    rv$TaxonList <- inData$TaxonList %>%
      mutate(VIC_ADVISORY_STATUS = ifelse(
        is.na(VIC_ADVISORY_STATUS),
        "Least Concern",
        VIC_ADVISORY_STATUS
      ))
    
    
    
    
    
    
  })
  
  
  #observers to deal with only allowing selection of either Fire region/ district or DELWP region  ---------------
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
  
  tfiFiltered <- reactive(if (isTruthy(rv$TFI)) {
    rv$TFI %>%
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
        if (input$EFG_NAME != "ALL") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(EFG_NAME, TFI_STATUS, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))
  })
  
  
  #
  tfiOverall <- reactive(if (isTruthy(tfiFiltered())) {
    tfiFiltered() %>%
      group_by(TFI_STATUS, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))
  })
  # # TFI status plot ----
  output$tfiPlot <- renderPlot(if (isTruthy(tfiFiltered())) {
    tfiFiltered() %>%
      {
        if (input$SEASONS[1] < input$SEASONS[2]) {
          ggplot(., aes(
            x = SEASON,
            y = Hectares,
            fill = TFI_STATUS
          )) +
            geom_col() +
            tfiScale() +
            facet_wrap(facets = ~ EFG_NAME,
                       scales = "free",
                       ncol = 6)
        } else {
          ggplot(., aes(
            x = EFG_NAME,
            y = Hectares,
            fill = TFI_STATUS
          )) +
            geom_col(position = "fill") +
            tfiScale() +
            theme(axis.text.x = element_text(
              angle = 90,
              vjust = 0.5,
              hjust = 1
            ))
        }
      } +
      labs(x = "Fire Year") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  }
  else{
    NULL
  })
  #   # TFI status  overall summary plot ----
  output$tfiOverallPlot <- renderPlot(if (isTruthy(tfiOverall())) {
    ggplot(tfiOverall(),
           aes(x = SEASON, y = Hectares, fill = TFI_STATUS)) +
      geom_col(position = "fill") +
      tfiScale() +
      theme(axis.text.x = element_text(
        angle = -90,
        vjust = 0.5,
        hjust = 1
      )) +
      labs(x = "Fire Year", y = "Proportion of total area")
  } else{
    NULL
  })
  
  #
  # #  Filtering BBTFI plots  -----
  
  bbtfiFiltered <- reactive(if (isTruthy(rv$BBTFI)) {
    rv$BBTFI %>%
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
        if (input$EFG_NAME != "ALL") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(EFG_NAME, TBTFI, SEASON) %>%
      summarise(Hectares = sum(Hectares, na.rm = T))
  })
  
  bbtfiOverall <-
    reactive(if (isTruthy(bbtfiFiltered())) {
      bbtfiFiltered() %>%
        group_by(TBTFI, SEASON) %>%
        summarise(Hectares = sum(Hectares, na.rm = T))
    })
  
  output$bbtfiPlot <-
    renderPlot(if (isTruthy(bbtfiFiltered())) {
      bbtfiFiltered() %>%
        ggplot(aes(x = SEASON, y = Hectares, fill = TBTFI)) +
        geom_col() +
        scale_fill_brewer(palette = "YlOrRd", direction = -1) +
        facet_wrap(facets = ~ EFG_NAME,
                   scales = "free_y",
                   ncol = 6) +
        labs(x = "Fire Year") +
        theme(
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    } else{
      NULL
    })
  
  output$bbtfiOverallPlot <-
    renderPlot(if (isTruthy(bbtfiOverall())) {
      bbtfiOverall() %>%
        ggplot(aes(x = SEASON, y = Hectares, fill = TBTFI)) +
        geom_col() +
        scale_fill_brewer(palette = "YlOrRd", direction = -1) +
        labs(x = "Fire Year") +
        theme(
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.background = element_rect(fill = "grey70"),
          axis.line = element_line(color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank()
        )
    } else{
      NULL
    })
  
  
  # RA plots and summary  -----
  
  
  sppSumm <- reactive(if (isTruthy(rv$RA)) {
    rv$RA %>%
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
        if (input$EFG_NAME != "ALL") {
          filter(., EFG_NAME == input$EFG_NAME)
        } else {
          (.)
        }
      } %>%
      group_by(SEASON, TAXON_ID, COMMON_NAME) %>%
      summarise(sumRA = sum(sumRA, na.rm = T), .groups = "drop")
  })
  sppSumm2 <- reactive(if (isTruthy(sppSumm())) {
    sppSumm() %>%
      left_join(baselines()) %>%
      left_join(rv$TaxonList) %>%
      mutate(deltaRA = sumRA / baseline) %>%
      mutate(belowThresh = deltaRA < CombThreshold)
  })
  #
  baslineText <- reactive({
    ifelse(
      length(input$BASELINE[1]:input$BASELINE[2]) == 1,
      as.character(input$BASELINE[1]),
      paste(input$BASELINE[1],
            "to",
            input$BASELINE[2])
    )
  })
  #
  baselines <- reactive({
    sppSumm() %>%
      dplyr::filter(SEASON %in% input$BASELINE[1]:input$BASELINE[2]) %>%
      group_by(TAXON_ID) %>%
      summarise(baseline = mean(sumRA))
  })
  #
  
  
  # # Calculation of number of species declining below thresholds--------------------
  # # sort order for Conservation_Status columns
  belowThrshSummNames <- c(
    "Critically endangered",
    "Endangered",
    "Vulnerable",
    "Near threatened",
    "Data deficient",
    "Least Concern",
    "Total"
  )
  # # table of number below threshold for each SEASON by Conservation_Status and total
  belowThreshSummLong <-
    reactive(if (isTruthy(sppSumm2())) {
      sppSumm2() %>%
        group_by(SEASON, VIC_ADVISORY_STATUS) %>%
        summarise(nBelowThresh = sum(belowThresh, na.rm = T)) %>%
        # rbind(sppSumm %>%
        # group_by(SEASON) %>%
        # summarise(nBelowThresh = sum(belowThresh,na.rm=T)) %>%
        # mutate(VIC_ADVISORY_STATUS = "Total")) %>%
        mutate(VIC_ADVISORY_STATUS = factor(VIC_ADVISORY_STATUS,
                                            levels = belowThrshSummNames)) %>%
        rename(Conservation_Status = VIC_ADVISORY_STATUS)
    })
  #
  belowThreshSumm <-
    reactive(if (isTruthy(belowThreshSummLong())) {
      belowThreshSummLong() %>%
        pivot_wider(
          names_from = "Conservation_Status",
          values_from = nBelowThresh,
          names_sort = TRUE
        )
    })
  #
  #
  # # filtering just the last season for total decreasing and  biggest increases and decreases on summary---
  finalSeason <- reactive(max(levels(belowThreshSumm()$SEASON)))
  
  output$finalSeasonBelowThreshold <-
    renderTable(if (isTruthy(belowThreshSumm())) {
      belowThreshSumm() %>%
        filter(SEASON %in% finalSeason()) %>%
        select(-SEASON)
    })
  
  output$finalSeasonIncreasers <-
    renderTable(if (isTruthy(sppSumm2())) {
      sppSumm2() %>%
        filter(SEASON %in% finalSeason()) %>%
        slice_max(order_by = deltaRA, n = 5) %>%
        select(COMMON_NAME, deltaRA) %>%
        filter(deltaRA > 1)
    })
  
  output$finalSeasonDecreasers <-
    renderTable(if (isTruthy(sppSumm2())) {
      sppSumm2() %>%
        filter(SEASON %in% finalSeason()) %>%
        slice_min(order_by = deltaRA, n = 5) %>%
        select(COMMON_NAME, deltaRA) %>%
        filter(deltaRA < 1)
    })
  
  
  spChoices <- reactive(sort(unique(sppSumm2()$COMMON_NAME)))
  
  
  
  # delta RA  plot ----
  #if (length(input$raSpChoices>0)){
  
  output$deltaRAPlot <-
    renderPlot(if (isTruthy(sppSumm2())) {
      sppSumm2() %>%
        filter(COMMON_NAME %in% input$raSpChoices) %>%
        ggplot() +
        geom_line(aes(
          x = SEASON,
          y = deltaRA,
          group = COMMON_NAME,
          color = COMMON_NAME
        )) +
        theme(
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          text = element_text(size = 16)
        )
    } else{
      NULL
    })
  # }else{
  #      output$deltaRAPlot <- renderPlot(NULL)
  #    }
  #  )
  #     )
  #
  #   output$deltaRAPlotText<-renderText(NULL)
  #
  #   }else{
  #
  #   output$deltaRAPlotText<-renderText("Select species to show trends")
  # }
  
  # GMRA calculation ----
  gma <- reactive(sppSumm2() %>%
                    # drop_na() %>%
                    group_by(SEASON) %>%
                    summarise(GMRA = geoMean(deltaRA)) %>%
                    ungroup())
  
  
  #if (reactive(nrow(gma())) > 0) {
  output$gmaTitle <- renderText(
    paste(
      "Geometric mean of species' abundances (GMRA)\nrelative to baseline of",
      baslineText(),
      "\n and count of species below threshold abundance\n"
    )
  )
  
  
  output$gmaPlot <- renderPlotly(if (isTruthy(belowThreshSummLong())) {
    if (nrow(belowThreshSummLong()) > 0) {
      plot_ly(belowThreshSummLong()) %>%
        add_bars(
          data = belowThreshSummLong(),
          x ~ SEASON,
          y = ~ nBelowThresh,
          color = ~ Conservation_Status,
          yaxis = "y"
        ) %>%
        add_lines(
          data = gma(),
          x = ~ SEASON,
          y = ~ GMRA,
          name = "GMRA",
          type = "scatter",
          yaxis = "y2"
        ) %>%
        layout(
          barmode = "stack",
          yaxis = list(title = "Number species below threshold",
                       rangemode = "tozero"),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            automargin = T,
            title = "GMRA",
            rangemode = "tozero"
          ),
          xaxis = list(title = "Fire Year")
        )
    }
  })
  output$gmaMessage <- renderText(NULL)
  # } else {
  #   output$gmaPlot <- renderPlotly({NULL})
  #   output$gmaMessage <- renderText("There are no data to calculate a GMRA for this selection")
  # }
  
}
