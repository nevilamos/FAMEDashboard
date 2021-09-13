ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  useShinydashboard(),
  # Application title
  titlePanel(h2("Fire Environmental Resilience Metrics", align = "center")),

  # Sidebar with selectors
  fluidRow(
    column(
      width = 2,
      selectInput(
        inputId = "DELWP_REGION",
        label = "DELWP REGION OR STATE",
        choices = c(
          "ALL",
          delwpRegions
        )
      ),
      selectInput(
        inputId = "FIRE_REGION",
        label = "FIRE REGION OR STATE",
        choices = c(
          "ALL",
          fireRegions
        )
      ),
      selectInput(
        inputId = "FIRE_DISTRICT",
        label = "FIRE DISTRICT OR STATE",
        choices = c(
          "ALL",
          fireDistricts
        )
      ),
      selectInput(
        inputId = "FMZ",
        label = "FIRE MANAGEMENT ZONE",
        choices = c(
          "ALL",
          fireFMZ
        )
      ),
      selectInput(
        inputId = "EFG_NAME",
        label = "EFG SELECTION",
        choices = c(
          "ALL EFG",
          efgNames
        )
      ),
      sliderInput("SEASONS",
        label = "Min and Max Season to plot",
        min = min(seasons),
        max = max(seasons),
        value = c(min(seasons), max(seasons)),
        sep = "",
        step = 1
      ),
      sliderInput("BASELINE",
        label = "Select baseline year(s) for species' relative abundance calculations and GMRA",
        min = min(seasons),
        max = max(seasons),
        value = c(min(seasons), min(seasons)),
        sep = "",
        step = 1
      ),
      selectInput("raSpChoices",
        choices = NULL,
        label = "Choose species for species trends plot",
        selected = NULL,
        multiple = T
      )
    ),

    # Set of tabs for various output graphics
    column(
      width = 10,
      tabsetPanel(
        id = "Tabpanel1",
        type = "tabs",

        # Tab summarising species, BBTFI and TFI status metrics
        tabPanel(
          title = "SUMMARY",
          fluidRow(
            width = 12,
            column(
              width = 6,
              fluidRow(
                width = 6,
                box(
                  width = 12,
                  title = textOutput("gmaTitle"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("gmaMessage"),
                  plotlyOutput("gmaPlot", height = 400)
                )
              ),
              fluidRow(
                width = 6,
                box(
                  width = 12,
                  title = ("Number of species below threshold"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("finalSeasonBelowThreshold")
                )
              ),
              fluidRow(
                width = 6,
                box(
                  width = 6,
                  title = ("Species with largest decline"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("finalSeasonDecreasers")
                ),
                box(
                  width = 6,
                  title = ("Species with largest increase"),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("finalSeasonIncreasers")
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                width = 12,
                box(
                  width = 12,
                  title = "Overall TFI Status",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("tfiOverallPlot")
                ),
                box(
                  width = 12,
                  title = "Overall Area Burned Below TFI",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("bbtfiOverallPlot")
                )
              )
            )
          )
        ),
        tabPanel(
          "TFI STATUS",
          plotOutput("tfiPlot", width = "100%", height = 900)
        ),
        tabPanel(
          "BBTFI",
          plotOutput("bbtfiPlot", width = "100%", height = 900)
        ),
        tabPanel(
          "SPECIES TRENDS",
          textOutput("deltaRAPlotText"),
          plotOutput("deltaRAPlot", width = "100%", height = 700)
        )
        # ,
        #
        # tabPanel(
        #     "Tables",
        #     h4("Miscellaneous output tables",align = "center"),
        #     h5("Number of species below threshold by year"),
        #     tableOutput("belowThreshTable")
        #
        # )
      )
    )
  )
)
