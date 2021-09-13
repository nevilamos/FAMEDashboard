

thematic::thematic_shiny()


ui <- fluidPage(
    
    theme=bs_theme(bootswatch = "darkly"
                   
    ),
    
    
    
    # Application title
    titlePanel(h1("Fire Environmental Resilience Metrics", align = "center")),
    
    # Sidebar with selectors
    fluidRow(
        
        column(width = 2,
               selectInput(
                   inputId = "SEASON",
                   label = "FIRE SEASON",
                   choices = seasons),
               selectInput(inputId = "DELWP_REGION",
                           label = "DELWP REGION OR STATE",
                           choices = c("STATE",
                                       delwpRegions)),
               selectInput(inputId = "EFG_NAME",
                           label = "EFG SELECTION",
                           choices = c("ALL EFG",
                                       efgNames))
               ),
        
        
        column(width = 10,
               
               # Show a plot
               
               plotOutput("tfiPlot",width = "100%",height = 1200)
               
               
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    observeEvent(input$DELWP_REGION,{
        
            output$tfiPlot <-renderPlot({
                TFI %>% 
                    {if(input$DELWP_REGION!="STATE")
                         filter(.,DELWP_REGION == input$DELWP_REGION) else (.)
                    } %>% {
                        {if(input$EFG_NAME!="ALL EFG")
                            filter(.,EFG_NAME == input$EFG_NAME) else (.)
                        }
                }
                    filter(SEASON == input$SEASON) %>% 
                    group_by(EFG_NAME,TFI_STATUS) %>% 
                    summarise(Hectares = sum(Hectares,na.rm = T)) %>% 
                    ggplot(aes(x="",y= Hectares,fill = TFI_STATUS))+
                    geom_col()+
                    facet_wrap(facets = ~ EFG_NAME,scales ="free",ncol=4
                    )
                
            })

    })
}
    


# Run the application 
shinyApp(ui = ui, server = server)
