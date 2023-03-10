library(shiny)

source('library.R')

import_library()

dat <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
countries <- dat %>% 
  group_by(country) %>% 
  summarise() %>% 
  as.list()

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Food Consumption and Carbon Footprint"), # Create title
  tabsetPanel(type = "tabs", # Setup tabbed layout
              tabPanel("Consumption by Food Type",
                       plotOutput('raincloud', height = 600)),
              
              tabPanel("Carbon Footprint by Food Type",
                       plotOutput('dotplot')),
              
              tabPanel("All Countries",
                        plotOutput('scatter', click= 'plot_click'),
                        h5("Click on a country to see their consumpution by category"),
                        fluidRow( # Use Fluid row to position user inputs next to graph
                          column(2,
                          checkboxInput("axis1", # Allow user to select if axis are fixed at 1750KG
                                       label = "Fixed Axis",
                                       value = FALSE),
                          selectInput("sort1", # Allow user to select how to sort the bars
                                      "Sort Bars by:",
                                      choices = c("Alphabetically", "CO2", "Consumption"))),
                          column(10,
                          plotOutput('bar'))
                              )
                            ),
              tabPanel("Bar plot",
                       sidebarLayout(
                         sidebarPanel(width = 2, 
                         
                                      selectInput("country1", # allow user to select country
                                                  "Select 1st Country:",
                                                  choices = countries),
                                      selectInput("country2", # allow user to select country
                                                 "Select 2nd Country:",
                                                 choices = countries),
                                      checkboxInput("axis2", # allow user to select if axis are fixed
                                                    label = "Fixed Axis",
                                                    value = FALSE),
                                      
                                      selectInput("sort2", # allow user to select how to sort bars
                                                  "Sort Bars by:",
                                                  choices = c("Alphabetically", "CO2", "Consumption")
                                      )
                                 ),
                         mainPanel(
                         plotOutput("mainbar1"),
                          plotOutput("mainbar2")
                           
                                    )
                                  )
                       ),
 
              tabPanel("Mapping Consumption",
                       fluidRow(
                         tmapOutput("map1", width = "100%"),
                         tmapOutput("map2", width = "100%"))
                       ),
  
              tabPanel("Future CO2 Emissions",
                       plotOutput('future')),
              
  )
)
  
# Define server logic 
server <- function(input, output) {
  # Palette for visualizations
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  scatter_dat = scatter_dat(dat) # setup data for scatter plot
  
  output$scatter <- renderPlot({
    
  scatter_f(scatter_dat) # create scatter plot using scatter data
      })
  
   observeEvent(input$plot_click, { # create action when user clicks on scatter plot
        
      country_name <- reactive({ # sets country_name to nearest point selected
        np <- nearPoints(scatter_dat, input$plot_click, threshold = 10, maxpoints = 1,
                         addDist = TRUE)
        country_name <- as.character(select(np, "NAME_LONG"))
        country_name
      })  
      sorting <- reactive({input$sort1}) # records users sorting preferenece
      fixed_axis <- reactive({input$axis1}) # records users axis preference
      
      output$bar <- renderPlot({
      bar_dat(dat, country_name(), sorting(), fixed_axis()) # Creates bar plot using user actions
      })
})
   
   output$dotplot <- renderPlot({
     
     dotplot_fn(dat) # create dotplot using base dataset
   })
   
 output$map1 <- renderTmap({
   tmap_mode('view') # create interactive tmap
   create_map(dat,TRUE)
 }) 
 output$map2 <- renderTmap({
   tmap_mode('view') # create interactive tmap
   create_map(dat,FALSE)
 }) 
 
 output$raincloud <- renderPlot({
   raincloud_plot(dat) # call raincloud plot using base data
 })
 
 output$mainbar1<- renderPlot({
   bar_dat(dat, input$country1, input$sort2, input$axis2) # call bar plot using base data and user inputs
 })
 
 output$mainbar2<- renderPlot({
   bar_dat(dat, input$country2, input$sort2, input$axis2) # call bar plot using base data and user inputs
 })
 
 output$future <- renderPlot({
   future_co2(dat) # create future co2 plot using base data
 }, height = 675, width = 1000) # set plot size to ensure arrows are in correct place
}

# Run the application 
shinyApp(ui = ui, server = server)
