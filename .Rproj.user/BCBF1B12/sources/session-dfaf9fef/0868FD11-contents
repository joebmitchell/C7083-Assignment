library(shiny)

ui <- fluidPage(
  titlePanel("Merge CSV Files"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose first CSV file",
                accept = c(".csv")),
      fileInput("file2", "Choose second CSV file",
                accept = c(".csv")),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      tableOutput("merged"),
      tableOutput("unmatched")
    )
  )
)

server <- function(input, output) {
  data1 <- reactive({
    file1 <- input$file1
    if(is.null(file1)){
      return(NULL)
    }
    read.csv(file = file1$datapath, header = TRUE)
  })
  
  data2 <- reactive({
    file2 <- input$file2
    if(is.null(file2)){
      return(NULL)
    }
    read.csv(file = file2$datapath, header = TRUE)
  })
  
  merged_data <- reactive({
    if(is.null(data1()) || is.null(data2())){
      return(NULL)
    }
    merge(data1(), data2(), all=TRUE)
  })
  
  unmatched_data <- reactive({
    if(is.null(data1()) || is.null(data2())){
      return(NULL)
    }
    if(nrow(data1()) == 0 || nrow(data2()) == 0){
      return(NULL)
    }
    d1 <- data1()[!(data1()[,1] %in% data2()[,1]),]
    d2 <- data2()[!(data2()[,1] %in% data1()[,1]),]
    rbind(d1, d2)
  })
  
  output$merged <- renderTable({
    if(is.null(merged_data())){
      return(NULL)
    }
    merged_data()
  })
  
  output$unmatched <- renderTable({
    if(is.null(unmatched_data())){
      return(NULL)
    }
    if(nrow(unmatched_data()) == 0){
      return(NULL)
    }
    unmatched_data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("merged-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(merged_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
