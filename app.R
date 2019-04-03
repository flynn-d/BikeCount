library(ggplot2)
library(plotly)

ui <- pageWithSidebar(
  headerPanel('Bike count data'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', c('date','day_of_week','year'),
                selected = 'date'),
    selectInput('ycol', 'Y Variable', c('entries','exits','total'),
                selected = 'total')
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)

server <- function(input, output, session) {
    source('Bike_counter_get.R')

    # selectedData <- reactive({
    #   daily[, c(input$xcol, input$ycol)]
    # })
    #usevars <- reactive({c(input$xcol, input$ycol)})
      
    output$plot1 <- renderPlotly({
      gp = ggplot(daily, 
                  aes_string(x = as.name(input$xcol), y = as.name(input$ycol))) +
        geom_point() + theme_bw() 
      
      print(ggplotly(gp))
      #plot(selectedData(), aes(x = selectedData()[,1], y = selectedData()[,2]))
          })

}


shinyApp(ui, server)
