library(ggplot2)
library(plotly)
library(Cairo)

ui <- pageWithSidebar(
  headerPanel('Bike count data'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', c('date','day_of_week','year'),
                selected = 'date'),
    selectInput('ycol', 'Y Variable', c('entries','exits','total'),
                selected = 'total')
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output, session) {
    source('Bike_counter_get.R')

    selectedData <- reactive({
      daily[, c(input$xcol, input$ycol)]
    })

    output$plot1 <- renderPlot({
      # gp = ggplot(selectedData(), aes(x = selectedData()[,1], y = selectedData()[,2])) +
      #   geom_point()
      # ggplotly(gp)
      plot(selectedData(), aes(x = selectedData()[,1], y = selectedData()[,2]))
          })

}


shinyApp(ui, server)
