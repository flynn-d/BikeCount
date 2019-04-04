library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel('Bike count data'),
    
  sidebarPanel(
    # Input: Select the view ----
    radioButtons("view", "View data by:",
                 c("Date" = "daily",
                   "Day of Week" = "d_o_w",
                   "Hour of Day" = "hourly")),
    
    selectInput('ycol', 'Y Variable', c('exits','entries','total'),
                selected = 'total'),
    
     helpText(paste("Last upated", Sys.time()), br(),
              paste("Latest data from", latest_day$date))
    ),
  mainPanel(
    plotlyOutput('plot1')
  )
)

server <- function(input, output, session) {
    source('Bike_counter_get.R')
      
    output$plot1 <- renderPlotly({
      
      # Daily view
      if(input$view == 'daily'){
        gp = ggplot(daily,
                    aes_string(x = 'date', 
                               y = as.name(input$ycol), 
                               color = 'day_of_week')) +
          geom_point() + 
          xlab('Date') +
          theme_bw() 
        
      }
      
      # Hourly view
      if(input$view == 'hourly'){
        
        gp <- ggplot(hourly_hour_month, 
                     aes_string(x = 'hour', 
                                y = as.name(input$ycol), 
                                color = 'month')) +
          geom_point() + geom_smooth(se = F, span = 0.3) +
          xlab('Hour of day') +  
          theme_bw()
        
      }
      
      # Day of week view
      if(input$view == 'd_o_w'){
        gp <- ggplot(daily, 
                     aes_string(x = 'day_of_week', 
                                y = as.name(input$ycol), 
                                color = 'year')) +
          geom_point(aes(text = date)) +
          xlab('Day of week') + 
          theme_bw()
        
      }
      
      print(ggplotly(gp))
      
          })

}


shinyApp(ui, server)
