rm(list=ls())
library(ggplot2)
library(plotly)
source('Bike_counter_get.R')
source('Make_guesses.R')

ui <- fluidPage(
  titlePanel('Bike count data'),
  sidebarPanel(
    radioButtons("view", "View data by:",
                 c("Date" = "daily",
                   "Day of Week" = "d_o_w",
                   "Hour of Day" = "hourly")),
    
    selectInput('ycol', 
                'Y Variable', 
                c('exits','entries','total'),
                selected = 'total'),
    
     helpText(paste("Last updated", Sys.time()), br(),
              paste("Latest data from", latest_day$date))
    ),
  mainPanel(
    plotlyOutput('plot1'),
    h4(textOutput('latest_text')),
    h4(textOutput('guess_header_text')),
    h5(textOutput('reg_guess_text')),
    h5(textOutput('ts_guess_text')),
    h5(textOutput('rf_guess_text'))
    
  )
)

server <- function(input, output, session) {
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
      }) # end renderPlotly
  
  output$latest_text <- renderText({
    paste("Most recent value of", 
          input$ycol, "on",
          latest_day$day_of_week,
          latest_day$date,
          "was",
          latest_day[,input$ycol]
          )})
  
  output$guess_header_text <- renderText({
    paste0("Here are the best guesses of " , input$ycol,
          " for tomorrow, ", 
          tomorrow_dat$day[1], ", ",
          tomorrow, ": ")
    })
  
  output$reg_guess_text <- renderText({
    paste("Regression model: \t\t\t\t",
          format(round(get(paste0('regression_guess_', input$ycol)), 0), big.mark = ",")
    )})
  
  output$ts_guess_text <- renderText({
    paste("Time series model: \t\t\t\t", 
          format(round(get(paste0('ts_guess_', input$ycol)), 0), big.mark = ",")
    )})
  
  output$rf_guess_text <- renderText({
    paste("Machine learning model: \t\t\t\t", 
          format(round(get(paste0('rf_guess_', input$ycol)), 0), big.mark = ",")
    )})
  
  session$allowReconnect(TRUE) # change to TRUE for server
  
  }

shinyApp(ui, server)