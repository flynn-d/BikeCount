rm(list=ls())
library(ggplot2)
library(plotly)
source('Bike_counter_get.R')
source('Make_guesses.R')

ui <- fluidPage(

  tags$div(
    HTML('<a href="https://github.com/flynn-d/BikeCount" class="github-corner" aria-label="View source on GitHub"><svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true"><path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path><path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path><path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path></svg></a><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>')
    ),
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
          format(as.numeric(latest_day[,input$ycol]), big.mark = ",")
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