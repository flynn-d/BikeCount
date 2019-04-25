rm(list=ls())
library(ggplot2)
library(plotly)
library(shiny)
# source('get_dependencies.R') # Run this once on a new instance, may be time-consuming 
source('Helper_fx.R')
source('Bike_counter_get.R')
source('Make_guesses.R')


ui <- fluidPage(

  tags$div(
    HTML(octocat_badge) # from Helper_fx.R
    ),
  titlePanel('Bike count data'),
  sidebarPanel(
    radioButtons("view", "View data by:",
                 c("Date" = "daily",
                   "Day of Week" = "d_o_w",
                   "Hour of Day" = "hourly",
                   "Month" = "monthly")),
    
    selectInput('ycol', 
                'Y Variable', 
                c('Eastbound','Westbound','Total'),
                selected = 'Total'),
    
     helpText(paste("Last updated", Sys.time()), br(),
              paste("Latest data from", latest_day$date))
    ),
  mainPanel(
    plotlyOutput('plot1'),
    h4(textOutput('latest_text')), br(),
    h4(textOutput('guess_header_text_today')),
    h5(textOutput('reg_guess_text_today')),
    h5(textOutput('ts_guess_text_today')),
    h5(textOutput('rf_guess_text_today')), br(),
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
    
      # Monthly view
      if(input$view == 'monthly'){
        gp <- ggplot(monthly,
                     aes_string(x = 'monthly',
                                y = as.name(input$ycol),
                                color = 'month')) +
          geom_point(aes(text = date)) +
          xlab('Month') +
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
 
  # Today guesses ----
  output$guess_header_text_today <- renderText({
    paste0("Here are the best guesses of " , input$ycol,
           " for today, ", 
           curr_dat$day[1], ", ",
           today, ": ")
  })
  
  output$reg_guess_text_today <- renderText({
    paste("Regression model: \t\t\t\t",
          format(round(get(paste0('regression_guess_', input$ycol, '_today')), 0), big.mark = ","),
          "with weather variables: \t",
          format(round(get(paste0('regression_guess_wx_', input$ycol, '_today')), 0), big.mark = ",")
          
    )})
  
  output$ts_guess_text_today <- renderText({
    paste("Time series model: \t\t\t\t", 
          format(round(get(paste0('ts_guess_', input$ycol)), 0), big.mark = ",")
    )})
  
  output$rf_guess_text_today <- renderText({
    paste("Machine learning model: \t\t\t\t", 
          format(round(get(paste0('rf_guess_', input$ycol, '_today')), 0), big.mark = ","),
          "with weather variables: \t",
          format(round(get(paste0('rf_guess_wx_', input$ycol, '_today')), 0), big.mark = ",")
    )})
  
  # Tomorrow guesses ----
  output$guess_header_text <- renderText({
    paste0("Here are the best guesses of " , input$ycol,
          " for tomorrow, ", 
          tomorrow_dat$day[1], ", ",
          tomorrow, ": ")
    })
  
  output$reg_guess_text <- renderText({
    paste("Regression model: \t\t\t\t",
          format(round(get(paste0('regression_guess_', input$ycol)), 0), big.mark = ","),
          "with weather variables: \t",
          format(round(get(paste0('regression_guess_wx_', input$ycol)), 0), big.mark = ",")
                
    )})
  
  output$ts_guess_text <- renderText({
    paste("Time series model: \t\t\t\t", 
          format(round(get(paste0('ts_guess_', input$ycol)), 0), big.mark = ",")
    )})
  
  output$rf_guess_text <- renderText({
    paste("Machine learning model: \t\t\t\t", 
          format(round(get(paste0('rf_guess_', input$ycol)), 0), big.mark = ","),
          "with weather variables: \t",
          format(round(get(paste0('rf_guess_wx_', input$ycol)), 0), big.mark = ",")
    )})
  
  session$allowReconnect(TRUE) # change to TRUE for server
  
  }

shinyApp(ui, server)