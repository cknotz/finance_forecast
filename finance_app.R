
# Stock market data & forecasting app
#####################################

# Carlo Knotz

# Getting packages
library(shiny)
    library(shinydashboard)
    library(dashboardthemes)
    library(ggplot2)
    library(tidyverse)
    library(BatchGetSymbols)
    library(tidyquant)
    library(forecast)
    library(gtrendsR)
    library(seasonal)
    library(nnfor)
    library(gridExtra)
    library(ggiraph)
    library(shinyWidgets)
    library(shinyjs)



ui <- dashboardPage(
    dashboardHeader(title = "Stock Market Data Dashboard", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info", lib = "font-awesome")),
      menuItem("Key short-term indicators", tabName = "indicators", icon = icon("dashboard")),
      menuItem("Forecasting (^DJIA)", tabName = "forecast", icon = icon("chart-line", lib = "font-awesome"))
    )
    ),
    dashboardBody(shinyjs::useShinyjs(),
      shinyDashboardThemes(theme = "grey_dark"),
                  tags$style(type="text/css", "text {font-family: sans-serif}"),
                  tags$style(type="text/css",
                                ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
  background: #ff9900;
  border-color: #ff9900;}
                             .irs-max {font-family: 'arial'; color: white;}
      .irs-min {font-family: 'arial'; color: white;}"),
                  tabItems(
                      tabItem(tabName = "intro",
                              fluidRow(
                                box(width = 12, solidHeader = F,collapsible = F,title = "About this dashboard",
                                    HTML(
                                      "<p>This data dashboard provides up-to-date information about
                                      current stock market developments and forecasts of
                                      the Dow Jones Industrial Average (^DJIA), a leading stock market index and
                                      key indicator of worldwide stock market trends.</p>
                                      
                                      <p>The forecasts are based on historical Dow Jones index values from 
                                      January 1st, 1985 through today and estimated using classical forecasting
                                      techniques as well as neural networks,
                                      building on Hyndman & Athanasopoulos (<a href='https://otexts.com/fpp2/' 
                                      target='_blank'>2018</a>) and Kourentzes (<a 
                                      href='https://cran.r-project.org/web/packages/nnfor/index.html' target='_blank'>2019</a>).</p>
                                      
                                      <p>The short-term indicators are selected based (roughly) on this helpful Medium 
                                      <a href='https://medium.com/concoda/how-to-predict-major-market-shifts-99419ae1d23c'
                                       target='_blank'>post</a> and the <i>Nature</i> article by Preis et al. 
                                      (<a href='https://doi.org/10.1038/srep01684' target='_blank'>2013</a>).</p>
                                      <p>This is version 1.0. Future versions will include prediction ranges for 
                                      the neural network forecasts and customizable forecast horizons.</p>"
                                      ))
                              )),
                      tabItem(tabName = "indicators",
                              fluidRow(
                                  box(width=12,solidHeader = T,collapsible = T,collapsed = F,title = "Info",
                                      HTML(
                                      "<p>The interactive graphs below show the recent (last 90 days) development of some selected indicators of
                                        investor sentiment. These include:</p>
                                      <ul>
                                        <li>High-yield or 'junk' bonds (SPDR Blmbrg Barclays
                                        High Yield Bd ETF, JNK). Rising prices indicate investors are more willing to 
                                        take high risks.</li>
                                        <li>Gold Futures (Mini Gold Futures, YG=F). Rising prices indicate investors 
                                        seek low-risk assets.</li>
                                        <li>GoogleTrends query volumes for 'debt'. Increased numbers of searches for this term are associated with 
                                        subsequent drops in stock market prices (see <a href='https://doi.org/10.1038/srep01684' 
                                        target='_blank'>Preis et al., 2013</a>).</li>
                                        <li>U.S. Treasury Bond Futures (ZB=F). Rising prices indicate investors seek
                                        low-risk assets.</li>
                                      </ul>"),
                                      actionButton(inputId = "fetch_short",
                                                   label="Get data",
                                                   icon("arrow-circle-down",lib="font-awesome"))
                                      )),
                              fluidRow(
                                  column(width = 6,
                                         box(width = NULL, title = "Junk bonds",collapsible = T,collapsed = T,
                                             girafeOutput("jnk")),
                                         box(width = NULL, title = "Gold Futures",collapsible = T,collapsed = T,
                                             girafeOutput("gold"))),
                                  column(width = 6,
                                         box(width = NULL, title = "Google searches for 'debt'", collapsible = T,
                                             collapsed = T,
                                             girafeOutput("gtrends")),
                                         box(width = NULL, title = "US Treasury Futures", collapsible = T, collapsed = T,
                                             girafeOutput("zb")))
                              )
                              ),
                      tabItem(tabName = "forecast",
                              fluidRow(
                                box(width=12,solidheader=T,collapsible=F,title = "Dow Jones Forecast",
                                    column(width = 4,
                                           actionButton(inputId = "forecast", label = "Get data & estimate forecasts",
                                                 icon("arrow-circle-down",lib="font-awesome")),
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           materialSwitch(inputId = "showfit", label = "Show fitted values",
                                                          status = "warning")),
                                    column(width = 8,
                                           pickerInput(inputId = "model",
                                                       choices = list("Estimator" = c("Neural network" = "net",
                                                                                      "ARIMA" = "arima")),
                                                       multiple = F),
                                           sliderInput(inputId = "daterange", label = "Select start date",
                                                       min = as.Date("1985-01-01","%Y-%m-%d"),
                                                       max = as.Date(as.character(as.Date(Sys.Date()-365)),"%Y-%m-%d"),
                                                       value = as.Date(as.character(as.Date(Sys.Date()-365)),"%Y-%m-%d"),
                                                       timeFormat="%b %Y")
                                           )),
                                box(width = 12,collapsible = F,solidHeader = T,
                                    girafeOutput("foreplot")),
                                box(width=12,solidheader=F,title="Background info",collapsible=T,collapsed=T,
                                    HTML(
                                      "<p><strong>Important:</strong> New forecasts are being estimated
                                      every time this dashboard is loaded and based on the most recent DJIA data from
                                      Yahoo Finance. They can therefore vary over time.</p>
                                      <p>The two forecasting algorithms available here were selected out of a broader range of
                                      algorithms based on their performance in the following two tests:</p>
                                      <ol>
                                      <li>Pre-COVID predictive performance: Their predictive performance of real Dow Jones index values in the 
                                      period between July and December 2019, after being trained on Dow Jones data from
                                      January 1985 to June 2019;</li>
                                      <li>Post-COVID predictive performance: Their predictive performance of real
                                      Dow Jones index values in the period between June and August 2020 (after the 
                                      immediate post-lockdown shock), after being trained on Dow Jones data from
                                      January 1985 to May 2020.</li>
                                      </ol>
                                      <p>The following algorithms were under consideration:</p>
                                      <ul>
                                      <li>A flexible ARIMA estimator (the Hyndman-Khandakar algorithm, as
                                      implemented in the <i>auto.arima()</i> function in the
                                      <i>forecast</i>-package), which automatically selects an optimal
                                      ARIMA specification.</li>
                                      <li>Forecasts based on multiple seasonal decomposition (<i>mstl()</i>)
                                      and a random-walk with drift estimator.</li>
                                      <li>Exponential Smoothing State Space models selected using the 
                                      <i>ets()</i> algorithm in the <i>forecast</i>-package.</li>
                                      <li>Feed-forward neural network autoregression (NNAR) models, 
                                      autofitted using the <i>nnar()</i> algorithm.</li>
                                      <li>Feed-forward multi-layer perceptron (MLP) neural networks, autofitted
                                      using the <i>mlp()</i>-algorithm developed by Kourentzes, once with 
                                      differencing and once without.</li>
                                      </ul>
                                      <p>The flexible ARIMA estimator as well as the MLP neural network model without
                                      differencing performed overall best with mean absolute prediction errors (MEA) of
                                      1834.55 and 1731.74, respectively, for the pre-COVID era and MEAs of 1379.86 and
                                      1631.99, respectively, for the post-COVID era.</p>
                                      <p>Replication code for the analysis can be found <a 
                                      href='https://github.com/cknotz/finance_forecast' target='_blank'>here</a>.</p>
                                      "
                                    ))
                              ))
                  ))
)

server <- function(input, output, session) {
    tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"

    # Download & plot short-term indicator data
    observeEvent(input$fetch_short,{
        disable("fetch_short")
        showModal(modalDialog("Fetching data, please wait...", footer=NULL))  
        
        # Getting data from GoogleTrends
        gtrends <- gtrends(keyword = c("debt"), time="today 3-m", gprop="web")
            trends <- gtrends$interest_over_time
            rm(gtrends)
            trends <- pivot_wider(trends,names_from = keyword, values_from = hits) %>% 
                select(date,debt)
            trends$date <- as.Date(trends$date)
        
        # Getting data from Yahoo Finance
        first.date <- Sys.Date() - 90
            last.date <- Sys.Date()
            freq.data <- 'daily'
        
        tickers <- c('JNK') # Junk bonds
        
        jnkdata <- BatchGetSymbols(tickers = tickers,
                                 first.date = first.date,
                                 last.date = last.date,
                                 freq.data = freq.data,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache') ) # cache in tempdir())
        jnk <- jnkdata[["df.tickers"]] %>% 
            select(price.close,ref.date)
            rm(tickers,jnkdata)
            
        tickers <- c('YG=F') # Gold futures
        
        golddata <- BatchGetSymbols(tickers = tickers,
                                 first.date = first.date,
                                 last.date = last.date,
                                 freq.data = freq.data,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache') ) # cache in tempdir())
        gold <- golddata[["df.tickers"]] %>% 
            select(price.close,ref.date)
            rm(tickers,golddata)
            
        
        tickers <- c('ZB=F') # Treasury futures
        
        zbdata <- BatchGetSymbols(tickers = tickers,
                                 first.date = first.date,
                                 last.date = last.date,
                                 freq.data = freq.data,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache') ) # cache in tempdir())
        zb <- zbdata[["df.tickers"]] %>% 
            select(price.close,ref.date)
            rm(tickers,zbdata)        
        
        removeModal()
        
        # Drawing graphs
        
        output$gtrends <- renderGirafe({
           p <- ggplot(data = trends, aes(x=date,y=debt)) +
                    geom_line(color="white", size=.6, alpha=.6) +
                    geom_point_interactive(color="white",
                                           aes(tooltip=paste0("Date: ",date,"\n",
                          "Rel. search frequency: ",debt)),
                                           alpha=.4) +
                    stat_smooth(geom='line', se=F, color="#ff9900",alpha=.6, size=.8) +
                    xlab("") +
                    scale_x_date(date_labels = "%b-%Y") +
                    ylab("Web searches for 'debt' as proportion of all searches") +
                    theme_minimal(base_family = "sans") +
                        theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
                        theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
                        theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
                        theme(panel.grid.minor.x = element_blank()) +
                        theme(panel.grid.minor.y = element_blank()) +
                        theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
                        theme(axis.text = element_text(colour = "#d3d3d3", size = 12)) +
                        theme(axis.title = element_text(color = "#d3d3d3", size = 12)) +
                        theme(plot.caption = element_text(color="#d3d3d3")) +
                    labs(caption = "Source: GoogleTrends")
           
           girafe(ggobj = p,
                  fonts=list(sans = "Arial"),
                  options = list(
                      opts_selection(type = "none"),
                      opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                      opts_toolbar(saveaspng = FALSE)))
        })
        
        output$gold <- renderGirafe({
        p <- ggplot(data=gold,aes(x=ref.date,y=price.close)) +
                geom_line(color="white", size=.6, alpha=.6) +
                geom_point_interactive(color="white",
                                           aes(tooltip=paste0("Date: ",ref.date,"\n",
                          "Closing price: ",round(price.close,2))),
                                           alpha=.4) +
                stat_smooth(geom='line', se=F, color="#ff9900",alpha=.6, size=.8) +
                xlab("") +
                scale_x_date(date_labels = "%b-%Y") +
                ylab("Mini Gold Futures, closing price in USD") +
                theme_minimal(base_family = "sans") +
                    theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
                    theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.minor.x = element_blank()) +
                    theme(panel.grid.minor.y = element_blank()) +
                    theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
                    theme(axis.text = element_text(colour = "#d3d3d3", size = 12)) +
                    theme(axis.title = element_text(color = "#d3d3d3", size = 12)) +
                        theme(plot.caption = element_text(color="#d3d3d3")) +
                    labs(caption = "Source: Yahoo Finance")
        
        girafe(ggobj = p,
            fonts=list(sans = "Arial"),
            options = list(
                opts_selection(type = "none"),
                opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                opts_toolbar(saveaspng = FALSE)))
            
        })
        
        output$jnk <- renderGirafe({
        p <- ggplot(data=jnk, aes(x=ref.date,y=price.close)) +
                geom_line(color="white", size=.6, alpha=.6) +
                geom_point_interactive(color="white",
                                           aes(tooltip=paste0("Date: ",ref.date,"\n",
                          "Closing price: ",round(price.close,2))),
                                           alpha=.4) +
                stat_smooth(geom='line', se=F, color="#ff9900",alpha=.6, size=.8) +
                xlab("") +
                scale_x_date(date_labels = "%b-%Y") +
                ylab("High Yield Bond ETF, closing price in USD") +
                theme_minimal(base_family = "sans") +
                    theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
                    theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.minor.x = element_blank()) +
                    theme(panel.grid.minor.y = element_blank()) +
                    theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
                    theme(axis.text = element_text(colour = "#d3d3d3", size = 12)) +
                    theme(axis.title = element_text(color = "#d3d3d3", size = 12)) +
                        theme(plot.caption = element_text(color="#d3d3d3")) +
                    labs(caption = "Source: Yahoo Finance") 
        
        girafe(ggobj = p,
            fonts=list(sans = "Arial"),
            options = list(
                opts_selection(type = "none"),
                opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                opts_toolbar(saveaspng = FALSE)))
        })
        
        output$zb <- renderGirafe({
        p <- ggplot(data=zb, aes(x=ref.date,y=price.close)) +
                geom_line(color="white", size=.6, alpha=.6) +
                geom_point_interactive(color="white",
                                           aes(tooltip=paste0("Date: ",ref.date,"\n",
                          "Closing price: ",round(price.close,2))),
                                           alpha=.4) +
                stat_smooth(geom='line', se=F, color="#ff9900",alpha=.6, size=.8) +
                xlab("") +
                scale_x_date(date_labels = "%b-%Y") +
                ylab("US Treasury Bond Futures, closing price in USD") +
                theme_minimal(base_family = "sans") +
                    theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
                    theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
                    theme(panel.grid.minor.x = element_blank()) +
                    theme(panel.grid.minor.y = element_blank()) +
                    theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
                    theme(axis.text = element_text(colour = "#d3d3d3", size = 12)) +
                    theme(axis.title = element_text(color = "#d3d3d3", size = 12)) +
                        theme(plot.caption = element_text(color="#d3d3d3")) +
                    labs(caption = "Source: Yahoo Finance") 
        
        girafe(ggobj = p,
            fonts=list(sans = "Arial"),
            options = list(
                opts_selection(type = "none"),
                opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                opts_toolbar(saveaspng = FALSE)))    
        })
        
    })
    
    # Download DJIA data & estimate forecasts
    observeEvent(input$forecast, {
      disable("forecast")
      showModal(modalDialog("Fetching data, please wait...", footer=NULL))  
       
      # Getting data from Yahoo Finance
        first.date <- as.Date("1985-01-01")
            last.date <- Sys.Date()
            freq.data <- 'monthly'
        
        tickers <- c('^DJI') # Junk bonds
        
        dowdata <- BatchGetSymbols(tickers = tickers,
                                 first.date = first.date,
                                 last.date = last.date,
                                 freq.data = freq.data,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache') ) # cache in tempdir())
        dow <- dowdata[["df.tickers"]] %>% 
            select(price.close,ref.date)
            rm(dowdata,tickers,first.date,freq.data,last.date)
            
        tsdow <- ts(dow$price.close, frequency = 12, start = c(1985,1))
          rm(dow)
      removeModal()
      
      # Estimations
      showModal(modalDialog("Running estimations, almost there...", footer=NULL)) 
      
      arima <- auto.arima(tsdow, seasonal = T) # ARIMA
        summary(arima)
        arima %>% forecast(12) %>% 
          autoplot()
        
        arimafc <- arima %>% forecast(h=12)
        
      mlp <- mlp(tsdow, difforder=0)
        plot(forecast(mlp,h=12))
        
        mlpfc <- forecast(mlp, h=12)
       
      # Generate date variable  
      date <- data.frame(date = as.character(seq(from = as.Date("1985-01-01"), 
                                                 to = as.Date(as.character(Sys.Date()+365)), by = "month")))  
         
      # Extracting data - ARIMA model
      {
      arima_vals <- data.frame(vals=as.matrix(arimafc$model$x),
                               date=as.character(as.Date(time(arimafc$model$x))))
      arima_fitted <- data.frame(fitted=as.matrix(arimafc$fitted),
                                 date=as.character(as.Date(time(arimafc$fitted))))
      arima_fore <- data.frame(fore=as.matrix(arimafc$mean),
                               date=as.character(as.Date(time(arimafc$mean))))
      arima_upper <- data.frame(upper=as.matrix(arimafc$upper[,2]),
                                date=as.character(as.Date(time(arimafc$upper[,2]))))
      arima_lower <- data.frame(lower=as.matrix(arimafc$lower[,2]),
                                date=as.character(as.Date(time(arimafc$lower[,2]))))
        
      arima_data <- merge(date,arima_vals,by.x="date", all.x = T) 
      arima_data <- merge(arima_data,arima_fore, by="date", all.x = T)
      arima_data <- merge(arima_data,arima_fitted, by="date", all.x = T)
      arima_data <- merge(arima_data,arima_lower, by="date", all.x = T)
      arima_data <- merge(arima_data,arima_upper, by="date", all.x = T)
     
      arima_data$date <- as.Date(arima_data$date)
        rm(arima,arima_fitted,arima_fore,arima_lower,arima_upper,arima_vals,arimafc)
    }
      # Extracting data - MLP
      {
      mlp_vals <- data.frame(vals=as.matrix(mlpfc$x),
                             date=as.character(as.Date(time(mlpfc$x))))
      mlp_fitted <- data.frame(fitted=as.matrix(mlpfc$fitted),
                               date=as.character(as.Date(time(mlpfc$fitted))))
      mlp_fore <- data.frame(fore=as.matrix(mlpfc$mean),
                             date=as.character(as.Date(time(mlpfc$mean))))
      
      mlp_data <- merge(date,mlp_vals,by.x = "date", all.x = T)
      mlp_data <- merge(mlp_data,mlp_fitted, by.x = "date", all.x = T)
      mlp_data <- merge(mlp_data,mlp_fore, by.x = "date", all.x = T)
      
      mlp_data$date <- as.Date(mlp_data$date)
        rm(mlp,mlpfc,mlp_fitted,mlp_fore,mlp_vals,date)
      }
      
      removeModal()
      
      # Graphs
      output$foreplot <- renderGirafe({
        if(input$model=="arima"){
      p <- ggplot(arima_data[arima_data$date>=input$daterange,], aes(x=date)) + #as.Date("2017-08-01")
        geom_line(aes(y=vals), color="white", alpha=.6) +
        geom_point_interactive(aes(y=vals,
                                   tooltip=paste0("Points: ",round(vals,2),"\n",
                                                  "Date: ", format(as.Date(date), format="%b %Y"))),
                               fill="white", color="white", alpha=.8) +
        geom_ribbon(aes(y=fore,ymin=lower,ymax=upper), alpha=.3, fill="white") +
        geom_line(aes(y=fore), color="#ff9900", size=1.5, alpha=.6) +
        geom_point_interactive(aes(y=fore,
                                   tooltip=paste0("Points: ",round(fore,2),"\n",
                                                  "Date: ", format(as.Date(date), format="%b %Y"))),
                   fill="#ff9900", color="#ff9900", alpha=.8) +
          {if(input$showfit==T) geom_line(aes(y=fitted), color="#ff9900")} +
        xlab("") +
        ylab("Dow Jones Industrial Average (points)") +
        theme_minimal(base_family = "sans") +
          theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
          theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.minor.x = element_blank()) +
          theme(panel.grid.minor.y = element_blank()) +
          theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
          theme(axis.text = element_text(colour = "#d3d3d3", size = 9)) +
          theme(axis.title = element_text(color = "#d3d3d3", size = 9)) +
          theme(plot.caption = element_text(color="#d3d3d3")) +
            labs(caption = "Data source: Yahoo Finance; 95% prediction intervals")
      
      girafe(ggobj = p, width_svg = 7, height_svg = 4,
                  fonts=list(sans = "Arial"),
                  options = list(
                      opts_selection(type = "none"),
                      opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                      opts_toolbar(saveaspng = FALSE)))
        }else{
          p <- ggplot(data = mlp_data[mlp_data$date>=input$daterange,],aes(x=date)) +
    geom_line(aes(y=vals), color="white", alpha=.6) +
    geom_point_interactive(aes(y=vals,
                                   tooltip=paste0("Points: ",round(vals,2),"\n",
                                                  "Date: ", format(as.Date(date), format="%b %Y"))),
                               fill="white", color="white", alpha=.8) +
    geom_line(aes(y=fore), color="#ff9900", size=1.5, alpha=.6) +
    geom_point_interactive(aes(y=fore,
                                   tooltip=paste0("Points: ",round(fore,2),"\n",
                                                  "Date: ", format(as.Date(date), format="%b %Y"))),
                   fill="#ff9900", color="#ff9900", alpha=.8) +
         {if(input$showfit==T) geom_line(aes(y=fitted), color="#ff9900")} +
    xlab("") +
        ylab("Dow Jones Industrial Average (points)") +
        theme_minimal(base_family = "sans") +
          theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
          theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.minor.x = element_blank()) +
          theme(panel.grid.minor.y = element_blank()) +
          theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
          theme(axis.text = element_text(colour = "#d3d3d3", size = 9)) +
          theme(axis.title = element_text(color = "#d3d3d3", size = 9)) +
          theme(plot.caption = element_text(color="#d3d3d3")) +
            labs(caption = "Data source: Yahoo Finance")
     
    girafe(ggobj = p, width_svg = 7, height_svg = 4,
                  fonts=list(sans = "Arial"),
                  options = list(
                      opts_selection(type = "none"),
                      opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                      opts_toolbar(saveaspng = FALSE)))
        }
      
      })
    })
    
}

shinyApp(ui, server)