
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
    #library(forecast)
    library(gtrendsR)
   # library(seasonal)
    #library(nnfor)
    library(gridExtra)
    library(ggiraph)
    library(shinyWidgets)
    library(shinyjs)
    library(httr)
    library(jsonlite)

# Alpha Vantage key
key <- readLines("www/key.txt") # key.txt is not on GitHub; replace by own key if you want to use this app with your own key
agent <- httr::user_agent("https://github.com/cknotz")

# Error-proof Function to load Yahoo data
loadyahoo <- function(symbol,start,end,freq,thres){
          out <- tryCatch(
            {data <- BatchGetSymbols(tickers = symbol,
                                 first.date = start,
                                 last.date = end,
                                 freq.data = freq,
                                 thresh.bad.data = thres,
                                 cache.folder = file.path(tempdir(),
                                                          'BGS_Cache'))
              return(data)
            },
            error = function(cond){
              return("There's been an error. Either the ticker symbol is not correct or the time-series for this symbol might be too short.")
            },
            warning = function(cond){ # getsymbols() posts errors as warnings
              return("There's been an error. Either the ticker symbol is not correct or the time-series for this symbol might be too short.")
            },
            finally = {
              message("Done here")
            }
          )
          return(out)
}

ui <- dashboardPage(
    dashboardHeader(title = "Stock Market Data Dashboard", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(id = "sidebar",
      menuItem("Introduction", tabName = "intro", icon = icon("info", lib = "font-awesome")),
      menuItem("Sentiment indicators", tabName = "indicators", icon = icon("dashboard")),
      menuItem("Stock prices", tabName = "data", icon = icon("chart-line", lib = "font-awesome"))
      # HTML("<br><br><br>"),
      # menuItemOutput("fetch_ind")
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
                                      "<p>This dashboard features data on current stock market developments, including
                                      a selection of short-term indicators of investor sentiment and prices of
                                      selected stocks and ETFs.</p>
                                      
                                      <p>The short-term indicators are selected based (roughly) on this helpful Medium 
                                      <a href='https://medium.com/concoda/how-to-predict-major-market-shifts-99419ae1d23c'
                                       target='_blank'>post</a> and the <i>Nature</i> article by Preis et al. 
                                      (<a href='https://doi.org/10.1038/srep01684' target='_blank'>2013</a>). The data
                                      on short-term indicators are retrieved from Yahoo Finance and GoogleTrends.</p>
                                      
                                      <p>The stock price data are retrieved through the <a target='_blank'
                                      href='https://www.alphavantage.co/'>Alpha Vantage API</a> and Yahoo Finance.</p>
                                      
                                      <p>This is version 1.0. </p>"
                                      # <p>The forecast function for the stock market prices assumes the data follow
                                      # a simple random walk with drift process. Many (but not all!) economists think
                                      # this is a reasonable approximation of how stock market prices evolve.</p>
                                      ))
                              )),
                      tabItem(tabName = "indicators",
                              fluidRow(
                                  box(width=12,solidHeader = T,collapsible = T,collapsed = F,title = "Info",
                                      HTML(
                                      "<p>The interactive graphs below show the recent (last 90 days) development of some selected indicators of
                                        investor sentiment. These include:</p>
                                      <ul>
                                        <li>High-yield or 'junk' bond prices (SPDR Blmbrg Barclays
                                        High Yield Bd ETF, JNK). Rising prices indicate investors are more willing to 
                                        take risks.</li>
                                        <li>Gold Futures (Mini Gold Futures, YG=F). Rising prices indicate investors 
                                        seek low-risk assets.</li>
                                        <li>Worldwide GoogleTrends query volumes for 'debt'. Increased numbers of searches for this term are associated with 
                                        subsequent drops in stock market prices (see <a href='https://doi.org/10.1038/srep01684' 
                                        target='_blank'>Preis et al., 2013</a>).</li>
                                        <li>U.S. Treasury Bond Futures (ZB=F). Rising prices indicate investors seek
                                        low-risk assets.</li>
                                      </ul>"),
                                      HTML("<br>"),
                                        actionBttn(inputId = "fetch_inds",
                                                   label = "Get data",
                                                   style = "minimal",
                                                   color = "warning",
                                                   size = "sm")
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
                      tabItem(tabName = "data",
                              fluidRow(
                                box(width = 12,solidHeader = T,collapsible = F,
                                    title = NULL,
                                    column(12,
                                    searchInput(
                                      inputId = "search",
                                      label = "Search for ticker symbols/companies",
                                      placeholder = "Enter company name or ticker symbol...",
                                      btnSearch = icon("search"), 
                                      btnReset = icon("remove"), 
                                      width = "80%"),
                                    tableOutput(outputId = "search_res"))
                                    ),
                                box(width=12,solidHeader = T,collapsible = F, title = NULL,
                                    column(8,
                                           # pickerInput(inputId = "picker",
                                           #             choices = c("Alphabet Cl. A (GOOGL)" = "GOOGL",
                                           #                         "Amazon (AMZN)" = "AMZN",
                                           #                         "Apple (AAPL)" = "AAPL",
                                           #                         "Microsoft (MSFT" = "MSFT"))),
                                           searchInput(
                                              inputId = "getstocks",
                                              label = "Enter ticker symbol",
                                              placeholder = "Reset before requesting same ticker twice",
                                              btnSearch = icon("search"), 
                                              btnReset = icon("remove"), 
                                              width = "80%")),
                                    column(4,
                                           radioGroupButtons(inputId = "datasource",
                                                             label = "Select data source",
                                                             choices = c("Alpha Vantage",
                                                                         "Yahoo Finance"),
                                                             status = "warning",
                                                             checkIcon = list(
                                                               yes = icon("ok",lib = "glyphicon")),
                                                             size = "xs")
                                           ),
                                    column(width = 6, align = "center",
                                           awesomeCheckbox(inputId = "ma50",
                                                           label = "Show 50 day moving-average",
                                                           value = T,
                                                           status = "warning")),
                                    column(width = 6, align = "center",
                                           awesomeCheckbox(inputId = "ma200",
                                                           label = "Show 200 day moving-average",
                                                           value = T,
                                                           status = "info")),
                                    column(width=12, align = "center",
                                           girafeOutput("stocksplot"),
                                           HTML("<br>")
                                           ),
                                    column(width=12,
                                           sliderInput(inputId = "daterange", label = "Adjust start date",
                                                       min = as.Date("2015-01-01","%Y-%m-%d"),
                                                       max = as.Date(as.character(as.Date(Sys.Date()-180)),"%Y-%m-%d"),
                                                       value = as.Date(as.character(as.Date(Sys.Date()-3*365)),"%Y-%m-%d"),
                                                       timeFormat="%b %Y"))
                                    )
                              )
                              )))
)


slider_date <- as.Date("2015-01-01","%Y-%m-%d")


server <- function(input, output, session) {
    tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"
    
    # Dynamic sidebar; object does not remove properly with ShinyWidgets
    # observeEvent(input$sidebar,{
    #   if(input$sidebar=="indicators"){
    #   output$fetch_ind <- renderMenu({
    #     actionBttn(inputId = "fetch_inds",
    #                label = "Get data",
    #                style = "stretch",
    #                color = "warning",
    #                size = "sm")
    #   })
    #   }else{
    #     output$fetch_ind <- NULL
    #   }
    #   
    # })
    
    # Search function
    observeEvent(input$search,{

      output$search_res <- renderTable({
      #input$search
      
      if(input$search!=""){
      
      term <- gsub(" ","",input$search,fixed = T)
        
      # Set up API request
      base <- "https://www.alphavantage.co/query?"
      datatype <- "json"
      searchcall <- paste0(base,
                     "function=","SYMBOL_SEARCH",
                     "&keywords=",term, # 
                     "&datatype=",datatype,
                     "&apikey=",key)
      

      # Fetch data - JSON
      found <- GET(searchcall) %>%
        httr::content(as = "text",
                      encoding = "UTF-8") %>%
        fromJSON(flatten = T)
      
      
      
      if(length(found[[1]])==0){
         error <- data.frame(x = c(" "),
                                y = ("Looks like nothing could be found. Please try another search term."))
         names(error) <- NULL
         error
      } else {
      # Extract data
      found <- found[[1]][,c(2,1)]
      names(found) <- NULL
      found
      }
      
      }
      })
    })
    
    # Download & plot short-term indicator data
    observeEvent(input$fetch_inds,{
        disable("fetch_inds")
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
                    labs(caption = "Source: GoogleTrends; orange line indicates LOESS smoother.")
           
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
                    labs(caption = "Source: Yahoo Finance; orange line indicates LOESS smoother.")
        
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
                    labs(caption = "Source: Yahoo Finance; orange line indicates LOESS smoother.") 
        
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
                    labs(caption = "Source: Yahoo Finance; orange line indicates LOESS smoother.") 
        
        girafe(ggobj = p,
            fonts=list(sans = "Arial"),
            options = list(
                opts_selection(type = "none"),
                opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                opts_toolbar(saveaspng = FALSE)))    
        })
        
    })
    
    # Download stock market price data from Alpha Vantage API
    observeEvent(input$getstocks,{
      if(input$datasource=="Alpha Vantage"){
      if(input$getstocks==""){
      print(input$getstocks)  
      } else {
      showModal(modalDialog("Fetching data, please wait...", footer=NULL))  
      
      # Construct call
      base <- "https://www.alphavantage.co/query?"
      datatype <- "json"
      size <- "full"
      call <- paste0(base,
             "function=","TIME_SERIES_DAILY",
             "&symbol=",input$getstocks, #   "DTEGF"     
             "&outputsize=",size,
             "&datatype=",datatype,
             "&apikey=",key)
     
      # Fetch data - CSV
      data <- httr::GET(call, agent) %>%
          httr::content(as = "text",encoding = "UTF-8") %>%
          fromJSON(flatten = T)
      
      if(length(data[[1]])==1){
        showNotification(paste0("Looks like something went wrong - possibly a misspelled ticker symbol? (Also, Alpha Vantage asks me to relay this to you: '",data[[1]],"')"),
                         type = "error"
        )
        removeModal()
      } else {
        
      # Store meta info
      info <- data[[1]][[1]]
      tz <- data[[1]][[5]]
      symbol <- data[[1]][[2]]
      
      # Tidy data
      inter <- as.data.frame(unlist(data[[2]]))

      inter$id <- rownames(inter)
      data <- separate(data = inter,
               col = id,
               into = c("date","var"),
               sep = ".[[:digit:]].[[:blank:]]",
               remove = T
               )
      data$value <- as.double(data[,1])
      data$date <- as.Date(data$date)
      data[,1] <- NULL
      
      # Reshape to wider
      data <- data %>%
          pivot_wider(
              names_from = var,
              values_from = value
          )
      
      #Addign meta info
      data$symbol <- symbol
      data$tz <- tz
      rm(inter)
  
    # Limit to last 5 years (remove once AV corrected older data)
    data <- data[data$date>=Sys.Date() - 5*365,]
      
    # Calculating moving averages
    data <- data[order(data$date),] # reverse order
    data$ma50 <- stats::filter(data$close,rep(1/50,50),sides=1)  
    data$ma200 <- stats::filter(data$close,rep(1/200,200),sides=1)  
    data <- data[rev(order(data$date)),]  # re-reverse order
    
     removeModal()
     
     # Graph
     output$stocksplot <- renderGirafe({
     p <- ggplot(data=data, aes(x=date,y=close)) +
      geom_line(color="white", alpha=.6) +
      geom_point_interactive(fill="white", color="white", alpha=.8,
                             aes(tooltip=paste0("Date: ",date,"\n","Closing price: ",round(close,2)))) +
          {if(input$ma50==T) geom_line(aes(y=ma50), color="#ff9900", alpha=.6, size=1.5)} +
          {if(input$ma200==T) geom_line(aes(y=ma200), color="#46acd1", alpha=.6, size=1.5)} +
       xlab("") +
       ylab("Closing price") +
          theme_minimal(base_family = "sans") +
          theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
          theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
          theme(panel.grid.minor.x = element_blank()) +
          theme(panel.grid.minor.y = element_blank()) +
          theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
          theme(axis.text = element_text(colour = "#d3d3d3", size = 9)) +
          theme(axis.title = element_text(color = "#d3d3d3", size = 9)) +
          theme(plot.caption = element_text(color="#d3d3d3"))
     
     girafe(ggobj = p, # width_svg = 7, height_svg = 4,
                  fonts=list(sans = "Arial"),
                  options = list(
                      opts_selection(type = "none"),
                      opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                      opts_toolbar(saveaspng = FALSE)))
     
     })
     
      } # closes second 'else' condition
      } # closes first 'else' condition (input$getstocks!="")
      } else { # moves to Yahoo finance
        if(input$getstocks==""){
      print(input$getstocks)  
      } else {
       
        # Downloading data
        showModal(modalDialog("Fetching data, please wait...", footer=NULL))  
        
        data <- loadyahoo(symbol = input$getstocks,
                  start = Sys.Date()-5*365,
                  end = Sys.Date(),
                  freq = "daily",
                  thres = 0.5)
        
        if(is.character(data)){
          showNotification(paste0(data))
          removeModal()
        }else{
        data <- data[["df.tickers"]] %>% 
            select(price.close,ref.date)
        
        # Calculating moving averages
        data$ma50 <- stats::filter(data$price.close,rep(1/50,50),sides=1)  
        data$ma200 <- stats::filter(data$price.close,rep(1/200,200),sides=1)  
            
        removeModal()
        
        output$stocksplot <- renderGirafe({    
        p <- ggplot(data, aes(x=ref.date,y=price.close)) +
          geom_line(color="white", alpha=.6) +
          geom_point_interactive(fill="white", color="white", alpha=.8,
                             aes(tooltip=paste0("Date: ",ref.date,"\n","Closing price: ",round(price.close,2)))) +
          {if(input$ma50==T) geom_line(aes(y=ma50), color="#ff9900", alpha=.6, size=1.5)} +
          {if(input$ma200==T) geom_line(aes(y=ma200), color="#46acd1", alpha=.6, size=1.5)} +
          xlab("") +
          ylab("Closing price") +
            theme_minimal(base_family = "sans") +
            theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) + 
            theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
            theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
            theme(axis.text = element_text(colour = "#d3d3d3", size = 9)) +
            theme(axis.title = element_text(color = "#d3d3d3", size = 9)) +
            theme(plot.caption = element_text(color="#d3d3d3"))
     
     girafe(ggobj = p, # width_svg = 7, height_svg = 4,
                  fonts=list(sans = "Arial"),
                  options = list(
                      opts_selection(type = "none"),
                      opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
                      opts_toolbar(saveaspng = FALSE)))
        })
      }}}
      })
    
    # rwf(y, h, drift=TRUE)
}

shinyApp(ui, server)