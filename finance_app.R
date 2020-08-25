
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
    library(httr)
    library(jsonlite)

# Alpha Vantage functions
#########################

# Set up API request
base <- "https://www.alphavantage.co/query?"
type <- "TIME_SERIES_DAILY"
datatype <- "json"
size <- "full"
key <- readLines("www/key.txt") # key.txt is not on GitHub; replace by own key if you want to use this app yourself

# Function to download
download_av <- function(symbol){
# Construct call
call <- paste0(base,
       "function=",type,
       "&symbol=",symbol,
       "&outputsize=",size,
       "&datatype=",datatype,
       "&apikey=",key)

# Fetch data - JSON
data <- GET(call) %>% 
    httr::content(as = "text",encoding = "UTF-8") %>% 
    fromJSON(flatten = T)
return(data)
}

# Function to check data and transform to tidy data frame
munging_av <- function(x){
#Check if call successful  
if(length(x[[1]])==1) {
  showNotification(paste0("Something seems to be wrong. Alpha Vantage says: '",x[[1]],"'"),
                   type = "error")
  print(paste0("Something's wrong. Alpha Vantage says :",x[[1]]),"'")
} else { # Munge data
  
# Store meta info
info <- x[[1]][[1]]
tz <- x[[1]][[5]]
symbol <- x[[1]][[2]]

# Tidy
y <- as.data.frame(unlist(x[[2]]))

# Tidy
y$id <- rownames(y)
x <- separate(data = y,
         col = id,
         into = c("date","var"),
         sep = ".[[:digit:]].[[:blank:]]",
         remove = T
         )
x$value <- as.double(x[,1])
x$date <- as.Date(x$date)
x[,1] <- NULL

# Reshape to wider
x <- x %>% 
    pivot_wider(
        names_from = var,
        values_from = value
    )

#Addign meta info
x$symbol <- symbol
x$tz <- tz

return(x)
message("Data are ready.")
}
}


ui <- dashboardPage(
    dashboardHeader(title = "Stock Market Data Dashboard", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info", lib = "font-awesome")),
      menuItem("Sentiment indicators", tabName = "indicators", icon = icon("dashboard")),
      menuItem("Stock prices", tabName = "data", icon = icon("chart-line", lib = "font-awesome")),
      HTML("<br><br>"),
      #textInput(inputId = "key", placeholder = "Enter your API key" ,label = NULL),
      #actionButton(inputId = "submit", label = "Store key", width = 200),
      #HTML("<br>"),
      actionButton(inputId = "fetch",
                   label="Download data",
                   icon("arrow-circle-down",lib="font-awesome"), width = 200)
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
                                      href='https://www.alphavantage.co/'>Alpha Vantage API</a>.</p>
                                      
                                      <p>The forecast function for the stock market prices assumes the data follow
                                      a simple random walk with drift process. Many (but not all!) economists think
                                      this is a reasonable approximation of how stock market prices evolve.</p>
                                      
                                      <p>This is version 1.0. Future versions will feature an option to dynamically search and
                                      download data for all stocks Alpha Vantage has data for.</p>"
                                      ))
                              )),
                      tabItem(tabName = "indicators",
                              fluidRow(
                                  box(width=12,solidHeader = T,collapsible = T,collapsed = F,title = "Info",
                                      HTML(
                                      "<p>The interactive graphs below show the recent (last 90 days) development of some selected indicators of
                                        investor sentiment. These include:</p>
                                      <ul>
                                        <li>High-yield or 'junk' bond (SPDR Blmbrg Barclays
                                        High Yield Bd ETF, JNK) prices. Rising prices indicate investors are more willing to 
                                        take high risks.</li>
                                        <li>Gold Futures (Mini Gold Futures, YG=F). Rising prices indicate investors 
                                        seek low-risk assets.</li>
                                        <li>Worldwide GoogleTrends query volumes for 'debt'. Increased numbers of searches for this term are associated with 
                                        subsequent drops in stock market prices (see <a href='https://doi.org/10.1038/srep01684' 
                                        target='_blank'>Preis et al., 2013</a>).</li>
                                        <li>U.S. Treasury Bond Futures (ZB=F). Rising prices indicate investors seek
                                        low-risk assets.</li>
                                      </ul>")
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
                                box(width=12,solidHeader = T,collapsible = F, title = " ",
                                    column(width=8,
                                           pickerInput(inputId = "picker",
                                                       choices = c("Alphabet Cl. A (GOOGL)" = "GOOGL",
                                                                   "Amazon (AMZN)" = "AMZN",
                                                                   "Apple (AAPL)" = "AAPL",
                                                                   "Microsoft (MSFT" = "MSFT"))),
                                    column(width=4, align = "center",
                                           actionButton(inputId = "forecast",label = "Forecast",
                                                 icon("arrow-circle-right",lib="font-awesome"))),
                                    column(width = 6, align = "center",
                                           awesomeCheckbox(inputId = "50days",
                                                           label = "Show 50 day moving-average",
                                                           value = F,
                                                           status = "warning")),
                                    column(width = 6, align = "center",
                                           awesomeCheckbox(inputId = "200days",
                                                           label = "Show 200 day moving-average",
                                                           value = F,
                                                           status = "warning")),
                                    column(width=12,
                                           sliderInput(inputId = "daterange", label = "Adjust start date",
                                                       min = as.Date("2015-01-01","%Y-%m-%d"),
                                                       max = as.Date(as.character(as.Date(Sys.Date()-365)),"%Y-%m-%d"),
                                                       value = as.Date(as.character(as.Date(Sys.Date()-3*365)),"%Y-%m-%d"),
                                                       timeFormat="%b %Y"))
                                    )
                              )
                              )))
)

server <- function(input, output, session) {
    tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"
    # Download & plot short-term indicator data
    observeEvent(input$fetch,{
        disable("fetch")
        showModal(modalDialog("Fetching data, please wait...", footer=NULL))  
        
      # Getting data from Alpha Vantage
      aapl <- download_av("AAPL")
      aapl <- munging_av(aapl)
      
      googl <- download_av("GOOGL")
      googl <- munging_av(googl)
      
      amzn <- download_av("AMZN")
      amzn <- munging_av(amzn)
      
      msft <- download_av("MSFT")
      msft <- munging_av(msft)
      
      
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
    
    
    # rwf(y, h, drift=TRUE)
}

shinyApp(ui, server)