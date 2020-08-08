
# Forecasting ^DJIA - Model Evaluation
######################################

library(ggplot2)
    library(tidyverse)
    library(BatchGetSymbols)
    library(tidyquant)
    library(forecast)
    library(gtrendsR)
    library(seasonal)
    library(nnfor)

# Setting up results table
##########################
results <- data.frame(Model = character(),
                         MAE_1 = double())


# Loading stock market data
###########################

# set dates
first.date <- as.Date("1985-01-01")
    last.date <- as.Date("2020-08-07")
    freq.data <- 'monthly'

# Dow Jones
tickers <- c('^DJI')

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') ) # cache in tempdir())

# Check
print(l.out$df.control)

# Plotting
ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close)) +
    geom_line() +
    #geom_candlestick(aes(open=price.open, high=price.high,low=price.low,close=price.close)) +
    facet_wrap(~ticker, scales = 'free_y') +
    ylab("Daily closing price") + xlab("Date")

dow <- l.out[["df.tickers"]]$price.close

tsdow <- ts(dow, frequency = 12, start = c(1985,1))
    rm(l.out,tickers,dow)

plot(tsdow)

# Checking for seasonality & autocorrelation
############################################

ggseasonplot(tsdow, polar = T) # no seasonality apparent

gglagplot(tsdow, set.lags = c(1,2,3,4)) # ?

ggAcf(tsdow) # looks integrated/unit root
ggAcf(tsdow, type = "partial")


# Split into train and test data
################################

dow_train <- window(tsdow, end=c(2019,6))
    dow_eval <- window(tsdow, start=c(2019,7), end=c(2019,12))
    dow_test <- window(tsdow, start=c(2020,1))

# Running models
################

# Forecast using STL seasonal decomposition
fit <- mstl(dow_train)
fit  %>% forecast(method="rwdrift", h=6) %>%
    autoplot() +
    autolayer(dow_eval)

fc <- forecast(fit, method = "rwdrift", h=6)
    pred1 <- fc$mean
    mean(abs(dow_eval-pred1))

    results <- rbind(results,c("STL seas. decomp; rwdrift",mean(abs(dow_eval-pred1))))
    results <- results %>% rename(
        Model = X.STL.seas..decomp..rwdrift.,
        MAE = X.1827.33463160102.
    )

# Forecast using ETS
ets <- ets(dow_train)
    ets %>% forecast(6) %>%
        autoplot() +
        autolayer(dow_eval) # strange

etsfc <- forecast(ets,h=6)
    pred2 <- etsfc$mean
    mean(abs(dow_eval-pred2))
    results <- rbind(results,c(etsfc$method,mean(abs(dow_eval-pred2))))


# Forecast using ARIMA (with seasonality)
arima <- auto.arima(dow_train, seasonal = T)
    summary(arima) # random walk with drift (makes sense)
    arima %>% forecast(6) %>%
        autoplot() +
        autolayer(dow_eval)

arimafc <- forecast(arima,h=6)
    pred3 <- arimafc$mean
    mean(abs(dow_eval - pred3))
    results <- rbind(results,c(arimafc$method,mean(abs(dow_eval - pred3))))


# Neural network (no diff.)
mlp <- mlp(dow_train,outplot=T, difforder=0)
    plot(mlp)
    print(mlp)
    plot(forecast(mlp,h=6))

mlpfc <- forecast(mlp,6)
    pred4 <- mlpfc$mean
    mean(abs(dow_eval - pred4))
    results <- rbind(results,c("MLP, no diff.",mean(abs(dow_eval - pred4))))

# Neural network (diff.)
mlp2 <- mlp(dow_train,outplot=T)
    plot(mlp2)
    print(mlp2)
    plot(forecast(mlp2,h=6))

mlpfc2 <- forecast(mlp2,6)
    pred5 <- mlpfc2$mean
    mean(abs(dow_eval - pred5))
    results <- rbind(results,c("MLP, diff.",mean(abs(dow_eval - pred5))))

# Neural net, nnetar
nnet <- nnetar(dow_train, lambda = "auto")
    autoplot(forecast(nnet, h=6, PI=T)) +
        autolayer(dow_eval)

nnetfc <- forecast(nnet, h=6)
    pred6 <- nnetfc$mean
    mean(abs(dow_eval - pred6))
    results <- rbind(results,c(nnet$method,mean(abs(dow_eval - pred6))))

# Re-test against COVID-period
##############################

dow_train <- window(tsdow, end=c(2020,05))
    dow_test <- window(tsdow, start=c(2020,06))

# STL seasonal decomposition
fit <- mstl(dow_train)
fit  %>% forecast(method="rwdrift", h=3) %>%
    autoplot() +
    autolayer(dow_test)

fc <- forecast(fit, method = "rwdrift", h=3)
    pred1 <- fc$mean
    MAE_COV <- mean(abs(dow_test-pred1))


# Forecast using ETS
ets_cov <- ets(y=dow_train)
    ets_cov %>% forecast(3) %>%
        autoplot() +
        autolayer(dow_test)

ets_cov_fc <- forecast(ets_cov,h=3)
    pred2 <- ets_cov_fc$mean
    MAE_COV <- c(MAE_COV,mean(abs(dow_test-pred2)))


# Forecast using ARIMA (with seasonality)
arima_cov <- auto.arima(dow_train, seasonal = T)
    arima_cov %>% forecast(3) %>%
        autoplot() +
        autolayer(dow_test)

arima_cov_fc <- forecast(arima_cov,h=3)
    pred3 <- arima_cov_fc$mean
    mean(abs(dow_test - pred3))
    MAE_COV <- c(MAE_COV,mean(abs(dow_test - pred3)))


# Neural network (no diff.)
mlp_cov <- mlp(dow_train,outplot=T, difforder=0)
    plot(mlp_cov)
    print(mlp_cov)
    plot(forecast(mlp_cov,h=3))

mlp_cov_fc <- forecast(mlp_cov,3)
    pred4 <- mlp_cov_fc$mean
    mean(abs(dow_test - pred4))

    MAE_COV <- c(MAE_COV,mean(abs(dow_test - pred4)))

 # Neural netword, (diff.)
mlp2_cov <- mlp(y=dow_train,outplot=T)
    plot(mlp2_cov)
    plot(forecast(mlp2_cov, h=3))

mlp2_cov_fc <- forecast(mlp2_cov,h=3)
    pred5 <- mlp2_cov_fc$mean
    mean(abs(dow_test - pred5))

    MAE_COV <- c(MAE_COV,mean(abs(dow_test - pred5)))


# Neural net, nnetar
nnet_cov <- nnetar(y=dow_train, lambda = "auto")
    autoplot(forecast(nnet_cov,h=3, PI=T)) +
        autolayer(dow_test)

nnet_cov_fc <- forecast(nnet_cov, h=3)
    pred6 <- nnet_cov_fc$mean
    mean(abs(dow_test - pred6))
    MAE_COV <- c(MAE_COV,mean(abs(dow_test - pred6)))


# Adding new results
results <- cbind(results,MAE_COV)
    results

results2 <- results %>% gather(key=var,value = value,MAE,MAE_COV)
    results2$value <- as.numeric(results2$value)

ggplot(results2,aes(x=reorder(Model,value),
                    y=value,fill=var)) +
    geom_bar(stat = 'identity', position = 'dodge',alpha=.75) +
    ylab("Mean abslute error") +
    xlab("") +
    scale_fill_manual(breaks=c("MAE","MAE_COV"),
                      labels=c("Pre-COVID","Post-COVID"),
                      values = c("#ff9900", "white")) +
    theme_minimal(base_family = "sans") +
            theme(panel.background = element_rect(fill = "#343e48",color = "#d3d3d3")) +
            theme(panel.grid.major.y = element_line(color="#d3d3d3", size = .1)) +
            theme(panel.grid.major.x = element_line(color="#d3d3d3", size = .1)) +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.minor.y = element_blank()) +
            theme(plot.background = element_rect(fill="#343e48", color = "#343e48")) +
            theme(axis.text = element_text(colour = "#d3d3d3", size = 10)) +
            theme(axis.title = element_text(color = "#d3d3d3", size = 10)) +
            theme(legend.position = "bottom") +
            theme(legend.title = element_blank()) +
            theme(legend.text = element_text(color = "#d3d3d3")) +
            theme(axis.text.x = element_text(angle = 25,hjust = 1))

# ggsave("forecast_eval.png")
