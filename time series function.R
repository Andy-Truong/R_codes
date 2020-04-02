library(tidyverse)
library(forecast)

#The following functions are used to identify the time series method that gives the lowest test MSE

##For Number of Work Orders by Craft Group

COUNT_TS_MSE = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(count = n()) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$count, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)

  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$count[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.SESmodel <- HoltWinters(ts.training, beta=FALSE, gamma=FALSE)
  ts.training.DESmodel <- HoltWinters(ts.training, gamma=FALSE)
  ts.training.HWmodel <- HoltWinters(ts.training)
  
  ts.SESmodel <- HoltWinters(ts,
                                       alpha=ts.training.SESmodel$alpha, 
                                       beta=FALSE, 
                                       gamma=FALSE)
  ts.DESmodel <- HoltWinters(ts, 
                                       alpha=ts.training.DESmodel$alpha, 
                                       beta=ts.training.DESmodel$beta, 
                                       gamma=FALSE)
  ts.HWmodel <- HoltWinters(ts, 
                                      alpha=ts.training.HWmodel$alpha, 
                                      beta=ts.training.HWmodel$beta, 
                                      gamma=ts.training.HWmodel$gamma)
  
  data.start <- train.periods + 1
  data.end <- train.periods + test.periods
  
  fit.start <- train.periods 
  fit.end <- train.periods + test.periods - 1
  
  SES.MSE <- sum((ts.SESmodel$fitted[fit.start:fit.end] - ts[data.start:data.end])^2)/(test.periods)
  
  fit.start <- train.periods - 1 
  fit.end <- train.periods + test.periods - 2
  
  DES.MSE <- sum((ts.DESmodel$fitted[fit.start:fit.end] - ts[data.start:data.end])^2)/(test.periods)
  
  fit.start <- train.periods - cycle + 1 
  fit.end <- train.periods + test.periods - cycle
  
  HW.MSE <- sum((ts.HWmodel$fitted[fit.start:fit.end] - ts[fit.start:fit.end])^2)/(test.periods)
  
  MSE = data.frame(SES.MSE, DES.MSE, HW.MSE)
  
  print(MSE)
  print(paste("The minimum MSE is", 
              names(MSE)[which.min(MSE)],
              "=",
              round(MSE[which.min(MSE)]))) 
  
}

### SES function for number of crafts

SES.prediction = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(count = n()) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$count, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$count[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.SESmodel <- HoltWinters(ts.training, beta=FALSE, gamma=FALSE)
  
  ts.SESmodel <- HoltWinters(ts,
                             alpha=ts.training.SESmodel$alpha, 
                             beta=FALSE, 
                             gamma=FALSE)
  
  forec = forecast(ts.SESmodel, 18)
  SES.plot = plot(ts.SESmodel)
  forecast.plot = plot(forec, main = "Forecast for 2019-2020")
  list(SES.plot, forecast.plot)
  
}

###DES function for number of craft

DES.prediction = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(count = n()) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$count, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$count[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.DESmodel <- HoltWinters(ts.training, gamma=FALSE)
  
  ts.DESmodel <- HoltWinters(ts,
                             alpha=ts.training.DESmodel$alpha, 
                             beta=ts.training.DESmodel$beta, 
                             gamma=FALSE)
  
  forec = forecast(ts.DESmodel, 18)
  DES.plot = plot(ts.DESmodel)
  forecast.plot = plot(forec, main = "Double Exponential Smoothing Forecast for 2019-2020")
  forecast.plot
  
}

### HW function for number of crafts

HW.prediction = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(count = n()) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$count, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$count[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.HWmodel <- HoltWinters(ts.training)
  
  ts.HWmodel <- HoltWinters(ts,
                            alpha=ts.training.HWmodel$alpha, 
                            beta=ts.training.HWmodel$beta, 
                            gamma=ts.training.HWmodel$gamma)
  
  forec = forecast(ts.HWmodel, 18)
  HW.plot = plot(ts.HWmodel)
  forecast.plot = plot(forec, main = "Holtwinter Seasonality Forecast for 2019-2020")
  list(HW.plot, forecast.plot)
  
}


##For Total Cost of Work Orders in each Craft Group

COST_TS_MSE = COUNT_COUNT_TS_MSE = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(totalcost = sum(actualcosts))%>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)

  ts = ts(tsdata$totalcost, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$totalcost[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.SESmodel <- HoltWinters(ts.training, beta=FALSE, gamma=FALSE)
  ts.training.DESmodel <- HoltWinters(ts.training, gamma=FALSE)
  ts.training.HWmodel <- HoltWinters(ts.training)
  
  ts.SESmodel <- HoltWinters(ts,
                             alpha=ts.training.SESmodel$alpha, 
                             beta=FALSE, 
                             gamma=FALSE)
  ts.DESmodel <- HoltWinters(ts, 
                             alpha=ts.training.DESmodel$alpha, 
                             beta=ts.training.DESmodel$beta, 
                             gamma=FALSE)
  ts.HWmodel <- HoltWinters(ts, 
                            alpha=ts.training.HWmodel$alpha, 
                            beta=ts.training.HWmodel$beta, 
                            gamma=ts.training.HWmodel$gamma)
  
  data.start <- train.periods + 1
  data.end <- train.periods + test.periods
  
  fit.start <- train.periods 
  fit.end <- train.periods + test.periods - 1
  
  SES.MSE <- sum((ts.SESmodel$fitted[fit.start:fit.end] - ts[data.start:data.end])^2)/(test.periods)
  
  fit.start <- train.periods - 1 
  fit.end <- train.periods + test.periods - 2
  
  DES.MSE <- sum((ts.DESmodel$fitted[fit.start:fit.end] - ts[data.start:data.end])^2)/(test.periods)
  
  fit.start <- train.periods - cycle + 1 
  fit.end <- train.periods + test.periods - cycle
  
  HW.MSE <- sum((ts.HWmodel$fitted[fit.start:fit.end] - ts[fit.start:fit.end])^2)/(test.periods)
  
  MSE = data.frame(SES.MSE, DES.MSE, HW.MSE)
  
  print(MSE)
  print(paste("The minimum MSE is", 
              names(MSE)[which.min(MSE)],
              "=",
              round(MSE[which.min(MSE)]))) 
  
}

### SES function for cost

SES.prediction.cost = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(totalcost = sum(actualcosts)) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$totalcost, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$totalcost[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.SESmodel <- HoltWinters(ts.training, beta=FALSE, gamma=FALSE)
  
  ts.SESmodel <- HoltWinters(ts,
                             alpha=ts.training.SESmodel$alpha, 
                             beta=FALSE, 
                             gamma=FALSE)
  
  forec = forecast(ts.SESmodel, 18)
  SES.plot = plot(ts.SESmodel)
  forecast.plot = plot(forec, main = "Single Exponential Smoothing Forecast for 2019-2020")
  list(SES.plot, forecast.plot)
  
}

###DES function for cost

DES.prediction.cost = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(totalcost = sum(actualcosts)) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$totalcost, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$totalcost[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.DESmodel <- HoltWinters(ts.training, gamma=FALSE)
  
  ts.DESmodel <- HoltWinters(ts,
                             alpha=ts.training.DESmodel$alpha, 
                             beta=ts.training.DESmodel$beta, 
                             gamma=FALSE)
  
  forec = forecast(ts.DESmodel, 18)
  DES.plot = plot(ts.DESmodel)
  forecast.plot = plot(forec, main = "Double Exponential Smoothing Forecast for 2019-2020")
  forecast.plot
  
}

### HW function for cost

HW.prediction.cost = function(craftgroup) {
  tsdata = nonzerocost %>% filter(craftgroup == !!craftgroup) %>% 
    group_by(ymrequested) %>% 
    summarize(totalcost = sum(actualcosts)) %>%
    full_join(wocount, by = "ymrequested") %>%
    na_replace(fill = 0) %>%
    select(-wocount) %>%
    arrange(ymrequested)
  
  ts = ts(tsdata$totalcost, 
          start = c(2001,1), 
          end = c(2019,6), 
          frequency = 12)
  
  
  train.periods = 155
  test.periods <- 67
  
  ts.training <- ts(tsdata$totalcost[1:train.periods], start = c(2001,1), end = c(2019,6), frequency = 12)
  
  ts.training.HWmodel <- HoltWinters(ts.training)
  
  ts.HWmodel <- HoltWinters(ts,
                            alpha=ts.training.HWmodel$alpha, 
                            beta=ts.training.HWmodel$beta, 
                            gamma=ts.training.HWmodel$gamma)
  
  forec = forecast(ts.HWmodel, 18)
  HW.plot = plot(ts.HWmodel)
  forecast.plot = plot(forec, main = "Holtwinter Seasonality Forecast for 2019-2020")
  list(HW.plot, forecast.plot)
  
}

