library(tidyverse)
library(forecast)

#The following functions are used to identify the time series method that gives the lowest test MSE

##For Number of Work Orders by Craft Group

COUNT_COUNT_TS_MSE = function(craftgroup) {
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

COUNT_TS_MSE("PLUMBING/RESTROOM")
COUNT_TS_MSE("FURNITURES/UTILITIES/APPLIANCES")
COUNT_TS_MSE("HEALTH/SAFETY/SECURITY")
COUNT_TS_MSE("ELECTRICITY/ENERGY")
COUNT_TS_MSE("OUTDOORS")
COUNT_TS_MSE("VEHICLE/TRANSPORTATION/DELIVERY")
COUNT_TS_MSE("CLEANING/SANITIZATION")
COUNT_TS_MSE("CONSTRUCTION/BUILDING")
COUNT_TS_MSE("EVENT/RECREATION/F&B")
COUNT_TS_MSE("PLANT/MATERIALS")
COUNT_TS_MSE("ADMIN/PERSONNEL/TRAINING")
COUNT_TS_MSE("PREVENTIVE/SCHEDULED")
COUNT_TS_MSE("INSPECTION")
COUNT_TS_MSE("IT/NETWORK")
COUNT_TS_MSE("CONTRACT")


#For Total Cost of Work Orders in each Craft Group

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

COST_TS_MSE("PLUMBING/RESTROOM")
