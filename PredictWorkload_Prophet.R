###################################################################   Prophet function
#################  
Data_Process_Role <- read.csv("Latest_Workload_Data.csv")
colnames(Data_Process_Role)[colnames(Data_Process_Role)=="ï..NOFO"] <- "NOFO"
Data_PO_PreFundingNoncompeting <- Data_Process_Role %>% filter(Role == "PO" & TaskProcess == "Pre Funding Noncompeting Review")
Data_PO_PreFundingNoncompeting$Date_NOFO <- as.Date(as.character(Data_PO_PreFundingNoncompeting$Date_NOFO), format="%Y-%m-%d")
Data_PO_PreFundingNoncompeting$Date_Task <- as.Date(as.character(Data_PO_PreFundingNoncompeting$Date_Task), format="%Y-%m-%d")

Data_NOFO_Published <- Data_PO_PreFundingNoncompeting  %>% select (NOFO,Date_Task) %>% group_by(Date_Task) %>% 
  summarise( NOFO_Count = n_distinct(NOFO))
Data_Task_Count <- Data_PO_PreFundingNoncompeting  %>% select (Date_Task,TaskCount) %>% group_by(Date_Task) %>% 
  summarise( Task_Count = sum(TaskCount))

######################################  Join regressor and taskcount together
colnames(Data_NOFO_Published)[colnames(Data_NOFO_Published)=="Date_Task"] <- "ds"
colnames(Data_Task_Count)[colnames(Data_Task_Count)=="Date_Task"] <- "ds"
colnames(Data_Task_Count)[colnames(Data_Task_Count)=="Task_Count"] <- "y"
df <- Data_Task_Count %>% left_join(Data_NOFO_Published, by="ds")
df[is.na(df)] <- 0
df_train <- df[1:102,]
df_test <- df[103:111,]
######################################
library(prophet)
library(tidyr)
m <- prophet(df_train, fit=FALSE)
m <- add_regressor(m, 'NOFO_Count', standardize = FALSE)
m <- fit.prophet(m, df_train)
future <- make_future_dataframe(m, periods = 12) %>%
  left_join((m$history %>%  select(ds, NOFO_Count)), by=c('ds'='ds'))
future <- replace_na(future, list(NOFO_Count=0))
forecast <- predict(m, future)
dyplot.prophet(m, forecast, uncertainty = TRUE) 
prophet_plot_components(m, forecast)
#######################################  find the best parameters
df_train$ds <- as.POSIXct(df_train$ds, format="%Y-%m-%d")
df_valid <- df_train[91:102,]
Actual <- as.vector(df_valid$y)
df_test$ds <- as.POSIXct(df_test$ds, format="%Y-%m-%d")
############################################################### Prophet grid
prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05,.03,.01,0.5,0.3,0.2,0.1,0.001,0.002),
                           seasonality_prior_scale = c(1,10,20,50,70,100), 
                           n_changepoints  = sample(5:25, 10, replace = F))
results <- vector(mode = 'numeric', length = nrow(prophetGrid))
outcome <- vector(mode = 'numeric', length = nrow(prophetGrid))

# Search best parameters
for (i in seq_len(nrow(prophetGrid))) {
  parameters <- prophetGrid[i, ]
  
  m <- prophet(df_train,seasonality.prior.scale = parameters$seasonality_prior_scale, 
               n_changepoints=parameters$n_changepoints,
               changepoint.prior.scale = parameters$changepoint_prior_scale, fit=FALSE)
  m <- add_regressor(m, 'NOFO_Count', standardize = FALSE)
  m <- fit.prophet(m, df_train)
  
  future <- make_future_dataframe(m, periods = 12) %>%
    left_join((m$history %>%  select(ds, NOFO_Count)), by=c('ds'='ds'))
  future$NOFO_Count[103] <- 27
  future$NOFO_Count[104] <- 31
  future$NOFO_Count[105] <- 57
  future$NOFO_Count[106] <- 63
  future$NOFO_Count[107] <- 55
  future$NOFO_Count[108] <- 54
  future$NOFO_Count[109] <- 35
  future$NOFO_Count[110] <- 23
  future$NOFO_Count[111] <- 20
  future <- replace_na(future, list(NOFO_Count=0))
  forecast <- predict(m, future)
  Predicted <- as.vector(forecast[forecast$ds %in% df_valid$ds, 'yhat'])
  results[i] <- Metrics::mape (df_test$y,forecast[forecast$ds %in% df_valid$ds, 'yhat'])
  outcome[i] <- Metrics::mae (df_test$y,forecast[forecast$ds %in% df_valid$ds, 'yhat'])
}

prophetGrid <- cbind(prophetGrid, results,outcome)
best_params <- prophetGrid[prophetGrid$results == min(results), ]
################################# changepoint_prior_scale seasonality_prior_scale n_changepoints   results
#################################            0.5                      50             25           0.3113957
#################################  Model
forecast['NOFO_Count']
future['NOFO_Count']
retrain <- bind_rows(df_train, df_valid)
m = prophet(seasonality.prior.scale = 50, 
            changepoint.prior.scale = .5, n_changepoints = 25)

m <- add_regressor(m, 'NOFO_Count',standardize = FALSE)
m <- fit.prophet(m, df_train)
future <- make_future_dataframe(m, periods = 12, freq = 'm') %>%
  left_join((m$history %>%  select(ds, NOFO_Count)), by=c('ds'='ds'))
future$NOFO_Count[103] <- 27
future$NOFO_Count[104] <- 31
future$NOFO_Count[105] <- 57
future$NOFO_Count[106] <- 63
future$NOFO_Count[107] <- 55
future$NOFO_Count[108] <- 54
future$NOFO_Count[109] <- 35
future$NOFO_Count[110] <- 23
future$NOFO_Count[111] <- 20
future <- replace_na(future, list(NOFO_Count=0))
forecast <- predict(m, future)
dyplot.prophet(m, forecast, uncertainty = TRUE) 
prophet_plot_components(m, forecast)