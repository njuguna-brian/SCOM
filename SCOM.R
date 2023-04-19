
# Importing the Packages --------------------------------------------------

library(pacman)
p_load(dplyr, tidyr, readxl, quantmod, ModelMetrics, randomForest, rpart ,rpart.plot, 
       xts, lubridate, TTR, PerformanceAnalytics, ggplot2, dygraphs, xgboost, tseries)

# Importing data ----------------------------------------------------------

safaricom <- read_excel("stock_prices.xlsx") %>% 
  mutate(Date = ymd(Date))

id <- safaricom$Date

safaricom2 <- safaricom %>% 
  select(-Date) %>% 
  xts(order.by = ymd(id))

head(safaricom)

# Descriptive ------------------------------------------------------------

data.frame(
  mean = format(sapply(safaricom[,2:ncol(safaricom)], mean), scientific = FALSE), 
  variance = format(sapply(safaricom[,2:ncol(safaricom)], var), scientific = TRUE),
  skewness = sapply(safaricom[,2:ncol(safaricom)], skewness),
  kurtosis = sapply(safaricom[,2:ncol(safaricom)], kurtosis)
)

# convert data to long format ---------------------------------------------

safaricom_long <- pivot_longer(
    safaricom[ ,1:5],
    cols = -Date,
    values_to = "price",
    names_to = "stock"
  )


# Plotting ----------------------------------------------------------------

safaricom_long %>% 
  ggplot(aes(x  = Date)) +
  geom_line(aes(y = price, col = stock), linewidth = .7) +
  facet_wrap(~stock, ncol = 2) +
theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.text = element_text(colour = "black"),
        plot.title = element_text(colour = "black", hjust = .5)) +
  guides(col = "none") +
  labs(title = "SCOM Stock Prices")

# Technical Indicators ---------------------------------------------------------------

safaricom_returns <- safaricom2$Close

#MACD
MACD <- MACD(safaricom_returns)

# RSI

RSI <- RSI(safaricom_returns)

# ADX
ADX <- ADX( safaricom2[,1:4] )

# BBANDS
bband <- BBands(safaricom_returns)

# Stochastic Oscillator 
sc <- stoch(safaricom_returns)

# Average True Range
ATR <- ATR(safaricom2[,1:4])

# Commodity Channel Index
CCI <- CCI(safaricom_returns)

# Returns -----------------------------------------------------------------

safaricom_returns$Return <- CalculateReturns(safaricom_returns)
adf.test(na.omit(safaricom_returns$Return))

safaricom_returns <- safaricom_returns %>% 
  data.frame()

safaricom_returns$Direction <- case_when(as.numeric(safaricom_returns$Return) < 0 ~ "Down", 
                          as.numeric(safaricom_returns$Return) >0 ~ "Up", 
                          .default = "Down")
saf_var <- data.frame("MACD" = MACD$macd, RSI, "ADX" = ADX$ADX, "middleband" = bband$mavg,
                      "fastD" = sc$fastD, "ATR" = ATR$atr, "CCI" = CCI$cci,
                      row.names = row.names(safaricom_returns),
                      "Direction" = safaricom_returns$Direction)
 
returns_data <- safaricom_returns %>% 
  na.omit() %>% 
  mutate(Date = as.Date(row.names(na.omit(safaricom_returns))))


# Plotting the returns
return_plot <- returns_data %>% 
  ggplot((aes(x = Date))) +
  geom_line(aes(y = Return), col = "#8B008B") + #lightgoldenrod4, steelblue4, midnightblue, peru,coral4
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.text = element_text(colour = "black"),
        plot.title = element_text(colour = "black", hjust = .5)) +
  guides(col = "none") +
  labs(title = "SCOM Stock Prices")
return_plot

# Plotting the direction using candlesticks

safaricom2 %>%
  as.data.frame() %>% 
  select(-c(Volume)) %>% 
  tail(n = 60) %>% 
  dygraph() %>%
  dyCandlestick()

# Feature Engineering -----------------------------------------------------

#saf_var$macd_indicator <- ifelse(saf_var$macd < saf_var$signal, "bearish", "bullish")
glimpse(saf_var)

# leading Directions -----------------------------------------------------

saf_var$Direction <- lead(saf_var$Direction)  
saf_var$Direction = as.factor(saf_var$Direction)
saf_var <- saf_var %>% 
  na.omit() %>% 
  data.frame() %>% 
  `row.names<-`(NULL)

# Splitting the data ------------------------------------------------------

size <- 0.8
sample_size <- floor(size * nrow(saf_var))
set.seed(123)
indicies <- sample(seq_len(nrow(saf_var)), size = sample_size)
train_data <- saf_var[indicies,]
test_data <- saf_var[-indicies,]

# Logistic Regression -----------------------------------------------------

logistic_model <- glm((Direction) ~ ., data = train_data, family = "binomial")
summary(logistic_model)

predict_train <- predict(logistic_model, train_data, type = "response")
predict_label_train <- ifelse(predict_train < 0.5, "Down", "Up") %>% 
  as.factor()
predict_data <- predict(logistic_model, test_data, type= "response")
predict_data

predict_label <- ifelse(predict_data < 0.5, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(test_data$Direction, predict_label)
caret::confusionMatrix(train_data$Direction, predict_label_train)

# Random Forest -----------------------------------------------------------

set.seed(845)
random_model <- randomForest(x = train_data[, 1:7], y = (train_data$Direction), ntree = 400)
predict_forest <- predict(random_model, new.data = train_data)
caret::confusionMatrix(train_data$Direction, predict_forest)

predict_forest_test <- predict(random_model, newdata = test_data)
caret::confusionMatrix(test_data$Direction, predict_forest_test)

# 1. XGBOOST --------------------------------------------------------------

train_data$Direction <- ifelse(train_data$Direction == "Up", 1, 0)
test_data$Direction <- ifelse(test_data$Direction == "Up", 1, 0)
y_train = train_data$Direction
y_test = test_data$Direction

xgb_train = xgb.DMatrix(as.matrix(train_data[,1:7]), label = y_train)
xgb_test = xgb.DMatrix(as.matrix(test_data[,1:7]), label = y_test)
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 5,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  eval_metric = "auc",
  objective = "binary:logistic"
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  verbose = 1
)

test_data$Direction <- ifelse(test_data$Direction == 1, "Up", "Down") %>% 
  as.factor()

train_data$Direction <- ifelse(train_data$Direction == 1, "Up", "Down") %>% 
  as.factor()

xgb_preds_train <- predict(xgb_model, xgb_train, type = "response")
xgb_preds_train <- ifelse(xgb_preds_train <0.48, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(train_data$Direction, xgb_preds_train)

xgb_preds <- predict(xgb_model, xgb_test, type = "response")
xgb_preds <- ifelse(xgb_preds <0.48, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(test_data$Direction, xgb_preds)



















