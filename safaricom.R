
# Importing the Packages --------------------------------------------------

library(pacman)
p_load(dplyr, tidyr, readxl, quantmod, ModelMetrics, randomForest, rpart ,rpart.plot, 
       xts, lubridate, TTR, PerformanceAnalytics, ggplot2, dygraphs)

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

#ADX
ADX <- ADX( safaricom2[,1:4] )

#BBANDS
bband <- BBands(safaricom_returns)

#Stochastic Oscillator 
sc <- stoch(safaricom_returns)

saf_var <- data.frame(safaricom_returns, MACD, RSI, ADX, bband, sc)

# Returns -----------------------------------------------------------------

safaricom_returns$Return <- CalculateReturns(safaricom_returns)

safaricom_returns <- safaricom_returns %>% 
  data.frame()

safaricom_returns$Direction <- case_when(as.numeric(safaricom_returns$Return) < 0 ~ "Down", 
                          as.numeric(safaricom_returns$Return) >0 ~ "Up", 
                          .default = "Down")
 
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

saf_var$macd_indicator <- ifelse(saf_var$macd < saf_var$signal, "bearish", "bullish")
glimpse(saf_var)

saf_var <- saf_var %>% 
  select(rsi, DIp, DIn, ADX, pctB, slowD, macd_indicator) %>% 
  mutate(Direction = safaricom_returns$Direction) %>% 
  na.omit()


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


# Trying ------------------------------------------------------------------

test_saf_var <- data.frame(saf_var[, 2:ncol(saf_var)], "Direction" = safaricom_returns$Direction)

test_saf_var$Direction <- lead(test_saf_var$Direction)
  
test_saf_var <- test_saf_var %>%
  na.omit() %>%
  as.data.frame() %>%
  `row.names<-`(NULL)

# splitting
size <- 0.8
sample_size2 <- floor(size * nrow(test_saf_var))
set.seed(123)
indicies2 <- sample(seq_len(nrow(test_saf_var)), size = sample_size2)
train_data2 <- test_saf_var[indicies2,]
test_data2 <- test_saf_var[-indicies2,]



MODEL <- glm(factor(Direction) ~., train_data2, family = binomial())
summary(MODEL)
step_model <- step(MODEL, direction = "both")

train_data3 <- train_data2 %>%
  select(macd, signal, rsi, DIn, mavg,DX, fastK, fastD, Direction) # 

test_data3 <- test_data2 %>%
  select(macd, signal, rsi, DIn, mavg, fastK,DX, fastD, Direction)

MODEL2 <- glm(factor(Direction) ~., train_data3, family = binomial())
summary(MODEL2)


predict_train2 <- predict(MODEL2, train_data3, type = "response")
predict_label_train2 <- ifelse(predict_train2 < 0.5, "Down", "Up") %>% 
  as.factor()
predict_data2 <- predict(MODEL2, test_data3, type= "response")

predict_label2 <- ifelse(predict_data2 < 0.5, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(factor(test_data2$Direction), predict_label2)
caret::confusionMatrix(train_data$Direction, predict_label_train)


# Random Forest
random_model <- randomForest(x = train_data3[, 1:8], y = factor(train_data3$Direction), ntree = 10000)













