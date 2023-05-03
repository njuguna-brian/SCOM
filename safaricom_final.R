
# Importing the Packages --------------------------------------------------

library(pacman)
p_load(lattice, dplyr, tidyr, readxl, quantmod, ModelMetrics, randomForest, rpart ,rpart.plot, 
       xts, lubridate, TTR, PerformanceAnalytics, ggplot2, dygraphs, corrplot,
       recipes, caret, glmnet, kernlab, pROC)

# Importing data ----------------------------------------------------------

safaricom <- read_excel("E:/Desktop/safcom/stock_prices.xlsx") %>% 
  mutate(Date = ymd(Date))

id <- safaricom$Date
safaricom <- safaricom %>% 
dplyr::select(-Date) %>% 
  xts(order.by = ymd(id))

head(safaricom)

# Descriptive ------------------------------------------------------------

table.Arbitrary(Cl(safaricom) |> CalculateReturns() |> na.omit(), 
                metrics = c('mean', 'sd', 'skewness', 'kurtosis'),
                metricsNames = c('mean', 'sd', 'skewness', 'kurtosis'))

# Plotting ----------------------------------------------------------------

OHLC(safaricom) %>%
  dygraph() %>%
  dyCandlestick()

# Constructing features: Technical Indicators -------------------------------

df <- safaricom
df$MACD <- MACD(Cl(df), nFast = 5, nSlow = 21, maType = 'EMA')$macd
df$RSI <- RSI(Cl(df), n = 10, maType = 'EMA')
df$ADX <- ADX(HLC(df), n = 21,  maType = 'EMA')$DX
df$VOL <- rollapply(Cl(df), width = 10, FUN = sd)
df$SNR <- SNR(HLC(df), n = 21)
df$fastK <- stoch(HLC(df), maType = 'EMA')$fastK
df$ATR <- ATR(HLC(df), n = 5, maType = 'EMA')$atr
df$CCI <- CCI(HLC(df), n = 21, maType = 'EMA')
df$pctB <- BBands(HLC(df), n = 21, maType = 'EMA')$pctB
df$VWAP <- CalculateReturns(VWMA(Cl(df), volume = df$Volume, n = 5))
df$RETURN <- CalculateReturns(Cl(df))

# Constructing the dependent variable -------------------------------------

df <- df %>% 
  data.frame() %>% 
  dplyr::select(!c(Open, High, Low, Close, Volume)) %>% 
  dplyr::mutate(Direction = ifelse(df$RETURN <= 0, "Down", "Up"))

df$Direction <- lead(df$Direction)
  
df <- df %>%
  dplyr::select(!c(RETURN))

df <- na.omit(df)

# Checking ----------------------------------------------------------------

# 1. Correlation
vari <- findCorrelation(cor(df[, -11]))
vari
# 2. Near Zero Variance
nzv <- nearZeroVar(df[, -11])

# 3. Linear Combination
var_col <- findLinearCombos(df[,-11])

# Splitting the Data ------------------------------------------------------

set.seed(123)
indicies <- createDataPartition(df$Direction, p = 0.8, times = 1)
train <- df[indicies$Resample1,]
test <- df[-indicies$Resample1,]


# Pre-processing Data ------------------------------------------------------

train_init <- recipe(Direction ~., data = train)
train_step1 <- train_init %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors())

train_step2 <- train_step1 %>% 
  prep(training = train, retain = TRUE)

train.df <- juice(train_step2)   
test.df <- bake(train_step2, test)

# Feature Plot ------------------------------------------------------------

featurePlot(x = train.df[,-11],
            y = train.df$Direction,
            plot = "density",
            auto.key = list(columns = 2))

# Modelling ---------------------------------------------------------------

ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = TRUE,
                     classProbs = T,
                     savePredictions =T)

rf_tune <- expand.grid(mtry = c(2, 3, 4, 6, 8))

# 1. Random Forest
rf_model <- train(Direction ~.,
                  data = train.df,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = rf_tune)
print(rf_model)

# Picking the best threshold
rf_thresh <- thresholder(rf_model,
                         threshold = seq(from = .1, to = .9, length = 50),
                         final = F,
                         statistics = c('Sensitivity', 'Specificity', 'Accuracy', 'J'))

max_row <- which.max(rf_thresh$J)
rf_thresh[max_row, ]
predicted <- predict(rf_model, test.df, type="prob")[,1]
predicted_rf <- ifelse(predicted >= 0.57347, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(test.df$Direction, predicted_rf)


predicted <- predict(rf_model$finalModel, test.df)

# 2. Lasso Model
lasso_model <- train(Direction ~ .,
                  data = train.df,
                  method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(from = 1e-6, to = .2, length = 50)))


print(lasso_model)
plot(lasso_model)
# Picking the best threshold
lasso_thresh <- thresholder(lasso_model,
                         threshold = seq(from = .1, to = .9, length = 50),
                         final = F,
                         statistics = c('Sensitivity', 'Specificity', 'Accuracy', 'J'))

max_row <- which.max(lasso_thresh$J)
ls_thresh <- lasso_thresh[max_row, ] # Which has given us the maximum Youden's J

predicted2 <- predict(lasso_model, as.matrix(test.df[,-11]), type="prob")[,1]
predicted_ls <- ifelse(predicted2 >= ls_thresh$prob_threshold, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(test.df$Direction, predicted_ls)

# 3.Ridge Model

ridge_model <- train(Direction ~ .,
                  data = train.df,
                  method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = expand.grid(alpha = 1,
                                         lambda = seq(from = 1e-6, to = .2, length = 50)))


print(ridge_model)
plot(ridge_model)
# Picking the best threshold
ridge_thresh <- thresholder(ridge_model,
                            threshold = seq(from = .1, to = .9, length = 50),
                            final = F,
                            statistics = c('Sensitivity', 'Specificity', 'Accuracy', 'J'))

max_row <- which.max(ridge_thresh$J)
ridge_thresh[max_row, ] # Which has given us the maximum Youden's J

predicted3 <- predict(ridge_model, test.df, type="prob")[,1]
predicted_ridge <- ifelse(predicted3 >= 0.63878, "Down", "Up") %>% 
  as.factor()
caret::confusionMatrix(test.df$Direction, predicted_ridge)









