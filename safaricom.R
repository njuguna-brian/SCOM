
# Importing the Packages --------------------------------------------------

library(pacman)
p_load(lattice, dplyr, tidyr, readxl, quantmod, ModelMetrics, randomForest, rpart ,rpart.plot, 
       xts, lubridate, TTR, PerformanceAnalytics, ggplot2, dygraphs, corrplot,
       recipes, caret, glmnet, kernlab, pROC)

# Importing data ----------------------------------------------------------

safaricom <- read_excel("stock_prices.xlsx") %>% 
  mutate(Date = ymd(Date))

id <- safaricom$Date
safaricom <- safaricom %>% 
  select(-Date) %>% 
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
df$STOCH <- stoch(HLC(df), maType = 'EMA')$fastK
df$ATR <- ATR(HLC(df), n = 5, maType = 'EMA')$atr
df$CCI <- CCI(HLC(df), n = 21, maType = 'EMA')
df$VWAP <- CalculateReturns(VWMA(Cl(df), volume = df$Volume, n = 5))
df$RETURN <- CalculateReturns(Cl(df))

# Omitting the OHLCV data
df <- df |>
  data.frame() |>
  select(!c('Open', 'High', 'Low', 'Close', 'Volume')) |>
  as.xts()

# adding the dependent variable
df$Direction <- ifelse(df$RETURN <= 0, "Down", 'Up') |>
  factor() |>
  lead()

# Down: 1 | Up: 2

df <- na.omit(df)

train.df <- df['::2021'] |> data.frame()
test.df <- df['2022::'] |> data.frame()

train.df$Direction <- factor(train.df$Direction,
                             levels = c(1, 2),
                             labels = c('Down', 'Up'))

test.df$Direction <- factor(test.df$Direction,
                             levels = c(1, 2),
                             labels = c('Down', 'Up'))


features <- colnames(df[, -ncol(df)])


# Visualizations ----------------------------------------------------------

corrplot::corrplot(
  cor(train.df[, -ncol(df)]),
  method = 'square',
  type = 'full',
  order = 'hclust',
  tl.col = 'black'
)

featurePlot(x = train.df[, -11],
            y = train.df[, 'Direction'],
            plot = 'density',
            scales = list(x = list(relation = 'free'),
                          y = list(relation = 'free')),
            adjust = .5,
            pch = '.',
            layout = c(4, 3),
            auto.key = list(columns = 2))
# majority are highly skewed: consder a yeo-johnson transform for skewed variables

featurePlot(x = train.df[, -11],
            y = train.df[, 'Direction'],
            plot = 'box',
            col = 'black',
            scales = list(x = list(rot = 90),
                          y = list(relation = 'free')),
            layout = c(3, 4))
# No visible separations really


# Feature engineering -----------------------------------------------------

step_init <- recipe(Direction ~ ., data = train.df)

scom_start <- step_init |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_YeoJohnson(all_numeric_predictors())

# extracting the transformed dataset from the object
init_f.ng <- scom_start |>
  prep(training = train.df, retain = TRUE, verbose = TRUE)
train.df_1 <- juice(init_f.ng)

featurePlot(x = train.df_1[, -11],
            y = train.df_1$Direction,
            plot = 'density',
            scales = list(x = list(relation = 'free'),
                          y = list(relation = 'free')),
            adjust = 1.5,
            pch = '.',
            layout = c(4, 3),
            auto.key = list(columns = 2))
# most are symmetric features now and they are centered and scaled properly

# Constructing the test dataset with the same pre processing steps operated on train data
test.df_1 <- bake(init_f.ng, test.df)


# Setting CARET parameters ------------------------------------------------

ctrl <- trainControl(method = "cv",
                     number = 10,
                     returnResamp = 'none',
                     #summaryFunction = twoClassSummary,
                     classProbs = T,
                     savePredictions = T,
                     verboseIter = F)

# probability cuttof thresholds
probs <- seq(from = .1,to = .9, by = .01)

# Elastic Net Regression | Ridge | Lasso ----------------------------------

# They are models which can handle the multi-collinearity

# RIDGE REGRESSION
ridgefit <- train(Direction ~ ., 
                  data = train.df_1, 
                  method = "glmnet",
                  trControl = ctrl,
                  metric = 'ROC',
                  verbose = F,
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(from = 1e-6, to = .2, length = 50)))

th_ridge <- thresholder(ridgefit,
                        threshold = seq(from = .1, to = .9, length = 50),
                        final = F,
                        statistics = c('Sensitivity', 'Specificity', 'Accuracy', 'J'))

View(th_ridge) # best threshold from training data: 0.6, acc: 58% using Youden's J as measure

ridge_thresh <- th_ridge |>
  filter(J == max(J)) |>
  pull(prob_threshold)

plot(ridgefit) # lambda: 0.05714357 | Accuracy: 61.3% >> 0.008164224
ridge_model <- glmnet(x = train.df_1[, -11], y = unlist(train.df_1[, 11]), family = 'binomial',
                      alpha = 0, lambda = 0.008164224)

ridge_glm_prob <- predict(ridge_model, as.matrix(train.df_1[, 1:10]), type = 'response') |>
  as.numeric()

# val = ifelse(ridge_glm_prob < 0.3887933, 'Down', 'Up') |> factor()
# confusionMatrix(val, train.df_1$Direction)

ridge_train_roc <- roc(response = train.df_1$Direction,
                       predictor = ridge_glm_prob)  %>%
  coords(ret = "all", transpose = FALSE)
View(ridge_train_roc) 

# the threshold to use for training
ridge_thresh <- ridge_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 

# LASSO
lassofit <- train(Direction ~ ., 
                  data = train.df_1, 
                  method = "glmnet",
                  trControl = ctrl,
                  metric = 'ROC',
                  tuneGrid = expand.grid(alpha = 1,
                                         lambda = seq(from = 1e-9, to = .05, length = 50)))

plot(lassofit) # lambda: 0.005102042 | Accuracy: 61.5%
lasso_model <- glmnet(x = train.df_1[, -11], y = unlist(train.df_1[, 11]), family = 'binomial',
                      alpha = 1, lambda = 0.005102042)

lasso_glm_prob <- predict(lasso_model, as.matrix(train.df_1[, 1:10]), type = 'response') |>
  as.numeric()
lasso_train_roc <- roc(response = train.df_1$Direction,
                       predictor = lasso_glm_prob)  %>%
  coords(ret = "all", transpose = FALSE)
View(lasso_train_roc) # best threshold from training data: 0.3957565, acc: 58%

# the best threshold for training
lasso_thresh <- lasso_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 


# Random Forests ----------------------------------------------------------

rfFit <- train(Direction ~ .,
               data = train.df_1,
               method = 'rf',
               trControl = ctrl,
               tuneGrid = expand.grid(mtry = c(2,3,4,5,6,7,8,9,10)))

plot(rfFit) # 2 trees are optimal | Accuracy 61.5%

rf_model <- rfFit$finalModel
rf_prob <- predict(rf_model, type = 'prob')[, 2] |>
  as.numeric()
rf_train_roc <- roc(response = train.df_1$Direction,
                       predictor = rf_prob)  %>%
  coords(ret = "all", transpose = FALSE)
View(rf_train_roc) # best threshold from training data: 0.3957565, acc: 58%

# the best threshold for training
rf_thresh <- rf_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 

# Feature engineering: PCA -------- ---------------------------------

scom_pca <- step_init |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), num_comp = 10, threshold = .95)

# extracting the transformed dataset from the object
pca.ng <- scom_pca |>
  prep(training = train.df, retain = TRUE, verbose = TRUE)
train.df_2 <- juice(pca.ng)

train.df_2 |>
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, col = Direction))+
  labs(title = "Principal Components Analysis",
       x = "PC-01", y = "PC-02")+
  theme_minimal()

# Constructing the test dataset 
test.df_2 <- bake(pca.ng, test.df)


# Feature engineering: Kernel PCA -----------------------------------------

set.seed(1)
sig_range <- 
  step_init |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  prep(training = train.df, retain = TRUE, verbose = TRUE) |>
  juice(all_numeric_predictors()) |>
  as.matrix() |>
  sigest(frac = 1) 

scom_kpca <- step_init |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_kpca(all_numeric_predictors(), 
            num_comp = 10, 
            #threshold = .95,
            options = list(kernel = "rbfdot", 
                           kpar = list(sigma = sig_range[2])))

# extracting the transformed dataset from the object
kpca.ng <- scom_kpca |>
  prep(training = train.df, retain = TRUE, verbose = TRUE)
train.df_3 <- juice(kpca.ng)

train.df_3 |>
  ggplot()+
  geom_point(aes(x = kPC01, y = kPC02, col = Direction))+
  labs(title = "Kernel Principal Components Analysis",
       x = "kPC-01", y = "kPC-02")+
  theme_minimal()

kpca_eigen <- unname(eig((kpca.ng$steps)[[4]][[5]])) |>
  data.frame() |>
  setNames("kpca_eigen") |>
  mutate(
    component = 1:10,
    component_contribution = kpca_eigen / sum(kpca_eigen),
    group_contribution = cumsum(component_contribution)
  ) |>
  setNames(c("Eigen", "component", "component_contribution", "group_contribution")) |>
  ggplot(aes(x = factor(component), y = group_contribution))+
  geom_segment(aes(xend = factor(component), yend = 0))+
  geom_point()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = 'Component contribution (Kernel-PCA)', x = '# of components', y = '% Variance explained')+
  theme_classic()
kpca_eigen
# shows that component 8 already explains upto 94$ of total variability

# Constructing the test dataset 
test.df_3 <- bake(kpca.ng, test.df)

# Logistic Regression -------------------------------------------------

glmFit.pca <- train(glm_model_pca$formula, data = train.df_2,
                method = 'glmStepAIC',
                trControl = trainControl(method = "cv"))

glmFit.pca
glm_pca <- glmFit.pca$finalModel

glmFit.kpca <- train(glm_model_kpca$formula, data = train.df_3,
                method = 'glmStepAIC',
                trControl = trainControl(method = "cv"))

glmFit.kpca
glm_kpca <- glmFit.kpca$finalModel

pca_probs <- predict(glm_pca, type = 'response') |> as.numeric()
kpca_probs <- predict(glm_kpca, type = 'response') |> as.numeric()

# thresholding
glm_pca_train_roc <- roc(response = train.df_1$Direction,
                       predictor = pca_probs)  %>%
  coords(ret = "all", transpose = FALSE)
View(glm_pca_train_roc) # best threshold from training data: 0.3957565, acc: 58%

glm_kpca_train_roc <- roc(response = train.df_1$Direction,
                         predictor = kpca_probs)  %>%
  coords(ret = "all", transpose = FALSE)
View(glm_kpca_train_roc) # best threshold from training data: 0.3957565, acc: 58%

# the best threshold for training
glm_pca_thresh <- glm_pca_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 

glm_kpca_thresh <- glm_kpca_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 

# Support Vector Machines -------------------------------------------------

svmFit.pca <- train(Direction ~ .,
                data = train.df_2,
                method = "svmRadial", 
                trControl = ctrl,
                tuneGrid = expand.grid(sigma = sig_range,
                                       C = seq(from = .1, to = 10, length = 10)),
                scaled = FALSE)
# sigma = 0.1786369 and C = .1 | Accurancy: 63%
svm_pca <- svmFit.pca$finalModel

svm_pca_probs <- predict(svm_pca, train.df_2[, -1], type = 'probabilities')[,2]
svm_pca_train_roc <- roc(response = train.df_2$Direction,
                          predictor = svm_pca_probs)  %>%
  coords(ret = "all", transpose = FALSE)
View(svm_pca_train_roc) # best threshold from training data: 0.3957565, acc: 58%

# the best threshold for training
svm_pca_thresh <- svm_pca_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 

svmFit.kpca <- train(Direction ~ .,
                     data = train.df_3[, 1:9],
                     method = "svmRadial", 
                     trControl = ctrl,
                     tuneGrid = expand.grid(sigma = sig_range,
                                            C = seq(from = .1, to = 10, length = 10)),
                     scaled = FALSE)
# sigma = 0.02630556 and C = 0.1. | Accurancy: 60%
svm_kpca <- svmFit.kpca$finalModel

svm_kpca_probs <- predict(svm_kpca, train.df_3[, 2:9], type = 'probabilities')[,2]
svm_kpca_train_roc <- roc(response = train.df_2$Direction,
                         predictor = svm_kpca_probs)  %>%
  coords(ret = "all", transpose = FALSE)
View(svm_kpca_train_roc) # best threshold from training data: 0.3957565, acc: 58%

# the best threshold for training
svm_kpca_thresh <- svm_kpca_train_roc |>
  filter(youden == max(youden)) |>
  pull(threshold) 


# Predicting the test data ------------------------------------------------

# Ridge regression model
ridge_test_prob <- predict(ridge_model, as.matrix(test.df_1[, -11]), type = 'response') |>
  as.numeric()
ridge_test_pred <- ifelse(ridge_test_prob < ridge_thresh, 'Down', 'Up') |>
  factor()

# LASSO regression model
lasso_test_prob <- predict(lasso_model, as.matrix(test.df_1[, -11]), type = 'response') |>
  as.numeric()
lasso_test_pred <- ifelse(lasso_test_prob < lasso_thresh, 'Down', 'Up') |>
  factor()

# Random Forest Model
rf_test_prob <- predict(rf_model, as.matrix(test.df_1[, -11]), type = 'prob')[, 2] |>
  as.numeric()
rf_test_pred <- ifelse(rf_test_prob < rf_thresh, 'Down', 'Up') |>
  factor()

# Logistic regression models
glm_pca_test_prob <- predict(glm_pca, test.df_2, type = 'response') |>
  as.numeric()
glm_pca_test_pred <- ifelse(glm_pca_test_prob < glm_pca_thresh, 'Down', 'Up') |>
  factor()

glm_kpca_test_prob <- predict(glm_kpca, test.df_3, type = 'response') |>
  as.numeric()
glm_kpca_test_pred <- ifelse(glm_kpca_test_prob < glm_kpca_thresh, 'Down', 'Up') |>
  factor()

# SVMs
svm_pca_test_prob <- predict(svm_pca, test.df_2[, -1], type = 'probabilities')[,2] |>
  as.numeric()
svm_pca_test_pred <- ifelse(svm_pca_test_prob < svm_pca_thresh, 'Down', 'Up') |>
  factor()

svm_kpca_test_prob <- predict(svm_kpca, test.df_3[, 2:9], type = 'probabilities')[,2] |>
  as.numeric()
svm_kpca_test_pred <- ifelse(svm_kpca_test_prob < svm_kpca_thresh, 'Down', 'Up') |>
  factor()


list(ridge_test_pred, lasso_test_pred, rf_test_pred, glm_pca_test_pred, glm_kpca_test_pred,
     svm_pca_test_pred, svm_kpca_test_pred) |>
  lapply(FUN = function(x) mean(x == test.df_1$Direction)) |>
  list2DF() |>
  t() |>
  data.frame() |>
  mutate(
    Model = c('L2-GLM', 'L1-GLM','RANDOMFOREST', 'GLM-PCA', 'GLM-KPCA', 'SVM-PCA',
              'SVM-KPCA')
  ) |>
  setNames(c('Accuracy', 'Model'))
