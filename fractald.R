rm(list = ls())

path <- "/home/manish/Desktop/Data2017/July/fractal/"
setwd(path)

library(data.table)

train <- fread("train.csv")
test <- fread("test.csv")

head(train)
str(train)
str(test)

library(lubridate)

train[,Datetime := ymd(Datetime)]
test[,Datetime := ymd(Datetime)]

train[,summary(Datetime)]
test[,summary(Datetime)]

# Insights

# all IDs are unique
train[,uniqueN(ID)]
test[,uniqueN(ID)]

train[,uniqueN(Item_ID)]
test[,uniqueN(Item_ID)]

item1 <- train[Item_ID == 30295]


# both variables are skewed

train[,summary(Price)]
train[,summary(Number_Of_Sales)]

train[,summary(Category_3)]
test[,summary(Category_3)]

train[,summary(Category_2)]
test[,summary(Category_2)]

train[,summary(Category_1)]
test[,summary(Category_1)]

# which train ID is in test

test[train, .N, by=.EACHI, on='Item_ID']


# remove item IDs not in test
item_remove_train <- train$Item_ID[which(!(train$Item_ID %in% test$Item_ID))]
item_remove_train <- unique(item_remove_train) # There are 82 IDs not in test

train_new <- train[!(Item_ID %in% item_remove_train)]
all(train_new$Item_ID %in% test$Item_ID) # YES

# "%ni%" <- Negate("%in%")
# train$Item_ID[train$Item_ID %ni% test$Item_ID]

# run a simple model and check score - 0.8381 LB Score
sapply(train_new, class)

# Model 1 - Category 3,2,1, Count Item ID

X_train <- train_new[,.(Datetime,Item_ID,Category_3,Category_2,Category_1,Price,Number_Of_Sales)]
X_test <- test[,.(Item_ID,Category_3,Category_2,Category_1)]

X_train[, Item_ID := .N, Item_ID]
X_test[, Item_ID := .N, Item_ID]


# time based validation is required

X_train[,summary(Datetime)]

X_train[,check_year := year(Datetime)]
X_train[,.N,check_year]
X_train[,check_year := NULL]

training_data <- X_train[Datetime < "2016-01-01"]
validation_data <- X_train[Datetime >= "2016-01-01"]

# rm(X_train, X_test)
# gc()

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(training_data$Price))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(validation_data$Price))
dtest <- xgb.DMatrix(data = as.matrix(X_test))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsampleby_tree = 1
  #alpha = 1
  
)

bst1 <- xgb.train(params = params
                 ,data = dtrain
                ,nrounds = 1000
                ,watchlist = list(train = dtrain, valid = dvalid)
                ,metric = 'rmse'
                ,print_every_n = 10
                ,early_stopping_rounds = 40
                ,maximize = F
                )
#   valid-rmse:0.661250

pred1 <- predict(bst1, dtest)
pred1 <- exp(pred1)-1

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(training_data$Number_Of_Sales))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(validation_data$Number_Of_Sales))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.01,
  max_depth = 8,
  min_child_weight = 10,
  subsample = 0.5,
  colsampleby_tree = 0.9
  #alpha = 1
  
)

bst2 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
)

pred2 <- predict(bst2, dtest)
pred2 <- exp(pred2)-1

sub <- data.table(ID = test$ID, Price = pred1, Number_Of_Sales = pred2)
head(sub)

fwrite(sub,"sub1.csv")

# Now Trying to predict Sales Again with Price Variable in train and test

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,-c('Number_Of_Sales','Datetime'),with=F]), label = log1p(training_data$Number_Of_Sales))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,-c('Number_Of_Sales','Datetime'),with=F]), label = log1p(validation_data$Number_Of_Sales))
dtest <- xgb.DMatrix(data = as.matrix(cbind(X_test, Price = pred1)))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 8,
  min_child_weight = 10,
  subsample = 0.5,
  colsampleby_tree = 0.9
  #alpha = 1
  
)

bst2 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
)

imp <- xgb.importance(feature_names = colnames(dtrain), model = bst2)
xgb.plot.deepness(bst2)
xgb.plot.importance(imp)

pred3 <- predict(bst2, dtest)
pred3 <- exp(pred3)-1

sub1 <- data.table(ID = test$ID, Price = pred1, Number_Of_Sales = pred3)
head(sub1)

fwrite(sub1,"sub2.csv") # 0.8334 LB Score


# create date variables
X_train <- train_new[,.(Datetime,Item_ID,Category_3,Category_2,Category_1,Price,Number_Of_Sales)]
X_test <- test[,.(Datetime,Item_ID,Category_3,Category_2,Category_1)]

X_train[, Item_ID := .N, Item_ID]
X_test[, Item_ID := .N, Item_ID]

X_train[,weekday := wday(Datetime)]
X_test[,weekday := wday(Datetime)]

X_train[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]
X_test[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]

X_train[,day := day(Datetime)]
X_test[,day := day(Datetime)]

training_data <- X_train[Datetime < "2016-01-01"]
validation_data <- X_train[Datetime >= "2016-01-01"]

# rm(X_train, X_test)
# gc()

library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(training_data$Price))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(validation_data$Price))
dtest <- xgb.DMatrix(data = as.matrix(X_test[,-c('Datetime'),with=F]))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsampleby_tree = 1
  #alpha = 1
  
)

bst1 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
)
#   valid-rmse:0.814935

pred1 <- predict(bst1, dtest)
pred1 <- exp(pred1)-1


dtrain <- xgb.DMatrix(data = as.matrix(training_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(training_data$Number_Of_Sales))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,-c('Price','Number_Of_Sales','Datetime'),with=F]), label = log1p(validation_data$Number_Of_Sales))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.01,
  max_depth = 8,
  min_child_weight = 10,
  subsample = 0.5,
  colsampleby_tree = 0.9
  #alpha = 1
  
)

bst2 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
)

pred2 <- predict(bst2, dtest)
pred2 <- exp(pred2)-1

sub <- data.table(ID = test$ID, Price = pred1, Number_Of_Sales = pred2)
head(sub)

fwrite(sub,"sub3.csv") # 0.86454 LB

























