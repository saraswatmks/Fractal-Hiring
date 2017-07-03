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

train[,weekday := wday(Datetime)]
test[,weekday := wday(Datetime)]

train[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]
test[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]

train[,day := day(Datetime)]
test[,day := day(Datetime)]

train[,xmonth := month(Datetime)]
test[,xmonth := month(Datetime)]


library(ggplot2)

ggplot(train, aes(Price))+geom_histogram(bins = 200,fill='red',color='black')
ggplot(train, aes(Number_Of_Sales))+geom_histogram(bins = 200,fill='red',color='black')
ggplot(train, aes(Number_Of_Sales))+geom_density()+ scale_x_log10()

ggplot(train, aes(Number_Of_Sales, Price))+geom_point() #outlier spotted - need to remove it

ggplot(train,aes(Category_1, Price))+stat_summary(fun.y = 'mean', geom = 'point')
ggplot(train,aes(Category_2, Price))+stat_summary(fun.y = 'mean', geom = 'point')
ggplot(train,aes(Category_3, Price))+stat_summary(fun.y = 'mean', geom = 'point')

ggplot(train,aes(Price))+geom_histogram(bins = 200,fill='red',color='black')+facet_grid(~Category_2)

ggplot(train,aes(Category_1, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'point')
ggplot(train,aes(Category_2, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'point')
ggplot(train,aes(Category_3, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'point')

ggplot(train,aes(weekday, Price))+stat_summary(fun.y = 'mean', geom = 'line')
ggplot(train,aes(weekyear, Price))+stat_summary(fun.y = 'mean', geom = 'line')+
  scale_x_continuous(breaks = seq(1,52,1))

ggplot(train,aes(day, Price))+stat_summary(fun.y = 'mean', geom = 'line')
ggplot(train,aes(xmonth, Price))+stat_summary(fun.y = 'mean', geom = 'line')

ggplot(train,aes(weekday, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'line')
ggplot(train,aes(weekyear, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'line')+
  scale_x_continuous(breaks = seq(1,52,1))

ggplot(train,aes(day, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'line')

ggplot(train,aes(xmonth, Number_Of_Sales))+stat_summary(fun.y = 'mean', geom = 'line')




# Predicting Price --------------------------------------------------------

# Remove Outlier

train[,summary(Price)]
quantile(x = train$Price, probs = seq(0,1,0.001))

train <- train[Price < 120] # with this we keep 99.% data
ggplot(train, aes(Price))+geom_density()

train[,summary(Number_Of_Sales)]
quantile(x = train$Number_Of_Sales, probs = seq(0,1,0.001))

train <- train[Number_Of_Sales < 11761]

# remove item IDs not in test

item_remove_train <- train$Item_ID[which(!(train$Item_ID %in% test$Item_ID))]
train <- train[!(Item_ID %in% item_remove_train)]


# Create Features ---------------------------------------------------------

med_price <- train[,median(Price), Item_ID]
setnames(med_price,"V1","medID")

med_sales <- train[,median(Number_Of_Sales),Item_ID]
setnames(med_sales,"V1","medSales")

catone_med_price <- train[,median(Price),Category_1]
setnames(catone_med_price,"V1","medCatOnePrice")

catone_med_sales <- train[,median(Number_Of_Sales),Category_1]
setnames(catone_med_sales,"V1","medCatOneSales")

cattwo_med_price <- train[,median(Price),Category_2]
setnames(cattwo_med_price,"V1","medCatTwoPrice")

cattwo_med_sales <- train[,median(Number_Of_Sales),Category_2]
setnames(cattwo_med_sales,"V1","medCatTwoSales")

train <- med_price[train, on='Item_ID']
train <- med_sales[train, on='Item_ID']
train <- catone_med_price[train, on='Category_1']
train <- catone_med_sales[train, on='Category_1']
train <- cattwo_med_price[train, on='Category_2']
train <- cattwo_med_sales[train, on='Category_2']

test <- med_price[test, on='Item_ID']
test <- med_sales[test, on='Item_ID']
test <- catone_med_price[test, on='Category_1']
test <- catone_med_sales[test, on='Category_1']
test <- cattwo_med_price[test, on='Category_2']
test <- cattwo_med_sales[test, on='Category_2']

train[,item_count := .N, Item_ID]
test[,item_count := .N, Item_ID]

train[,is_weekend := 0]
train[weekday %in% c(6,7), is_weekend := 1]
test[weekday %in% c(6,7), is_weekend := 1]

test[,is_weekend := 0]
test[weekday %in% c(6,7), is_weekend := 1]
test[weekday %in% c(6,7), is_weekend := 1]

train[,.N,is_weekend]

rm(catone_med_price,catone_med_sales,cattwo_med_price,cattwo_med_sales,med_price,med_sales)


# Log Transform Features --------------------------------------------------

numeric_cols <- colnames(train)[sapply(train, is.numeric)]
numeric_cols <- setdiff(numeric_cols, c('is_weekend','xmonth','day','weekyear','weekday',"Item_ID","Category_1","Category_2","Category_3","item_count"))
numeric_cols

for( x in numeric_cols)
  set(x = train, j = x, value = log1p(train[[x]]))

numeric_cols <- colnames(test)[sapply(test, is.numeric)]

for( x in numeric_cols)
  set(x = test, j = x, value = log1p(test[[x]]))



# Regression Model --------------------------------------------------------

train[,ID := NULL]
cols_to_use <- setdiff(colnames(train), c('Price','Number_Of_Sales','Item_ID','Datetime'))

training_data <- train[Datetime < "2016-01-01"]
validation_data <- train[Datetime >= "2016-01-01"]

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,cols_to_use,with=F]), label = training_data$Price)
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,cols_to_use,with=F]), label = validation_data$Price)
dtest <- xgb.DMatrix(data = as.matrix(test[,cols_to_use,with=F]))


params <- list(
  
  objective = 'reg:linear',
  eta = 0.025,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.8,
  colsampleby_tree = 1,
  alpha = 1
  
)

bst1 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
) # valid-rmse:0.305127
 
imp <- xgb.importance(feature_names = colnames(dtrain), model = bst1)
xgb.plot.importance(imp)

pred1 <- predict(bst1, dtest)
pred1 <- exp(pred1)-1

pred3 <- predict(bst1, dtest)
pred3 <- exp(pred3)-1



# Run a model with top 8 features ----------------------------------------

top8featsPrice <- imp$Feature[1:8]

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,top8feats,with=F]), label = training_data$Price)
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,top8feats,with=F]), label = validation_data$Price)
dtest <- xgb.DMatrix(data = as.matrix(test[,top8feats,with=F]))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsampleby_tree = 1,
  alpha = 1
  
)

bst1 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
) # valid-rmse:0.304098

pred5 <- predict(bst1, dtest)
pred5 <- exp(pred5)-1



# Predicting Number of Sales ----------------------------------------------

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,cols_to_use,with=F]), label = training_data$Number_Of_Sales)
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,cols_to_use,with=F]), label = validation_data$Number_Of_Sales)
dtest <- xgb.DMatrix(data = as.matrix(test[,cols_to_use,with=F]))


params <- list(
  
  objective = 'reg:linear',
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 10,
  subsample = 0.7,
  colsampleby_tree = 0.8,
  alpha = 10
  
)

bst1 <- xgb.train(params = params
                  ,data = dtrain
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain, valid = dvalid)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
) #0.776170 (Default) | #0.771921 (After tuning)

imp <- xgb.importance(feature_names = colnames(dtrain), model = bst1)
xgb.plot.importance(imp)

#default
pred2 <- predict(bst1, dtest)
pred2 <- exp(pred2)-1

#tuned
pred4 <- predict(bst1, dtest)
pred4 <- exp(pred4)-1


submit <- data.table(ID = test$ID, Price = pred1, Number_Of_Sales = pred2)
fwrite(submit, "sub4.csv") #0.72102

submit <- data.table(ID = test$ID, Price = pred3, Number_Of_Sales = pred4)
fwrite(submit, "sub5.csv")

# Run another model with top8 features -----------------------------------

top8featsSales <- imp$Feature[1:8]

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,top8feats,with=F]), label = training_data$Number_Of_Sales)
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,top8feats,with=F]), label = validation_data$Number_Of_Sales)
dtest <- xgb.DMatrix(data = as.matrix(test[,top8feats,with=F]))


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
) #0.776170 tuned | 0.7796 top8

imp <- xgb.importance(feature_names = colnames(dtrain), model = bst1)
xgb.plot.importance(imp)

pred2 <- predict(bst1, dtest)
pred2 <- exp(pred2)-1

pred6 <- predict(bst1, dtest)
pred6 <- exp(pred6)-1

submit <- data.table(ID = test$ID, Price = pred5, Number_Of_Sales = pred6)
fwrite(submit,"sub6.csv")





# Run random forest once --------------------------------------------------

train_rf <- train[,c(top8featsPrice,"Price"),with=F]
test_rf <- test[,c(top8featsPrice),with=F]

colSums(is.na(train_rf))
colSums(is.na(test_rf))

train_rf[is.na(train_rf)] <- -1
test_rf[is.na(test_rf)] <- -1

library(randomForest)
model1 <- randomForest(Price ~ ., data = train_rf, ntree = 500, nodesize = 11)










