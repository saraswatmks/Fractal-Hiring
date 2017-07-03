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

train <- train[Datetime >= "2015-01-01"]

train <- train[Number_Of_Sales <= 340000]

train[,summary(Datetime)]
test[,summary(Datetime)]

train[,weekday := wday(Datetime)]
test[,weekday := wday(Datetime)]

train[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]
test[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]

train[,xmonth := month(Datetime)]
test[,xmonth := month(Datetime)]

train[,xyear := year(Datetime)]
test[,xyear := year(Datetime)]



# all data
alldata <- rbindlist(list(train,test), fill=T)

alldata[,lag1 := shift(Price,1,fill = 0), .(xmonth,Item_ID)]
alldata[,lag2 := shift(Price, 2, fill=0),.(xmonth,Item_ID)]
alldata[,lag3 := shift(Price,3,fill = 0), .(xmonth,Item_ID)]
alldata[,lag4 := shift(Price, 4, fill=0),.(xmonth,Item_ID)]
alldata[,lag5 := shift(Price, 5,fill=0),.(xmonth,Item_ID)]
alldata[,lag6 := shift(Price, 6,fill=0),.(xmonth,Item_ID)]

x_train <- alldata[1:nrow(train)]
x_test <- alldata[(nrow(train)+1):nrow(alldata)]


# Train Data
cols_to_use <- setdiff(colnames(train), c('Price','Number_Of_Sales','Item_ID','Datetime','ID'))

training_data <- x_train[Datetime < "2016-01-01"]
validation_data <- x_train[Datetime >= "2016-01-01"]

dtrain <- xgb.DMatrix(data = as.matrix(training_data[,cols_to_use,with=F]), label = log1p(training_data$Price))
dvalid <- xgb.DMatrix(data = as.matrix(validation_data[,cols_to_use,with=F]), label = log1p(validation_data$Price))
dtest <- xgb.DMatrix(data = as.matrix(x_test[,cols_to_use,with=F]))


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
) #0.650030

imp <- xgb.importance(feature_names = colnames(dtrain), model = bst1)
xgb.plot.importance(imp)

bst1pred <- predict(bst1, dtest)
bst1pred <- exp(bst1pred)-1

dtrain2 <- xgb.DMatrix(data = as.matrix(training_data[,cols_to_use,with=F]), label = log1p(training_data$Number_Of_Sales))
dvalid2 <- xgb.DMatrix(data = as.matrix(validation_data[,cols_to_use,with=F]), label = log1p(validation_data$Number_Of_Sales))

params <- list(
  
  objective = 'reg:linear',
  eta = 0.025,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.8,
  colsampleby_tree = 1,
  alpha = 1
  
)

bst2 <- xgb.train(params = params
                  ,data = dtrain2
                  ,nrounds = 1000
                  ,watchlist = list(train = dtrain2, valid = dvalid2)
                  ,metric = 'rmse'
                  ,print_every_n = 10
                  ,early_stopping_rounds = 40
                  ,maximize = F
)

bst2pred <- predict(bst2, dtest)
bst2pred <- exp(bst2pred)-1

subX1 <- data.table(ID = test$ID, Price = bst1pred, Number_Of_Sales = bst2pred)
fwrite(subX1,"subwithlags.csv")






