rm(list = ls())

path <- "/home/manish/Desktop/Data2017/July/fractal/"
setwd(path)

library(data.table)

train <- fread("train.csv")
test <- fread("test.csv")

library(lubridate)

train[,Datetime := ymd(Datetime)]
test[,Datetime := ymd(Datetime)]

library(DT)
datatable(head(train))

train[,weekday := wday(Datetime)]
test[,weekday := wday(Datetime)]

train[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]
test[,weekyear := as.numeric(format(as.Date(Datetime),"%W"))]

train[,day := day(Datetime)]
test[,day := day(Datetime)]

train[,xmonth := month(Datetime)]
test[,xmonth := month(Datetime)]

train[,xyear := year(Datetime)]
test[,xyear := year(Datetime)]

datatable(head(train))

X_panel <- rbindlist(list(train,test),fill = T)
X_panel[,summary(Datetime)]
X_panel[,c('Datetime','Category_3', 'Category_1','Category_2','day') := NULL]
X_panel[,ID := NULL]
# X_train <- train[,.(Item_ID, weekday, weekyear, xmonth, year)]
# X_test <- test[,.(Item_ID, weekday, weekyear, xmonth, year)]

alldata <- rbindlist(list(train,test),fill = T)


# # add one year, one month
# my_panel <- copy(X_panel)
# my_panel[,xmonth := NULL]
# my_panel[,xyear := xyear + 1]
# #my_panel[,xmonth := xmonth +1]
# setnames(my_panel,c('Price','Number_Of_Sales'), c('prev_year_price','prev_year_sales'))
# alldata <- my_panel[alldata, on=c('xyear','Item_ID','weekday','weekyear')]
# rm(my_panel)

# add year, weekyear
my_panel <- copy(X_panel)
my_panel[,xyear := NULL]
my_panel[,xmonth := xmonth + 1]
my_panel[,weekyear := weekyear + 1]
setnames(my_panel,c('Price','Number_Of_Sales'),c('prev_weekyear_price','prev_weekyear_sales'))
alldata <- my_panel[alldata, on=c('xmonth','Item_ID','weekday','weekyear')]
rm(my_panel)

# add year, 2 week years
my_panel <- copy(X_panel)
my_panel[,xyear := xyear + 1]
my_panel[,weekyear := weekyear + 2]
setnames(my_panel,c('Price','Number_Of_Sales'),c('prev_twoweekyear_price','prev_twoweekyear_sales'))
alldata <- my_panel[alldata, on=c('xyear','Item_ID','Category_1','Category_2','weekday','weekyear','xmonth')]
rm(my_panel)

# add year, second next week
my_panel <- copy(X_panel)
my_panel[,xyear := xyear + 1]
my_panel[,weekyear := weekyear - 2]
setnames(my_panel,c('Price','Number_Of_Sales'),c('prev_nextweekyear_price','prev_nextweekyear_sales'))
alldata <- my_panel[alldata, on=c('xyear','Item_ID','Category_1','Category_2','weekday','weekyear','xmonth')]
rm(my_panel)

#add year , next week
my_panel <- copy(X_panel)
my_panel[,xyear := xyear + 1]
my_panel[,weekyear := weekyear - 1]
setnames(my_panel,c('Price','Number_Of_Sales'),c('prev_year_nextweekyear_price','prev_year_nextweekyear_sales'))
alldata <- my_panel[alldata, on=c('xyear','Item_ID','Category_1','Category_2','weekday','weekyear','xmonth')]
rm(my_panel)
rm(X_panel)

# split the data in train and test
X_train <- alldata[1:nrow(train)]
X_test <- alldata[(nrow(train)+1):nrow(alldata)]

head(X_train)


















