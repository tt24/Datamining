install.packages("lme4")
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-5.tar.gz"
url <- "http://cran.stat.ucla.edu/src/contrib/Ckmeans.1d.dp_3.3.1.tar.gz"
install.packages(url, repos=NULL, type="source")
install.packages("caret")
library("xgboost")
library("readr")
library("stringr")
library("caret")
library("car")
train = read.table("train.csv", header=T, sep=",")
test = read.table("test.csv", header=T, sep=",")

set.seed(100)

col_names <- names(train)[c(3:ncol(train))]


for (col in col_names) {
  if (class(train[[col]])=="factor") {
    levels <- unique(c(train[[col]], test[[col]]))
    train[[col]] <- as.integer(train[[col]])
    test[[col]]  <- as.integer(test[[col]])
  }
}

train[is.na(train)]   <- -999
test[is.na(test)]   <- -999


h <- sample(nrow(train),1000)

#all = rbind(train[,-2],test)
train_no_target = train[-2]

dval<-xgb.DMatrix(data=data.matrix(train_no_target[h,]),label=train$target[h])
dtrain<-xgb.DMatrix(data=data.matrix(train_no_target[-h,]),label=train$target[-h])

watchlist<-list(val=dval,train=dtrain)

xgb <- xgboost(data = dtrain, 
               eta = 0.01,
               max_depth = 11, 
               nround=25, # CHANGE TO >1500
               subsample = 0.96,
               colsample_bytree = 0.45,
               eval_metric = "logloss",
               objective = "binary:logistic",
               min_child_weight = 1,
               num_parallel_tree = 1, 
               verbose             = 1,
               early.stop.round    = 300,
               watchlist            = watchlist,
               maximize            = FALSE
)

cat("Calculate predictions\n")
pred1 <- predict(xgb,
                 data.matrix(test))

model <- xgb.dump(xgb, with.stats = T)
model[1:10]
names <- dimnames(data.matrix(train))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])
best_score = xgb$bestScore


most_important_features = importance_matrix$Feature[1:3]
train1<-train[,most_important_features]
train1$target = train$target
test1<-test[,most_important_features]

h <- sample(nrow(train1),1000)

#all = rbind(train[,-2],test)


dval1<-xgb.DMatrix(data=data.matrix(train1[h,]),label=train1$target[h])
dtrain1<-xgb.DMatrix(data=data.matrix(train1[-h,]),label=train1$target[-h])

watchlist1<-list(val=dval1,train=dtrain1)

xgb1 <- xgboost(data = dtrain1, 
               eta = 0.01,
               max_depth = 11, 
               nround=25, # CHANGE TO >1500
               subsample = 0.96,
               colsample_bytree = 0.45,
               eval_metric = "logloss",
               objective = "binary:logistic",
               min_child_weight = 1,
               num_parallel_tree = 1, 
               verbose             = 1,  #1
               early.stop.round    = 300,
               watchlist            = watchlist1,
               maximize            = FALSE
)

cat("Calculate predictions\n")
pred2 <- predict(xgb1,
                 data.matrix(test1))
xgb1$bestScore
