library(magrittr)
library(data.table)
library(xgboost)
library(methods)
library(randomForest)
library(ggplot2)
library(Ckmeans.1d.dp)
library(DiagrammeR)

set.seed(7)
train <- fread('train.csv', header = T, stringsAsFactors = T)
test <- fread('test.csv', header=TRUE, stringsAsFactors = T)
ID <- test$ID

# Delete ID column in training dataset
train[, ID := NULL]

# Delete ID column in testing dataset
test[, ID := NULL]

col_names <- names(train)[c(3:ncol(train))]

# substitute categorical variables with integers
for (col in col_names) {
  if (class(train[[col]])=="factor") {
    levels <- unique(c(train[[col]], test[[col]]))
    train[[col]] <- as.integer(train[[col]])
    test[[col]]  <- as.integer(test[[col]])
  }
}

# Impute the NAs through simple median
train <- na.roughfix(train)
test <- na.roughfix(test)

# Save the name of the last column
name_target <- names(train)[1]

target <- train[, name_target, with = F][[1]]

train[, name_target:=NULL, with = F]

produce_model = function(train, test, target, file_name) {
	trainMatrix <- train[,lapply(.SD,as.numeric)] %>% as.matrix
	testMatrix <- test[,lapply(.SD,as.numeric)] %>% as.matrix


	param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss")

	cv.nround <- 5
	cv.nfold <- 3

	bst.cv = xgb.cv(param=param, data = trainMatrix, label = target, 
                nfold = cv.nfold, nrounds = cv.nround)
	nround = 50
	bst = xgboost(param=param, data = trainMatrix, label = target, nrounds=nround)
	pred <- predict(bst, data.matrix(test))

	pred <- as.data.frame(pred)
	ID <- as.data.frame(ID)
	out <- cbind(ID, "PredictedProb"=pred$pred)
	write.csv(out, file = file_name, row.names = F)

	return(bst)
}

bst = produce_model(train, test, target, "answer_xgb_imp.csv") 

names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


# new predictions with importnant features only
most_important_features = importance_matrix$Feature[1:10]

train1<-train[,most_important_features, with = F]
test1<-test[,most_important_features, with = F]
bst1 = produce_model(train1, test1, target, "answer_xgb_imp1.csv") 


highCorrRemovals <- c("v8","v23","v25","v36","v37","v46",
                      "v51","v53","v54","v63","v73","v81",
                      "v82","v89","v92","v95","v105","v107",
                      "v108","v109","v116","v117","v118",
                      "v119","v123","v124","v128")

train2 <- train[,-which(names(train) %in% highCorrRemovals), with=F]
test2 <- test[,-which(names(test) %in% highCorrRemovals), with=F]
bst2 = produce_model(train2, test2, target, "answer_xgb_imp2.csv")


