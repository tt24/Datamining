library(magrittr)
library(data.table)
library(xgboost)
library(methods)
library(randomForest)
library(ggplot2)
library(Ckmeans.1d.dp)
library(DiagrammeR)

setwd("D:/Dropbox/St Andrews/IT/ID5059/Practical 2/")

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

# END OF MODEL #
##################################################
# START MODEL WITHOUT 10 MOST IMPORTANT FEATURES #
#                                                #
#  "BAD MODEL ESTIMATIONS" to prove our point    #
#                                                #
#drops <- c("v50","v66","v47","v31","v24","v40","v56","v21","v114","v12")
#train_bad <- train[]
#train_bad <- as.data.frame(train[,!(names(train) %in% drops)])

#train_bad[,c("v50", "v66", "v47", "v31")] <- NULL
train_bad <- as.data.frame(train[])

train_bad$v50 <- NULL
train_bad$v66 <- NULL
train_bad$v47 <- NULL
train_bad$v31 <- NULL
train_bad$v24 <- NULL
train_bad$v40 <- NULL
train_bad$v56 <- NULL
train_bad$v21 <- NULL
train_bad$v114 <- NULL
train_bad$v12 <- NULL

test_bad <- as.data.frame(test[])

test_bad$v50 <- NULL
test_bad$v66 <- NULL
test_bad$v47 <- NULL
test_bad$v31 <- NULL
test_bad$v24 <- NULL
test_bad$v40 <- NULL
test_bad$v56 <- NULL
test_bad$v21 <- NULL
test_bad$v114 <- NULL
test_bad$v12 <- NULL

test_bad <- as.data.table(test_bad[])
train_bad <- as.data.table(train_bad[])

# Make a new model with the missing columns 
bst_bad = produce_model(train_bad, test_bad, target, "answer_xgb_missing_values.csv") 

# Plot the differences
#xgb.plot.tree(model = bst_bad)

########## END BAD MODEL ESTIMATIONS



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


