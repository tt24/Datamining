# Personal variables

setwd("/cs/home/sgs4/Documents/Datamining")
setwd("D:/Dropbox/St Andrews/IT/ID5059/Practical 2/")

# Start H2O and training


library(randomForest)
library(h2o) 
h2o.init(nthreads=-1,max_mem_size = '8G') # Select the number of cores and RAM memory to use
train<-h2o.uploadFile("train.csv",
                      destination_frame = "train.hex")  ## loads file in parallel
test<-h2o.uploadFile("test.csv",
                     destination_frame = "test.hex")     ## loads file in parallel


# Cleans some useless columns and store the test set IDs
train$ID <- NULL
train$v22 <- NULL
ID <- test$ID
test$ID <- NULL
test$v22 <- NULL

# Takes away the categorical variables from the dataset
cate <- train[,c("v3", "v24", "v30", "v31", "v47", "v52", "v56","v66", "v71", "v74", "v75", "v79", "v91", "v107", "v110", "v112", "v113", "v125")]
drops <- c("v3", "v24", "v30", "v31", "v47", "v52", "v56","v66", "v71", "v74", "v75", "v79", "v91", "v107", "v110", "v112", "v113", "v125")
train1 <- train[ , !(names(train) %in% drops)]
test1 <- test[ , !(names(test) %in% drops)]

# Impute the NAs through simple median
train1 <- na.roughfix(train1)
test1 <- na.roughfix(test1)

# ================================================================================= #

train1$target<-as.factor(train1$target) 


model <- h2o.deeplearning(x = 2:113,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train1, # data in H2O format
                   nfolds = 5,
                   activation = "Tanh", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   fast_mode = TRUE,
                   hidden = c(1024,512,256), # three layers of 50 nodes
                   epochs = 200, # max. no. of epochs
                   max_w2=10) 

## Using the DNN model for predictions
preds <- h2o.predict(model, test1)

## Get the logloss
logloss <- h2o.logloss(model)

## Converting H2O format into data frame
preds <- as.data.frame(preds)

ID <- as.data.frame(ID)
out <- cbind(ID, "PredictedProb"=preds$p1)
write.csv(out, file = "answer_H2O-deepLearning-e100-bmodel.csv", row.names = F)
