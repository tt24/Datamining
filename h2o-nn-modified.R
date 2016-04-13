# Personal variables

setwd("/cs/home/sgs4/Documents/Datamining")
setwd("D:/Dropbox/St Andrews/IT/ID5059/Practical 2/")

# Start H2O and training


library(randomForest)
library(h2o) 
#library(data.table)
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

col_names <- names(train)[c(2:ncol(train))]

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

train$target<-as.factor(train$target) 


# Improved version

model <- h2o.deeplearning(x=2:113,
                          y=1,
                          training_frame = train,
                          #classification=1,
                          activation="RectifierWithDropout",
                          hidden_dropout_ratio=c(0.5,0.5),
                          input_dropout_ratio=0.0441629,
                          #hidden=c(61,31,16),
                          hidden=c(512,256),
                          epochs=100,
                          l1=0.000839377,
                          l2=0.0007871581,
                          rho=0.999,
                          epsilon=1e-8,
                          train_samples_per_iteration=-2,
                          stopping_metric = "logloss",
                          stopping_rounds = 5,
                          stopping_tolerance = 1e-3,
                          fast_mode = TRUE,
                          nfolds = 2,
                          #overwrite_with_best_model=F,
                          max_w2=10,
                          seed=13)

# see the model
summary(model)


hyper_params <- list(
  hidden=list(c(32,32,32),c(64,64)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params
grid <- h2o.grid(
  "deeplearning",
  model_id="dl_grid", 
  training_frame=train,
  validation_frame=test, 
  x=2:113, 
  y=1,
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)
grid

## Find the best model and its full set of parameters (clunky for now, will be improved)
scores <- cbind(as.data.frame(unlist((lapply(grid@model_ids, function(x) 
{ h2o.confusionMatrix(h2o.performance(h2o.getModel(x),valid=T))$Error[8] })) )), unlist(grid@model_ids))
names(scores) <- c("misclassification","model")
sorted_scores <- scores[order(scores$misclassification),]
head(sorted_scores)
best_model <- h2o.getModel(as.character(sorted_scores$model[1]))
print(best_model@allparameters)
best_err <- sorted_scores$misclassification[1]
print(best_err)

#######END TEST
# NEW TEST

sampled_train=train[1:10000,]

models <- c()
for (i in 1:10) {
  rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
  rand_numlayers <- sample(2:5,1)
  rand_hidden <- c(sample(10:50,rand_numlayers,T))
  rand_l1 <- runif(1, 0, 1e-3)
  rand_l2 <- runif(1, 0, 1e-3)
  rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
  rand_input_dropout <- runif(1, 0, 0.5)
  dlmodel <- h2o.deeplearning(
    model_id=paste0("dl_random_model_", i),
    training_frame=sampled_train,
    #validation_frame=test, 
    x=2:113, 
    y=1,
    nfold=2,
    epochs=100,                    ## for real parameters: set high enough to get to convergence
    #epochs=1,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
    #score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    
    ### Random parameters
    activation=rand_activation, 
    hidden=rand_hidden, 
    l1=rand_l1, 
    l2=rand_l2,
    input_dropout_ratio=rand_input_dropout, 
    hidden_dropout_ratios=rand_dropout
  )                                
  models <- c(models, dlmodel)
}

#find the best model
best_err <- 1      ##start with the best reference model from the grid search above, if available
for (i in 1:length(models)) {
  err <- h2o.confusionMatrix(h2o.performance(models[[i]],valid=F))$Error[8]
  if (err < best_err) {
    best_err <- err
    best_model <- models[[i]]
  }
}
h2o.confusionMatrix(best_model,valid=F)
best_params <- best_model@allparameters
best_params$hidden
best_params$l1
best_params$l2
best_params$input_dropout_ratio



## Using the DNN model for predictions
preds <- h2o.predict(model, test)


names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])




## Converting H2O format into data frame
preds <- as.data.frame(preds)

ID <- as.data.frame(ID)
out <- cbind(ID, "PredictedProb"=preds$predict)
write.csv(out, file = "answer_H2O-deepLearning-32.csv", row.names = F)
