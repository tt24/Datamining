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

train[is.na(train)]   <- 999
test[is.na(test)]   <- 999

library(MASS)

train$ID <- NULL
test$ID<-NULL

select(lm.ridge(target~., data=train,lambda=seq(0,0.5,0.001)))

ridge = lm.ridge(target~., data=train,lambda=0.026)


test1<-data.frame(1,test)
a<-as.matrix(test1)%*%coef(ridge)
write.csv(a, file = "newdata.csv", row.names = FALSE)
                 
