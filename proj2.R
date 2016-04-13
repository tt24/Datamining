dim(train)
str(train)
summary(train)
dim(test)
str(test)

#no duplicates
length(unique(train$ID))
length(unique(test$ID))

#1=suitable for accelerated approval
table(train$target)
prop.table(table(train$target))

#replace blanks with NA for categorical variables
levels(train$v3)[1] <- NA
levels(train$v22)[1] <- NA
levels(train$v30)[1] <- NA
levels(train$v31)[1] <- NA
levels(train$v52)[1] <- NA
levels(train$v56)[1] <- NA
levels(train$v91)[1] <- NA
levels(train$v107)[1] <- NA
levels(train$v112)[1] <- NA
levels(train$v113)[1] <- NA
levels(train$v125)[1] <- NA
levels(test$v3)[1] <- NA
levels(test$v22)[1] <- NA
levels(test$v30)[1] <- NA
levels(test$v31)[1] <- NA
levels(test$v52)[1] <- NA
levels(test$v56)[1] <- NA
levels(test$v91)[1] <- NA
levels(test$v107)[1] <- NA
levels(test$v112)[1] <- NA
levels(test$v113)[1] <- NA
levels(test$v125)[1] <- NA

#variables without NA
dim(na.omit(train))

#number of NA in each variable
Count <- apply(train, 2, function(x){sum(is.na(x))})
Percentage <- apply(train, 2, function(x){sum(is.na(x))/length(x)})
missingsvariables <- cbind(Count, Percentage)

#NA depending on target
target0 <- train[train$target==0,]
target1 <- train[train$target==1,]
Count_0 <- apply(target0, 2, function(x){sum(is.na(x))})
Percentage_0 <- apply(target0, 2, function(x){sum(is.na(x))/length(x)})
Count_1 <- apply(target1, 2, function(x){sum(is.na(x))})
Percentage_1 <- apply(target1, 2, function(x){sum(is.na(x))/length(x)})
missingvariables <- cbind(Count_0, Percentage_0, Count_1, Percentage_1)

#continuous variables
cont.var.names <- c(paste("v", c(1, 2, 4:21, 23, 25:29, 32:46, 48:51, 53:55, 57:65, 67:70, 72, 73, 76:78, 80:90, 92:106, 108, 109, 111, 114:124, 126:131), sep=""))
summary(train[cont.var.names])

target0 <- train[which(train$target==0),]
target1 <- train[which(train$target==1),]
for (i in 1:length(train[cont.var.names])) {
  par(mfrow=c(1,2))
  hist(train[cont.var.names][[i]], breaks=100, main="Histogram", xlab=colnames(train[cont.var.names][i]))
  hist(target0[cont.var.names][[i]], breaks=100, main="Histogram target 0", xlab=colnames(target0[cont.var.names][i]))
  hist(target1[cont.var.names][[i]], breaks=100, main="Histogram target 1", xlab=colnames(target1[cont.var.names][i]))
  
}

#categorical variables
cat.var.names <- c(paste("v", c(3, 22, 24, 30, 31, 47, 52, 56, 66, 71, 74, 75, 79, 91, 107, 110, 112, 113, 125), sep=""))
summary(train[cat.var.names])
apply(train[cat.var.names], 2, function(x){length(unique(x))} )
table(train$v91, train$v107) #the same
train$v3<-as.integer(train$v3)
train$v22<-as.integer(train$v22)
train$v24<-as.integer(train$v24)
train$v30<-as.integer(train$v30)
train$v31<-as.integer(train$v31)
train$v47<-as.integer(train$v47)
train$v52<-as.integer(train$v52)
train$v56<-as.integer(train$v56)
train$v66<-as.integer(train$v66)
train$v71<-as.integer(train$v71)
train$v74<-as.integer(train$v74)
train$v75<-as.integer(train$v75)
train$v79<-as.integer(train$v79)
train$v91<-as.integer(train$v91)
train$v107<-as.integer(train$v107)
train$v110<-as.integer(train$v110)
train$v112<-as.integer(train$v112)
train$v113<-as.integer(train$v113)
train$v125<-as.integer(train$v125)
test$v3<-as.integer(test$v3)
test$v22<-as.integer(test$v22)
test$v24<-as.integer(test$v24)
test$v30<-as.integer(test$v30)
test$v31<-as.integer(test$v31)
test$v47<-as.integer(test$v47)
test$v52<-as.integer(test$v52)
test$v56<-as.integer(test$v56)
test$v66<-as.integer(test$v66)
test$v71<-as.integer(test$v71)
test$v74<-as.integer(test$v74)
test$v75<-as.integer(test$v75)
test$v79<-as.integer(test$v79)
test$v91<-as.integer(test$v91)
test$v107<-as.integer(test$v107)
test$v110<-as.integer(test$v110)
test$v112<-as.integer(test$v112)
test$v113<-as.integer(test$v113)
test$v125<-as.integer(test$v125)


#visualize the categorical variables with less than 10 categories and per category visualize the percentage of target == 1
barplot(table(train$target, train$v3), main="v3")
plot(prop.table(table(train$v3, train$target), 1), main="v3")
barplot(table(train$target, train$v24), main="v24")
plot(prop.table(table(train$v24, train$target), 1), main="v24")
barplot(table(train$target, train$v30), main="v30")
plot(prop.table(table(train$v30, train$target), 1), main="v30")
barplot(table(train$target, train$v31), main="v31")
plot(prop.table(table(train$v31, train$target), 1), main="v31")
barplot(table(train$target, train$v47), main="v47")
plot(prop.table(table(train$v47, train$target), 1), main="v47")
barplot(table(train$target, train$v52), main="v52")
plot(prop.table(table(train$v52, train$target), 1), main="v52")
barplot(table(train$target, train$v66), main="v66")
plot(prop.table(table(train$v66, train$target), 1), main="v66")
barplot(table(train$target, train$v71), main="v71")
plot(prop.table(table(train$v71, train$target), 1), main="v71")
barplot(table(train$target, train$v74), main="v74")
plot(prop.table(table(train$v74, train$target), 1), main="v74")
barplot(table(train$target, train$v75), main="v75")
plot(prop.table(table(train$v75, train$target), 1), main="v75")
barplot(table(train$target, train$v91), main="v91")
plot(prop.table(table(train$v91, train$target), 1), main="v91")
barplot(table(train$target, train$v107), main="v107")
plot(prop.table(table(train$v107, train$target), 1), main="v107")
barplot(table(train$target, train$v110), main="v110")
plot(prop.table(table(train$v110, train$target), 1), main="v110")


# read file and convert factor to int
library(readr)
for (f in names(train)) {
  if (class(train[[f]])=="factor") {
    levels <- unique(train[[f]])
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
  }
}

# make a table of missing values
library(mice)
missers <- md.pattern(train[, -c(1:2)])
View(missers)

# plot missing values
library(VIM)
miceplot <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                 numbers=TRUE, combined=TRUE, border="gray50",
                 sortVars=TRUE, ylabs=c("Missing Data Pattern"),
                 labels=names(train[-c(1:2)]), cex.axis=.7,
                 gap=3)



#replace all NA
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for (var in 1:ncol(train)) {
  if (class(train[,var]) %in% c("numeric","integer")) {
    train[is.na(train[,var]),var] <- mean(train[,var], na.rm = TRUE)
  } else if (class(train[,var]) %in% c("character", "factor")) {
    train[is.na(train[,var]),var] <- Mode(train[,var], na.rm = TRUE)
  }
}
for (var in 1:ncol(test)) {
  if (class(test[,var]) %in% c("numeric","integer")) {
    test[is.na(test[,var]),var] <- mean(test[,var], na.rm = TRUE)
  } else if (class(test[,var]) %in% c("character", "factor")) {
    test[is.na(test[,var]),var] <- Mode(test[,var], na.rm = TRUE)
  }
}

#PCA
pca_cor<-princomp(train[,3:133], cor=T)
summary(pca_cor)
loadings(pca_cor)
eigv<-pca_cor$sdev^2
eigv
sum(eigv)/131
screeplot(pca_cor, type="lines",main="Scree Plot")
screeplot(pca_cor, ncp=10,main="Scree Plot",xlab="Components")


#logistic regression with the continuous variables
glm.fit.int<-glm(target~v1+v2+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+v21+v23+v25+v26+v27+v28+v29+v32+v33+v34+v35+v36+v37+v38+v39+v40+v41+v42+v43+v44+v45+v46+v48+v49+v50+v51+v53+v54+v55+v57+v58+v59+v60+v61+v62+v63+v64+v65+v67+v68+v69+v70+v72+v73+v76+v77+v78+v80+v81+v82+v83+v84+v85+v86+v87+v88+v89+v90+v92+v93+v94+v95+v96+v97+v98+v99+v100+v101+v102+v103+v104+v105+v106+v108+v109+v111+v114+v115+v116+v117+v118+v119+v120+v121+v122+v123+v124+v126+v127+v128+v129+v130+v131,data=train,family=binomial)
summary (glm.fit.int)
glm.fit.int1<-glm(target~v6+v8+v10+v12+v14+v50+v62+v70+v72+v92+v93+v95+v98+v105+v114+v130,data=train,family=binomial)
summary (glm.fit.int1)
glm.fit.int2<-glm(target~v6+v10+v12+v14+v50+v62+v70+v72+v93+v98+v114,data=train,family=binomial)
summary (glm.fit.int2)
glm.fit.int3<-glm(target~v10+v12+v14+v50+v62+v72+v93+v98+v114,data=train,family=binomial)
summary (glm.fit.int3)

#logistic regression with the categorical variables
glm.fit.cat<-glm(target~v3+v22+v24+v30+v31+v47+v52+v56+v66+v71+v74+v75+v79+v91+v107+v110+v112+v113+v125,data=train,family=binomial)
summary (glm.fit.cat)
glm.fit.cat1<-glm(target~v3+v30+v31+v56+v66+v71+v74+v75+v79+v110+v112,data=train,family=binomial)
summary (glm.fit.cat1)

#logistic regression with all the variables
glm.fitall<-glm(target~v3+v22+v24+v30+v31+v47+v52+v56+v66+v71+v74+v75+v79+v91+v107+v110+v112+v113+v125+v1+v2+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+v21+v23+v25+v26+v27+v28+v29+v32+v33+v34+v35+v36+v37+v38+v39+v40+v41+v42+v43+v44+v45+v46+v48+v49+v50+v51+v53+v54+v55+v57+v58+v59+v60+v61+v62+v63+v64+v65+v67+v68+v69+v70+v72+v73+v76+v77+v78+v80+v81+v82+v83+v84+v85+v86+v87+v88+v89+v90+v92+v93+v94+v95+v96+v97+v98+v99+v100+v101+v102+v103+v104+v105+v106+v108+v109+v111+v114+v115+v116+v117+v118+v119+v120+v121+v122+v123+v124+v126+v127+v128+v129+v130+v131,data=train,family=binomial)
summary (glm.fitall)
glm.fitall1<-glm(target~v24+v31+v47+v56+v66+v74+v79+v110+v112+v6+v8+v10+v11+v12+v38+v43+v50+v62+v70+v72+v92+v93+v95+v98+v114+v130,data=train,family=binomial)
summary (glm.fitall1)
glm.fitall2<-glm(target~v24+v31+v47+v56+v66+v74+v79+v110+v112+v6+v10+v12+v38+v50+v62+v70+v72+v98+v114,data=train,family=binomial)
summary (glm.fitall2)

AIC(glm.fitall)  #best one
glm.probs<-predict(glm.fitall,type="response")
glm.pred<-rep("Down",114321)
glm.pred[glm.probs==0]="Up"
table(glm.pred,train$target)

#logistic regression according to PCA
glm.pca.var<-glm(target~v3+v1+v2+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16,data=train,family=binomial)
glm.pca.eig<-glm(target~v3+v1+v2+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v24+v17+v18+v19+v20+v21+v23+v25+v26+v27+v28+v29,data=train,family=binomial)
glm.pca.plot<-glm(target~v3+v1+v2,data=train,family=binomial)


#classification tree
library(tree)
tree1<-tree(target~.,train)
summary(tree1)
plot(tree1)
text(tree1,pretty =0)
tree.pred<-predict(tree1,type="class")
table(tree.pred)



#boosting tree
library(gbm)
boost<-gbm(target~.,data=train,distribution="gaussian",n.trees=500,interaction.depth=4)
summary(boost)
plot(boost,i="v50")
plot(boost,i="v47")
plot(boost,i="v66")
plot(boost,i="v31")
yhat.boost<-predict(boost,data=train,n.trees=5)
