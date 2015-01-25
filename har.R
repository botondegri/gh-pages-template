#
# TRAINING
#
training<-read.csv("pml-training.csv")

# leave out not needed input attributes
tr <- training[,8:159]
# convert factors to numbers
types <- sapply(tr,class)
f<-function(x) {x<-as.numeric(x)}
tr[,which(types != "numeric")]<-sapply(tr[,which(types != "numeric")],f)

# attributes which have count of is.na is greater then 70% of the cases left out of training sample
f<-function(x) { sum(is.na(x)) > length(x) *0.7}
leaveout <- sapply(tr,f)
tr <- tr[,-which(leaveout == T)]

# final training dataset devided to train and test 70% - 30%
set.seed(1)
ftr <- cbind(tr,training$classe)
names(ftr)[86] <- "classe"
tr_ind<-createDataPartition(ftr$classe,p=0.7,list=F)
ftr_tr <- ftr[tr_ind,]
ftr_te <- ftr[-tr_ind,]

# impute missing values with median
prep <- preProcess(ftr_tr[-86],method="medianImpute")
ftr_tr <- predict(prep,newdata=ftr_tr)
ftr_te <- predict(prep,newdata=ftr_te)

#train with crossvalidation default 10 folds
#and set classProbs to true for having the class probailities on the output

#library(fastICA)
#prep <- preProcess(tr,method="pca")
ctrl <- trainControl(method="cv",number=10,returnData = F,returnResamp = "all", classProbs = T) 
set.seed(1)

# try naive bayes classification 
#with assumption that sensors are measuring independent dimensions of the exercise
library(klaR)
#mod <- train(classe ~ ., data = ftr_tr, method = "nb", trControl = ctrl)
#mod <- train(classe ~ ., data = ftr_tr, method = "nb", trControl = ctrl, preProcess="ica", n.comp=10)
mod <- train(classe ~ ., data = ftr_tr, method = "rf")

plot(mod$finalModel)
ftr_te_resp <- predict(mod,newdata=ftr_te)
confusionMatrix(ftr_te_resp,ftr_te$classe)

#
# TESTING
#
# do preprocessing of testing data
# leave out not needed input attributes
testing<-read.csv("pml-testing.csv")
te <- testing[,8:159]

# convert factors (csv reading behaviour) to numbers
types <- sapply(te,class)

f<-function(x) {x<-as.numeric(x)}
te[,which(types != "numeric")]<-sapply(te[,which(types != "numeric")],f)

te <- te[,-which(leaveout == T)]

te2 <- predict(prep,newdata=te)
te_resp <- predict(mod,newdata=te2,type="prob")

