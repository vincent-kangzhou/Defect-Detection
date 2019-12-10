## remove all the mixed value
aa=which(dat$RQ=="M")
dat1=dat[-aa,]
bb=which(dat1$THL=="TL"|dat1$THL=="TH"|dat1$THL=="M")
dat2=dat1[-bb,]


dat=dat2
rm(dat1,dat2)

dat$binary.defect=ifelse(dat$Defect_Number>0,1,0)
dat$Grinding_B=ifelse(dat$Grinding==0,0,1)
dat$Grinding_B=as.character(dat$Grinding_B)
class(dat$Curve_Tangent)
dat$Track.class=ifelse(dat$Speed<=25, "1_2","3_4_5")

dat$Track.class=as.character(dat$Track.class)



position.f=unique(dat$Curve_Tangent)
grinding.f=unique(dat$Grinding_B)
speed.f=unique(dat$Track.class)

#### load package
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)

dat=within(dat,{
  RQ=factor(RQ)
  Prefix=factor(Prefix)
  Year=factor(Year)
  
})

opt.cut=function(prf,pred.basic){
  cut.ind=mapply(FUN = function(x,y,p){
    d=(x-0)^2+(y-1)^2
    ind=which(d==min(d))
    c(sensitivity=y[[ind]], specificity=1-x[[ind]],cutoff=p[[ind]])
  }, prf@x.values,prf@y.values,pred.basic@cutoffs)
}

goodness=data.frame(Total=NA, Defect=NA, Percentage=NA, ROC.area=NA, sensitivity=NA, specificity=NA,cutoff=NA)
require(data.table)
require(xlsx)
library(ROCR)
library(formattable)
pdf(file = "tree-based mixed-effect ROC curve.pdf")
for (i in 3: 3){
    for (j in 2: length(grinding.f)){
      for (k in 1: length(speed.f)){
        subdata=subset(dat,Curve_Tangent==position.f[i]  & Grinding_B==grinding.f[j] & Track.class==speed.f[k])
      aa=paste("a",i,j,k,sep = "")
      assign(aa,subdata)
      

     mm=glmer(binary.defect~Age+Prior_Defect+Geometry_Defects+VTI+Turnout+Curve_D+Traffic_Density+Grade+ factor(Year) +
                   Ballast+Rail_Size + RQ +(1|Prefix/Year),
                 data = subdata, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=3e5)),nAGQ = 1)
      
      subdata$Fit.prob=fitted(mm)
      mm_coeff=coef(summary(mm))
      pred=prediction(subdata$Fit.prob,subdata$binary.defect)
      prf=performance(pred, "tpr","fpr")
      auc=performance(pred,measure = "auc")
     goodness$Total[1]=nrow(subdata)
     goodness$Defect[1]=sum(subdata$binary.defect)
     goodness$Percentage[1]=percent(goodness$Defect[1]/goodness$Total[1])
     goodness$ROC.area=auc@y.values
     goodness$sensitivity[1]=opt.cut(prf,pred)[1]
     goodness$specificity[1]=opt.cut(prf,pred)[2]
     goodness$cutoff[1]=opt.cut(prf,pred)[3]
      
      plot(prf,main="Low rail_no grinding_speed>25")

        }
    }
}


### remove the outlier from the data
library(ggplot2)
ggplot(dat,aes(factor(binary.defect),y=Prior_Defect,fill=factor(binary.defect)))+geom_boxplot()


### choose prior defect=0; geometry defect=0; VTI=0; Turnout=0
data.most=dat[dat$Prior_Defect==0 & dat$Geometry_Defects==0 & dat$VTI==0 & dat$Turnout==0,]

### use rail quality , rail position, grinding as the tree node


data.most$binary.defect=ifelse(data.most$Defect_Number>0,1,0)
data.most$Grinding_B=ifelse(data.most$Grinding==0,0,1)
data.most$Grinding_B=as.character(data.most$Grinding_B)
class(data.most$Curve_Tangent)
data.most$Track.class=ifelse(data.most$Speed<=25, "1_2","3_4_5")

data.most$Track.class=as.character(data.most$Track.class)

position.f=unique(data.most$Curve_Tangent)
grinding.f=unique(data.most$Grinding_B)
speed.f=unique(data.most$Track.class)
library(xlsx)
for (i in 1: length(position.f)){
   for (j in 1: length(grinding.f)){
        for (k in 1: length(speed.f)){
           subdata=subset(data.most,Curve_Tangent==position.f[i]  & Grinding_B==grinding.f[j] & Track.class==speed.f[k])
         subdata=subdata[subdata$Division=="FLORENCE",]
           aa=paste(position.f[i],grinding.f[j],speed.f[k],sep = "")
          assign(aa,subdata)
          
          bb= table(subdata$binary.defect)
          write.xlsx(bb,file = "aaaa.xlsx",sheetName = aa,append = T)
         }
     }
 }  

###### choose one division data
## choose the division "florence"

data.use=T13_4_5
### choose the continous variables we consider: traffic density; rail age; rail size

data.use=data.use[,c(13,14,20,37)]
### normalized the continous variables

maxs=apply(data.use,2,max)
mins=apply(data.use,2,min)

scaled=as.data.frame(scale(data.use,center = mins,scale = maxs-mins))
library(reshape2)
library(DMwR)
library(unbalanced)
library(caret)
train.index=createDataPartition(scaled$binary.defect,p=0.75,list = FALSE)
train=scaled[train.index,]
test=scaled[-train.index,]
tomek=ubTomek(train[,-4],train[,4])
model_train_tomek=cbind(tomek$X,tomek$Y)
names(model_train_tomek)[4]="Class"
## removed index
removed.index=tomek$id.rm
train$binary.defect=as.factor(train$binary.defect)
train_tomek=train[-removed.index,]
### SMOTE after tomek links

set.seed(1234)
train_tomek_smote=SMOTE(binary.defect~.,train_tomek,perc.over = 1500,k=20,perc.under = 100)

rf_model=randomForest(binary.defect~.,train_tomek_smote,ntree = 1000,mtry = 2,sampsize = c(50,250),nodesize = 1,rules=TRUE)

rf_test_predictions=predict(rf_model,test,type = "Class")
comf_matrix_rf=table(test$binary.defect,rf_test_predictions)
confmatrix =confusionMatrix(comf_matrix_rf)
### borderline smote

library(smotefamily)
train.bs1=BLSMOTE(train[,-4],train[,4],K=10,C=10,dupSize = 0,method = "type1")
train.bs1=as.data.frame(train.bs1)

########## ####################################
######################################
####################################no tree-based subset and use tomek-link and SMOTE for the test data
## load the data "railbased_tenthmile_new.rda"
data.use=dat[which(dat$Division=="FLORENCE"),c(6:10,13:14,16:21,24:30)]
data.use$binary=ifelse(data.use$Defect_Number!=0,1,0)
maxs=apply(data.use,2,max)
mins=apply(data.use,2,min)

scaled=as.data.frame(scale(data.use,center = mins,scale = maxs-mins))
scaled$Defect_Number=NULL

scaled$binary=as.factor(scaled$binary)

train.index=createDataPartition(scaled$binary,p=0.75,list = FALSE)
train=scaled[train.index,]
test=scaled[-train.index,]



tomek=ubTomek(train[,-20],train[,20])
model_train_tomek=cbind(tomek$X,tomek$Y)
names(model_train_tomek)[20]="binary"
## removed index
removed.index=tomek$id.rm
train_tomek=train[-removed.index,]

### SMOTE after tomek links

set.seed(521)

train_tomek_smote=SMOTE(binary~.,train_tomek, perc.over=15000,k=200,perc.under=100)




train_tomek_smote=SMOTE( binary~.,  train_tomek,perc.over = 15000,k=200,perc.under = 100)

rf_model=randomForest(binary.defect~.,train_tomek_smote,ntree = 1000,mtry = 2,sampsize = c(50,250),nodesize = 1,rules=TRUE)

rf_test_predictions=predict(rf_model,test,type = "Class")
comf_matrix_rf=table(test$binary.defect,rf_test_predictions)
confmatrix =confusionMatrix(comf_matrix_rf)
#### borderline smote

require(smotefamily)

train_tomek_bs=BLSMOTE(train_tomek[,-20], train_tomek[,20], K=20,C=16,dupSize = 0,method = "type1")
aaa=train_tomek_bs$data
aaa$class=as.factor(aaa$class)

rf_model=randomForest(class~., data = aaa)
##### random forest for fit the train data
rf_model=randomForest(aaa[,-20],aaa[,20],ntree = 500,mtry = 4,nodesize = 1,rules=TRUE)

tomek.test=ubTomek(test[,-20],test[,20])
model_test_tomek=cbind(tomek.test$X,tomek.test$Y)
names(model_test_tomek)[20]="binary"
## removed index
removed.index=tomek.test$id.rm
test_tomek=test[-removed.index,]


test_tomek_bs=BLSMOTE(test_tomek[,-20], test_tomek[,20], K=20,C=16,dupSize = 0,method = "type1")
test.use=test_tomek_bs$data

rf_test_predictions=predict(rf_model,test.use,type = "prob")
rf_test_predictions=as.data.frame(rf_test_predictions)
table(test.use$class,rf_test_predictions)

### plot the tree 
plot(rf_model)
## variable importatnce plot
varImpPlot(rf_model,sort = T,n.var = 15)
variable.imp=data.frame(importance(rf_model,type = 2))
variable.imp$Variables=row.names(variable.imp)
library(ROCR)

pred.basic=prediction(rf_test_predictions$`1`,test.use$class)
slotNames(pred.basic)

# x s 1-specificity or false positive rate, y is true postive rate
prf=performance(pred.basic, "tpr","fpr")

plot(prf)

### check the result using orginal test data
rf.test=predict(rf_model,test,type = "class")

table(test$binary,rf.test)






############################################################################3333333333#######################
#############################################################################################################
#############################################################################################################


train.index=createDataPartition(scaled$binary,p=0.75,list = FALSE)
train=scaled[train.index,]
test=scaled[-train.index,]

### down-sampling using Tomek Links 

tomek=ubTomek(train[,-20],train[,20])
model_train_tomek=cbind(tomek$X,tomek$Y)
names(model_train_tomek)[20]="binary"
## removed index
removed.index=tomek$id.rm
train_tomek=train[-removed.index,]

### use train 

ctrl=trainControl(method = "cv",number = 5, savePredictions = TRUE)
tbmodel=train(binary~., data=train_tomek,method="treebag",trControl=ctrl)

predictors <- names(train_tomek)[names(train_tomek) != 'binary']
pred <- predict(tbmodel$finalModel, test[,predictors])
pred=as.character(pred)
pred=as.numeric(pred)
library(pROC)
auc <- roc(test$binary, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))


table(test$binary,pred)


######## use the SMOTE to oversampling and down sampling

library(DMwR)
class(train_tomek$binary)
train_tomek_smote=DMwR::SMOTE(binary~., train_tomek,perc.over=400,perc.under=200)


#### train


tbmodel_smote <- train(binary ~ ., data = train_tomek_smote, method = "treebag",
                 trControl = ctrl)

#################random forest


rf.1=randomForest(binary~.,train_tomek_smote,ntree = 500,mtry = 4,sampsize = c(50,250),nodesize = 1,rules=TRUE)

rf_test=predict(rf.1,test,type = "Class")

comf_matrix_rf=table(test$binary,rf_test)
confmatrix =confusionMatrix(comf_matrix_rf)

### tomek for test data

tomek.test=ubTomek(test[,-20],test[,20])
test_tomek=cbind(tomek.test$X,tomek.test$Y)
names(test_tomek)[20]="binary"
### using SMOTE to remove some test data

test_tomek_smote=DMwR::SMOTE(binary~., test_tomek,perc.over=400,perc.under=200)

rf_test.smote=predict(rf.1,test_tomek_smote,type = "Class")
table(test_tomek_smote$binary,rf_test.smote)








###################################
### neural network model
### combine Tomek link and Borderline Smote
n=names(train_tomek_smote)
f=as.formula(paste("binary~",paste(n[!n%in%"binary"],collapse = "+")))
library(neuralnet)
nn<-neuralnet(f,data=train_tomek_smote,hidden=c(10,5),linear.output = T)

###############################
##############################
################################
#################################
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all",classProbs = TRUE, savePredictions = TRUE)
gbmGrid = expand.grid(.interaction.depth = floor(sqrt(ncol(train_tomek_smote))), .n.trees = 500, .shrinkage = seq(0.0005,0.05,0.005),.n.minobsinnode = c(1))
gbmFit1 = train(train_tomek_smote[,-20],train_tomek_smote[,20],method = "gbm",trControl = fitControl,tuneGrid = gbmGrid,verbose = FALSE)

summary(bb)

library(ROCR)
pred= predict(gbmFit1,newdata = test_tomek[,-20])

pred.basic=ROCR::prediction(pred,test_tomek$binary)



# x s 1-specificity or false positive rate, y is true postive rate
prf=performance(pred.basic, "tpr","fpr")

plot(prf)

auc=performance(pred.basic,measure = "auc")
slot(auc,"y.values")
slot(auc,"y.name")


opt.cut=function(prf,pred.basic){
  cut.ind=mapply(FUN = function(x,y,p){
    d=(x-0)^2+(y-1)^2
    ind=which(d==min(d))
    c(sensitivity=y[[ind]], specificity=1-x[[ind]],cutoff=p[[ind]])
  }, prf@x.values,prf@y.values,pred.basic@cutoffs)
}

aa=opt.cut(prf,pred.basic)


library(xlsx)
### change the ratio in test data
parlist=c(100,150,200,250,300,350,400,500,600,700)

test_tomek_smote_1=DMwR::SMOTE(binary~., test_tomek,perc.over=400,perc.under=100000)
pred= predict(gbmFit1,newdata = test_tomek_smote_1[,-20])
pred.basic=ROCR::prediction(pred, test_tomek_smote_1$binary)
prf=performance(pred.basic, "tpr","fpr")


ratio=(table(test_tomek_smote_1$binary)[1])/table(test_tomek_smote_1$binary)[2]
auc=performance(pred.basic,measure = "auc")
ratio
auc@y.values

print(opt.cut(prf,pred.basic))









