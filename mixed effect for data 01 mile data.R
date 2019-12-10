#### load package
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(caret)
require(ROCR)
require(xlsx)
require(data.table)
modeldata=read.csv('G:/Kang Zhou/Desktop/Rail defect model/result/random forests/data 01mile model.csv')

dat1=modeldata[,c(1:7,9:10,12:14,16:18,21:23,25,27:28,31:32,34:41)]

dat1=within(dat1,{
  Prefix=factor(Prefix)
  Year=factor(Year)
  Track_Type=factor(Track_Type)
})

maxs=apply(dat1[,c(8:30)],2,max)
mins=apply(dat1[,c(8:30)],2,min)
scaled=as.data.frame(scale(dat1[,c(8:30)],center = mins,scale = maxs-mins))
data.use=cbind(dat1[,c(1:7,31)],scaled)

n=names(data.use)[c(9:14,16:31)]

K.fold=caret::createFolds(data.use$Defect_Not,k=5,list = TRUE,returnTrain = T)
train_1=data.use[K.fold[[1]],]
test_1=data.use[-K.fold[[1]],]




m.1=glmer(Defect_Not~Ballast_last+Ballast_last2+Car_Pass_last+Car_Pass_last2+Curve_Degree_D+
            Curve_Tangent_H+Curve_Tangent_T+All_Defect_last+All_Defect_last2+Geo_Defects_combined
          +Grade_Percent+Grinding_last+Grinding_last2+Rail_Age+Rail_Quality_N+Rail_Size_lbs_yard
          +Speed_mph+Traf_Den_current_MGT+Traf_Den_last2_MGT+Traf_Den_last_MGT+Turnout+VTI_last
          +(1|Prefix),
          data = train_1, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=5e5)),nAGQ = 1)




for (i in 2:5){
  int.train=data.use[K.fold[[i]],]
  code.tr=paste("train_",i,sep="")
  assign(code.tr,int.train)
  int.test=data.use[-K.fold[[i]],]
  code.te=paste("test_",i,sep="")
  assign(code.te,int.test)
  
  mm=glmer(Defect_Not~Ballast_last+Ballast_last2+Car_Pass_last+Car_Pass_last2+Curve_Degree_D+
                 Curve_Tangent_H+Curve_Tangent_T+All_Defect_last+All_Defect_last2+Geo_Defects_combined
               +Grade_Percent+Grinding_last+Grinding_last2+Rail_Age+Rail_Quality_N+Rail_Size_lbs_yard
               +Speed_mph+Traf_Den_current_MGT+Traf_Den_last2_MGT+Traf_Den_last_MGT+Turnout+VTI_last
               +(1|Prefix),
               data = int.train, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=5e5)),nAGQ = 1)
  
  
  code.model=paste("mixed",i,sep = "")
  assign(code.model,mm)
  
  
  
}

fitted.prob1= predict(m.1,test_1,type='response')
fitted.prob2= predict(mixed2,test_2,type='response')
fitted.prob3= predict(mixed3,test_3,type='response')
fitted.prob4= predict(mixed4,test_4,type='response')
fitted.prob5= predict(mixed5,test_5,type='response')

test_1['Fit.prob']=fitted.prob1
test_2['Fit.prob']=fitted.prob2
test_3['Fit.prob']=fitted.prob3
test_4['Fit.prob']=fitted.prob4
test_5['Fit.prob']=fitted.prob5

result=data.frame("Defect"=integer(),"Nondefect"=integer(), "Ratio"=numeric(),"AUC"=numeric(),"Gen.sens"=numeric(),
                  "Gen.spec"=numeric(),"Gen.prec"=numeric(), "Gen.cutoff"=numeric(),"F1.sens"=numeric(),"F1.spec"=numeric(),"F1.prec"=numeric(), "F1.cutoff"=numeric(),
                  "Geometry.sens"=numeric(),"Geometry.spec"=numeric(),"Geometry.prec"=numeric(),"Geometry.cutoff"=numeric()
)





Test.whole=rbind(test_1,test_2,test_3,test_4,test_5)
pred.basic=prediction(Test.whole['Fit.prob'],Test.whole['Defect_Not'])

perf=performance(pred.basic,'tpr','fpr')

auc=performance(pred.basic,measure = 'auc')

opt.cut=function(perf,pred.basic){
  cut.ind=mapply(FUN = function(x,y,p){
    d=(x-0)^2+(y-1)^2
    ind=which(d==min(d))
    c(sensitivity=y[[ind]], specificity=1-x[[ind]],cutoff=p[[ind]],ind=ind)
  }, perf@x.values,perf@y.values,pred.basic@cutoffs)
}

prec=performance(pred.basic,'prec')

gen.re=opt.cut(perf,pred.basic)

result[1,1]=min(table(Test.whole$Defect_Not))

result[1,2]=max(table(Test.whole$Defect_Not))
result[1,3]=result[1,2]/result[1,1]
result[1,4]=auc@y.values
result[1,5]=gen.re[1,1]
result[1,6]=gen.re[2,1]
result[1,8]=gen.re[3,1]
result[1,7]=unlist(prec@y.values)[gen.re[4,1]]

sen=unlist(perf@y.values)
spec=1-unlist(perf@x.values)
geometry.index=which.max(sqrt(sen*spec))
precision=unlist(prec@y.values)
geometry.cutoff=unlist(pred.basic@cutoffs)
result[1,13]=sen[geometry.index]
result[1,14]=spec[geometry.index]
result[1,15]=precision[geometry.index]
result[1,16]=geometry.cutoff[geometry.index]


F1.perf=performance(pred.basic,'ppv','tpr')
tpr=unlist(F1.perf@x.values)
F1.precision=unlist(F1.perf@y.values)
F1.ind=which.max(tpr*F1.precision/(tpr+F1.precision))
specificity=performance(pred.basic,'spec')
F1.cutoff=unlist(pred.basic@cutoffs)
result[1,9]=tpr[F1.ind]
result[1,10]=unlist(specificity@y.values)[F1.ind]
result[1,11]=F1.precision[F1.ind]
result[1,12]=F1.cutoff[F1.ind]
write.xlsx(result,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'criteria')



coef.1=coef(summary(m.1))
coef.2=coef(summary(mixed2))
coef.3=coef(summary(mixed3))
coef.4=coef(summary(mixed4))
coef.5=coef(summary(mixed5))

write.xlsx(coef.1,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'fold 1 coef',append = T)
write.xlsx(coef.2,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'fold 2 coef',append = T)
write.xlsx(coef.3,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'fold 3 coef',append = T)
write.xlsx(coef.4,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'fold 4 coef',append = T)
write.xlsx(coef.5,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'fold 5 coef',append = T)

Test.whole['Length']=0.1
test.tabledata=as.data.table(Test.whole)

check.prefix=test.tabledata[,lapply(.(Defect_Not,Fit.prob,Length),function(x) sum(x,na.rm = T)),by='Prefix']
check.div=test.tabledata[,lapply(.(Defect_Not,Fit.prob,Length),function(x) sum(x,na.rm = T)),by='Division,Year']
check.prefix.y=test.tabledata[,lapply(.(Defect_Not,Fit.prob,Length),function(x) sum(x,na.rm = T)),by='Prefix,Year']

write.xlsx(check.prefix,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'prefix validation',append = T)
write.xlsx(check.div,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'division validation',append = T)
write.xlsx(check.prefix.y,file = 'Mixed effect for 01 mile data criteria.xlsx',sheetName = 'prefix-year validation',append = T)



