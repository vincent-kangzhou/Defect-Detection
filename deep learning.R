


#maxs <- as.numeric(apply(dat2, 2, max))
#mins <- as.numeric(apply(dat2, 2, min))
#scaled <- as.data.frame(scale(dat2, center = mins, scale = maxs - mins))



dat2=subset(ndat,select = c(Division,Subdivision,Prefix,Milepost,Side,Year))
dat1 = subset(ndat, select=c(Defect_Not, Curve_Degree_D, 
                            Grade_Percent, Grinding, Ballast,Rail_Size_lbs_yard,
                            Prior_Defect,Geo_Defect,Curve_Tangent,Joint_Weld,Traf_Den_MGT,Ave_Car_Load_Ton,Age,Speed_mph))



factored = model.matrix(~.,dat1)
factored = factored[,-1]

ndat = cbind(factored,dat2)


ndat=cbind(dat2,dat1)



train <- ndat[which(ndat$Year!=2016),]
test <- ndat[which(ndat$Year==2016),]

require(neuralnet)
n <- names(train)
f <- as.formula(paste("Defect_Not ~", paste(n[!n %in% c("Defect_Not","Division","Subdivision","Prefix","Milepost","Side","Year")], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(20,10),threshold = 0.01, stepmax = 100000, linear.output=T)

summary(nn$response)

test.nn=compute(nn,test[,2:48])

unique(test.nn$net.result)


mean1 = sum(ndat$Defect_Not)/nrow(ndat)
mean2 = sum(dat$Defect_Not)/nrow(dat)
nn$net.result
table(1*(nn$response>=mean1),train$Defect_Not)
table(1*(test.nn$net.result>=mean1),test$Defect_Not)
require(ROCR)
pred <- prediction(nn$net.result,train$Defect_Not)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
dis = sqrt((unlist(roc.perf@x.values) - 0)^2 + (unlist(roc.perf@y.values) -1)^2)
min(dis)
plot(roc.perf)


train.nn=compute(nn,train[,2:48])











