dat1=dat[,c(155,1:10,13:16,19:22,25:28,31:34,37:40,43:65,68:69,72:97,106,131:136)]
data.splitting=function(dat)
{
  dat1 = dat[,grepl("\\d", names(dat))] 
  dat2 = dat[,!grepl("\\d", names(dat))] 
  remove(dat)
  a = regmatches(names(dat1), regexpr("[[:digit:]]+", names(dat1)))
  names(dat1) = gsub('.[0-9]+', '', names(dat1))
  b = as.character(seq(from = 2011, to = 2016, by = 1))
  ff =NULL
  for(i in 1:length(b)){
    Time = as.data.frame(rep(b[i],nrow(dat1)))
    c = dat1[,a==b[i]]
    e = cbind(c,Time)
    ff = rbind(e,ff) 
  }
  remove(dat1)
  remove(c)
  remove(e)
  ff1 = NULL
  for(i in 1:length(b)){
    ff1 = rbind(dat2,ff1) 
  }
  remove(dat2)
  dat = cbind(ff,ff1)
  dat[,"rep(b[i], nrow(dat1))"]=NULL
  rm(ff,ff1,Time)
  return(dat)
}

rm(dat)
dat1=data.splitting(dat1)

data.adding=function(dat)
{
  Start_Age=as.vector(dat[,"Start_date"]-dat[,"Laid_Year"])/365.25
  Defect_Age=as.vector(dat[,"Test_date"]-dat[,"Laid_Year"])/365.25
  Age=as.vector(as.numeric(format(dat[,"Test_date"],"%Y"))- as.numeric(format(dat[,"Laid_Year"],"%Y")))
  Year=as.vector(as.numeric(format(dat[,"Test_date"],"%Y")))
  dat=cbind(dat, Start_Age=Start_Age, Defect_Age=Defect_Age,Age=Age,Year=Year)
  return(dat)
}

dat1=data.adding(dat1)

dat1 = subset(dat1,Year %in% c("2013","2014","2015","2016"))
dat1$ID=NULL

dat1$F_Milepost=floor(dat1$Milepost)
summary(dat1$F_Milepost)
library(data.table)
dat1$Mile=0.01

dat1$Rail_Quality[dat1$Rail_Quality=="Q"]=NA
dat1$Rail_Quality[dat1$Rail_Quality=="H"]=NA
dat1=subset(dat1,!is.na(dat1$Rail_Quality) & !is.na(dat1$Joint_Weld) & !is.na(dat1$Rail_Size_lbs_yard) 
            & !is.na(dat1$Speed_mph) &!is.na(dat1$Laid_Year))
dat1=subset(dat1,dat1$Age>=0)


dat1=as.data.table(dat1)

dat2 = dat1[, lapply(.(Defect_Not,Prior_Defect,Geo_Defect,VTI,Turnout,Mile, 0.01*Traf_Den_MGT),function(x) sum(x, na.rm = T)), by = 'Prefix,Year,Track_Type,Side,F_Milepost']
names(dat2)=c("Prefix","Year","Track_Type","Side","MP","Defect_Number","Prior_Defect","Geometry_Defects","VTI","Turnout","Length","Ton_Mile")
dat2$Ton_Mile=round(dat2$Ton_Mile/2,digits = 2)
dat2$Traffic_Density=dat2$Ton_Mile/dat2$Length
dat2$Traffic_Density=round(dat2$Traffic_Density,digits = 2)

dat3 = dat1[,lapply(.(Start_Age,Defect_Age,Car_Pass,Curve_Degree_D,abs(Grade_Percent),Grinding,Ballast,Rail_Size_lbs_yard,Speed_mph),function(x) mean(x,na.rm = T)), 
            by = 'Prefix,Year,Track_Type,Side,F_Milepost']

names(dat3)=c("Prefix","Year","Track_Type","Side","MP","Start_Age","Defect_Age","Car_Pass","Curve_D","Grade","Grinding","Ballast","Rail_Size","Speed")
dat3$Start_Age=round(dat3$Start_Age,digits = 2)
dat3$Defect_Age=round(dat3$Defect_Age,digits = 2)

dat3$Car_Pass=round(dat3$Car_Pass)
dat3$Speed=round(dat3$Speed)
dat3$Rail_Size=round(dat3$Rail_Size)

dat3[,9:12]=round(dat3[,9:12],digits = 2)

most.common <- function(x) {
  count <- sapply(unique(x), function(i) sum(x==i, na.rm=T))
  unique(x)[which(count==max(count))][1]
}

dat4 = dat1[,lapply(.(Division,Subdivision),function(x) most.common(x)), by = 'Prefix,Year,Track_Type,Side,F_Milepost']
names(dat4)=c("Prefix","Year","Track_Type","Side","MP","Division","Subdivision")
backup=dat1
dat1$Rail_Quality[dat1$Rail_Quality=="P"]="N"

dat1$Rail_Quality = factor(dat1$Rail_Quality , exclude = NULL)
dat1$Joint_Weld = factor(dat1$Joint_Weld, exclude = NULL)
dat1$Curve_Tangent = factor(dat1$Curve_Tangent, exclude = NULL)
dat6 = model.matrix(~ Rail_Quality - 1 ,dat1)
dat7 = model.matrix(~ Joint_Weld - 1 ,dat1)
dat8 = model.matrix(~ Curve_Tangent - 1,dat1)

dat9 = cbind(dat6,dat7,dat8)
rm(dat6,dat7,dat8)
dat9 = as.data.frame(dat9)
dat1 = cbind(dat1,dat9)

dat1=as.data.table(dat1)

dat10 = dat1[,lapply(.(Rail_QualityN, Rail_QualityR, Joint_WeldJ, Joint_WeldW,  Curve_TangentH, Curve_TangentL,
                       Curve_TangentT),function(x) sum(x,na.rm = T)),by = 'Prefix,Year,Track_Type,Side,F_Milepost']


names(dat10)=c("Prefix","Year","Track_Type","Side","MP","Rail_QualityN","Rail_QualityR",
               "Joint_WeldJ","Joint_WeldW","Curve_TangentH","Curve_TangentL","Curve_TangentT")


dat10$Length=apply(dat10[,10:12],1,sum)

dat10$Rail_QualityN_L=dat10$Rail_QualityN/dat10$Length
dat10$Rail_QualityR_L=dat10$Rail_QualityR/dat10$Length
dat10$Joint_WeldJ_L=dat10$Joint_WeldJ/dat10$Length
dat10$Joint_WeldW_L=dat10$Joint_WeldW/dat10$Length
dat10$Curve_TangentH_L=dat10$Curve_TangentH/dat10$Length
dat10$Curve_TangentL_L=dat10$Curve_TangentL/dat10$Length
dat10$Curve_TangentT_L=dat10$Curve_TangentT/dat10$Length

dat10[,14:20]=round(dat10[,14:20],digits = 2)

dat11=dat10[,c(1:5,14:20)]


dat = merge(dat2,dat3, by=c("Prefix","Year","Track_Type","Side","MP"),all = TRUE)

dat = merge(dat,dat4, by=c("Prefix","Year","Track_Type","Side","MP"),all = TRUE)
dat = merge(dat,dat11, by=c("Prefix","Year","Track_Type","Side","MP"),all = TRUE)

dat$ID=paste(dat$Prefix,dat$Track_Type,dat$Side, dat$MP,sep = "_")

dat=dat[order(dat[,32],dat[,2]),]

tran <- as.integer(ave(dat$Defect_Number, dat$ID, FUN = cumsum) >= 1)

dat$Mortality=tran

save(dat,file = "dat for survival analysis with weindows.rds")

dat$Defect_Number[dat$Defect_Number!=0]=1


dat=within(dat,{
  
  
  Prior_Defect= log(Prior_Defect+1)
  Geometry_Defects= log (Geometry_Defects+1)
  VTI = log (VTI+1)
  Turnout= log(Turnout+1)
  Ton_Mile= log(Ton_Mile+1)
  Traffic_Density= log( Traffic_Density+1)
  Curve_D= log(Curve_D+1)
  Grinding= log (Grinding+1)
  Rail_Size=log (Rail_Size + 1)
  Speed=log(Speed+1)
})

dat$Grinding_C=as.character(cut(dat$Grinding,c(0,1.1,3.5),include.lowest = F))
dat$Grinding_C[dat$Grinding==0]="0"
dat$Grinding_C=factor(dat$Grinding_C)
dat=within(dat,Grinding_C<-relevel(Grinding_C,ref = 3))
dat$JW_P=ifelse(dat$Joint_WeldJ_L==1,"J","W")

dat=within(dat,{
  Prefix=factor(Prefix)
  Year=factor(Year)
  Track_Type=factor(Track_Type)
  Side=factor(Side)
  Grinding_C=factor(Grinding_C)
})


dat$Curvature=1*dat$Curve_TangentT_L+2*dat$Curve_TangentH_L

require(survival)
require(coxme)


m.1=coxphf(Surv(Start_Age,Defect_Age,Mortality)~Prior_Defect+Geometry_Defects+VTI+Turnout+Traffic_Density+Curve_D+Grade+Grinding_C+Grinding_C:Traffic_Density +
            Ballast+Rail_Size+Speed + Rail_QualityR_L+JW_P+JW_P:Traffic_Density+ Curvature +(1|ID),
          data = dat,firth=T)

### select the first defect number =1

ID.set=unique(dat$ID)

dat1=subset(dat,dat$Defect_Number==1 & dat$Year==2013)

ID.select=unique(dat1$ID)
dat2=subset(dat,dat$Defect_Number==0 & dat$Year==2013)

dat3=subset(dat,dat$Year==2014 & !dat$ID  %in% ID.select)

survival.dat=rbind(dat1,dat2,dat3)
ID.select=unique(survival.dat$ID[survival.dat$Defect_Number==1])

dat4=subset(dat,dat$Year=="2015" & !dat$ID  %in% ID.select)

survival.dat=rbind(survival.dat,dat4)
ID.select=unique(survival.dat$ID[survival.dat$Defect_Number==1])
dat5=subset(dat,dat$Year=="2016" & !dat$ID  %in% ID.select)

survival.dat=rbind(survival.dat,dat5)


save(survival.dat,file = "survival data with first defects.rds")


sm.1=coxme(Surv(Start_Age,Defect_Age,Mortality)~Prior_Defect+Geometry_Defects+VTI+Turnout+Traffic_Density+Curve_D+Grade+Grinding_C+Grinding_C:Traffic_Density +
            Ballast+Rail_Size+Speed + Rail_QualityR_L+ Curvature +(1|ID),
          data = survival.dat)

### penalized cox model without random effect

install.packages("coxphf")
require(coxphf)
sm.2=coxphf(Surv(Start_Age,Defect_Age,Mortality)~Prior_Defect+Geometry_Defects+VTI+Turnout+Traffic_Density+Curve_D+Grade+Grinding_C+Grinding_C:Traffic_Density +
             Ballast+Rail_Size+Speed + Rail_QualityR_L+ Curvature,firth=T,
           data = dat)
### penalized logistic

install.packages("penalized")
library(penalized)


sm.3=penalized(Surv(Start_Age,Defect_Age,Mortality),penalized =~Prior_Defect+Geometry_Defects+VTI+Turnout+Traffic_Density+Curve_D+Grade+Grinding_C+Grinding_C:Traffic_Density +
                 Ballast+Rail_Size+Speed + Rail_QualityR_L+ Curvature,data=survival.dat,positive = F,steps = 2,standardize = TRUE)

### general cox model

sm.4=coxph(Surv(Start_Age,Defect_Age,Mortality)~Prior_Defect+Geometry_Defects+VTI+Turnout+Traffic_Density+Curve_D+Grade+Grinding_C+Grinding_C:Traffic_Density +
             Ballast+Rail_Size+Speed + Rail_QualityR_L+ Curvature,
           data = survival.dat)

base.haz=unlist(exp(-1*basehaz(sm.4,center=F)["hazard"]))
Time = unlist(basehaz(sm.4,centered=F)["time"])




X=model.matrix(terms(sm.1),dat)
X=X[,-1]

X1=as.data.frame(X)

b=fixef(sm.1)
medianpredproblogit=X %*% b
Pre=coef(mm_3)$Prefix[,1]
Yea=coef(mm_3)$Year[,1]

prefix=as.vector(usedata$Prefix)
Prename=as.vector(rownames(coef(mm_3)$Prefix))
Ranpre=Pre[match(prefix,Prename)]


Year=as.vector(usedata$Year)
Yearname=as.vector(rownames(coef(mm_3)$Year))
Ranyear=Yea[match(Year,Yearname)]

FixInter=fixef(mm_3)[1]
Intercept=rep(FixInter,nrow(usedata))

medianpredproblogit=as.data.frame(medianpredproblogit)
names(medianpredproblogit)[1]="Fixed.part"

check=as.vector(medianpredproblogit[,1])+Ranyear+Ranpre-2*Intercept
library(VGAM)
checkpro=logit(check,inverse=TRUE)


