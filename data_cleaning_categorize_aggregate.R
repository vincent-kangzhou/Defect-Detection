########splitting the raw database according to the static and time-dependent columns#########################
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
############add some usefule columns#############################################

data.adding=function(dat)
{
  Start_Age=as.vector(dat[,"Start_Date"]-dat[,"Laid_Year"])/365.25
  Defect_Age=as.vector(dat[,"Test_Date"]-dat[,"Laid_Year"])/365.25
  Age=as.vector(as.numeric(format(dat[,"Test_Date"],"%Y"))- as.numeric(format(dat[,"Laid_Year"],"%Y")))
  Year=as.vector(as.numeric(format(dat[,"Test_Date"],"%Y")))
  dat=cbind(dat, Start_Age=Start_Age, Defect_Age=Defect_Age,Age=Age,Year=Year)
  return(dat)
} 


############### convert the database for the all defect types into the database for one specific defect type#############
#### the columns we need to adjust are "Defect_Not", "Defect_Type", "Test_Date", "Defect_RailAge", "Prior_Defect"," Geo_D", "VTI"###########


Data_Ex$Defect_Type[Data_Ex$Defect_Type!="TDD"]=NA
AdjustedTestDate=paste(as.character(Data_Ex$Year),"-12-31",sep = "",collapse = NULL)
Data_Ex$Test_Date[is.na(Data_Ex$Defect_Type)==TRUE & Data_Ex$Defect_Not==1]=AdjustedTestDate[is.na(Data_Ex$Defect_Type)==TRUE & Data_Ex$Defect_Not==1]
AdjustRow=which(Data_Ex$Defect_Not==1 & is.na(Data_Ex$Defect_Type)==TRUE)

Num_R1=which(Data_Ex$Start_Age[AdjustRow]>=Data_Ex$Laid_Year[AdjustRow])

aaa=Data_Ex[AdjustRow,]








#################data cleaning###############

data.cleaning=function(dat)
  
{
  column.type=lapply(dat, class)
  dat=dat[,!names(dat) %in% c("Curve_Ele","Curve_Offset_ft","Curve_Length_mile","Grade_Length_mile")]
  
  dat$Joint_Weld[dat$Joint_Weld == ""] = "N"
  dat = dat[!is.na(dat$Laid_Year),]
  dat = dat[dat$Laid_Year < as.Date("2017-01-01"),]
  dat = dat[!is.na(dat$Start_RailAge_MGT),]
  dat = dat[!is.na(dat$Rail_Size_lbs_yard),]
  dat = dat[!is.na(dat$Curve_Degree_D),]
  dat = dat[!is.na(dat$Grade_Percent),]
  dat = dat[!is.na(dat$Grinding),]
  dat = dat[!is.na(dat$Ballast),]
  dat = dat[!is.na(dat$Prior_Defect),]
  dat = dat[!is.na(dat$Speed_mph),]
  dat = dat[dat$Defect_RailAge_MGT>0,]
  dat=dat[!is.na(dat$Ave_Car_Load_Ton),]
  return(dat)
}


############################data categirize###########################################

data.category=function(dat)
{
  dat$Age = as.factor(cut(dat$Age,c(0,20,40,60,80,Inf),include.lowest = T,labels = F))
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,15,Inf),labels = F))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Grade_Percent = abs(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,2,Inf),labels = F))
  dat$Grade_Percent[is.na(dat$Grade_Percent)]="0"
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding==0,"0",">=1"))
  dat$Ballast = as.factor(ifelse(dat$Ballast>1,">1",round(dat$Ballast)))
  dat$Traf_Den_MGT = as.factor(cut(dat$Traf_Den_MGT,c(0,10,20,30,40,50,Inf),include.lowest = T,labels = F))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T,labels = F,right = F))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,115,132,136,141,Inf),include.lowest = T,labels = F,right = F))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),labels = F,right = F,include.lowest = T))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  return(dat)
}


############################data categirize###########################################
data.category=function(dat)
{
  dat$Age = as.factor(cut(dat$Age,c(0,20,40,60,80,Inf),include.lowest = T,labels = F))
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,15,Inf),labels = F))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Grade_Percent = abs(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,2,Inf),labels = F))
  dat$Grade_Percent[is.na(dat$Grade_Percent)]="0"
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding==0,"0",">=1"))
  dat$Ballast = as.factor(ifelse(dat$Ballast>1,">1",round(dat$Ballast)))
  dat$Traf_Den_MGT = as.factor(cut(dat$Traf_Den_MGT,c(0,10,20,30,40,50,Inf),include.lowest = T,labels = F))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T,labels = F,right = F))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,115,132,136,141,Inf),include.lowest = T,labels = F,right = F))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),labels = F,right = F,include.lowest = T))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  return(dat)
}


############################data categirize###########################################

data.category1=function(dat)
{
  #dat$Age = as.factor(cut(dat$Age,c(0,20,40,60,80,Inf),include.lowest = T))
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,15,Inf)))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Grade_Percent = abs(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,2,Inf)))
  dat$Grade_Percent[is.na(dat$Grade_Percent)]="0"
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding==0,"0",">=1"))
  dat$Ballast = as.factor(ifelse(dat$Ballast>1,">1",round(dat$Ballast)))
  dat$Traf_Den_MGT = as.factor(cut(dat$Traf_Den_MGT,c(0,10,20,30,40,50,Inf),include.lowest = T))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T,right = F))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,115,132,136,141,Inf),include.lowest = T,right = F))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),include.lowest = T,right = F))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  return(dat)
}

##########Data_Category2##################

data.category2=function(dat)
{
  dat$Defect_RailAge_MGT = as.factor(cut(dat$Defect_RailAge_MGT,c(0,500,1000,1500,2000,2500,Inf),include.lowest = T))
  dat$Car_Pass = as.factor(cut(dat$Car_Pass,c(0,100000,300000,500000,700000,900000,1100000,1300000),include.lowest = T))
  dat$Failure_Age = as.factor(cut(dat$Failure_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Start_Age = as.factor(cut(dat$Start_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Defect_Age = as.factor(cut(dat$Defect_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Age = as.factor(cut(dat$Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Grade_Percent = abs(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,0.5,1,1.5,2,Inf)))
  dat$Grade_Percent[is.na(dat$Grade_Percent)]="0"
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding>=4,">=4",dat$Grinding))
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,Inf)))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Ballast = as.factor(ifelse(dat$Ballast>1,">1",round(dat$Ballast)))
  #dat$Traf_Den_MGT = as.factor(cut(dat$Traf_Den_MGT,c(0,5,10,20,30,40,50,60,70,Inf),include.lowest = T))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,115,132,136,141,Inf),include.lowest = T,right = F))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T,right = F))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),include.lowest = T,right = F))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  return(dat)
}

#####################################data.aggregate####################################
data.aggregate = function(dat)
{
  require(data.table)
  dat$Milepost = round(dat$Milepost*10)/10
  dat1 = as.data.table(dat)
  most.common <- function(x) {
    count <- sapply(unique(x), function(i) sum(x==i, na.rm=T))
    unique(x)[which(count==max(count))][1]
  }
  most.least <- function(x) {
    count <- sapply(unique(x), function(i) sum(x==i, na.rm=T))
    unique(x)[which(count==min(count))][1]
  }
  dat2 = dat1[, lapply(.(Speed_mph, Curve_Degree_D, Grade_Percent),mean), by = 'Prefix,Side,Milepost,Year']
  names(dat2)[5:7] = c("Speed_mph", "Curve_Degree_D", "Grade_Percent")
  dat3 = dat1[, lapply(.(Start_Date, Laid_Year),median), by = 'Prefix,Side,Milepost,Year']
  names(dat3)[5:6] = c("Start_Date","Laid_Year")
  dat = merge(dat2,dat3)
  dat4 = dat1[, lapply(.(Rail_Size_lbs_yard,Joint_Weld,Curve_Direction),most.common), by = 'Prefix,Side,Milepost,Year']
  names(dat4)[5:7] = c("Rail_Size_lbs_yard","Joint_Weld","Curve_Direction")
  dat = merge(dat,dat4)
  dat5 = dat1[, lapply(.(Defect_Not),function(x) 1*(sum(x)!=0)), by = 'Prefix,Side,Milepost,Year']
  names(dat5)[5] = c("Defect_Not")
  dat = merge(dat,dat5)
  dat6 = dat1[, lapply(.(Defect_Type),function(x) x[!is.na(x)][1] ), by = 'Prefix,Side,Milepost,Year']
  names(dat6)[5] = c("Defect_Type")
  dat = merge(dat,dat6)
  dat7 = dat1[,lapply(.(Start_RailAge_MGT,Defect_RailAge_MGT,Traf_Den_MGT,Ave_Car_Load_Ton,Grinding,Ballast),mean), by = 'Prefix,Side,Milepost,Year']
  names(dat7)[5:10] = c("Start_RailAge_MGT","Defect_RailAge_MGT","Traf_Den_MGT","Ave_Car_Load_Ton","Grinding","Ballast")
  dat = merge(dat,dat7)
  dat8 = dat1[,lapply(.(Prior_Defect,Geo_Defect),max), by = 'Prefix,Side,Milepost,Year']
  names(dat8)[5:6] = c("Prior_Defect","Geo_Defect")
  dat = merge(dat,dat8)
  dat9 = dat1[, lapply(.(Test_Date),most.least), by = 'Prefix,Side,Milepost,Year']
  names(dat9)[5] = c("Test_Date")
  dat = merge(dat,dat9)
  dat10 = dat1[,lapply(.(Division,Subdivision),most.common), by = 'Prefix,Side,Milepost,Year']
  names(dat10)[5:6] = c("Division","Subdivision")
  dat = merge(dat,dat10)
  
  dat[dat$Curve_Degree_D == 0 & dat$Curve_Direction != "T",]$Curve_Direction = "T"
  dat$Curve_Tangent[dat$Curve_Direction == "T"] = "T"
  dat$Curve_Tangent[dat$Curve_Direction != "T" & dat$Curve_Direction!= dat$Side] = "H"
  dat$Curve_Tangent[dat$Curve_Direction != "T" & dat$Curve_Direction == dat$Side] = "L"
  dat = dat[,-c("Year")]
  
  return(dat)
}
