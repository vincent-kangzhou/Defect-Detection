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

data.adding1=function(dat)
{
  Start_Age=as.vector(dat[,"Start_Date"]-dat[,"Laid_Year"])/365.25
  Defect_Age=as.vector(dat[,"Test_Date"]-dat[,"Laid_Year"])/365.25
  Failure_Age = as.vector(dat[,"Failure_Date"]-dat[,"Laid_Year"])/365.25
  Age=as.vector(as.numeric(format(dat[,"Test_Date"],"%Y"))- as.numeric(format(dat[,"Laid_Year"],"%Y")))
  F_Age = as.vector(as.numeric(format(dat[,"Failure_Date"],"%Y")) - as.numeric(format(dat[,"Laid_Year"],"%Y")))
  Year=as.vector(as.numeric(format(dat[,"Test_Date"],"%Y")))
  dat=cbind(dat, Start_Age=Start_Age, Defect_Age=Defect_Age,Age=Age,Year=Year,Failure_Age = Failure_Age,F_Age = F_Age)
  return(dat)
} 

#################data cleaning###############

data.cleaning=function(dat)
  
{
  column.type=lapply(dat, class)
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
  dat=dat[,!names(dat) %in% c("rep(b[i], nrow(dat1))","Curve_Ele","Curve_Offset_ft","Curve_Length_mile","Grade_Length_mile","Turnout","Inspection_Fre")]
  return(dat)
}



#################data cleaning###############

data.cleaning1=function(dat)
  
{
  column.type=lapply(dat, class)
  dat$Joint_Weld[dat$Joint_Weld == ""] = NA
  #  dat = dat[!is.na(dat$Laid_Year),]
  dat = dat[dat$Laid_Year < as.Date("2017-01-01"),]
  #  dat = dat[!is.na(dat$Start_RailAge_MGT),]
  #  dat = dat[!is.na(dat$Rail_Size_lbs_yard),]
  #  dat = dat[!is.na(dat$Curve_Degree_D),]
  #  dat = dat[!is.na(dat$Grade_Percent),]
  #  dat = dat[!is.na(dat$Grinding),]
  #  dat = dat[!is.na(dat$Ballast),]
  #  dat = dat[!is.na(dat$Prior_Defect),]
  #  dat = dat[!is.na(dat$Speed_mph),]
  dat = dat[dat$Defect_RailAge_MGT>0,]
  dat = dat[dat$Start_RailAge_MGT>0,]
  #  dat=dat[!is.na(dat$Ave_Car_Load_Ton),]
  dat=dat[,!names(dat) %in% c("rep(b[i], nrow(dat1))","Curve_Ele","Curve_Offset_ft","Curve_Length_mile","Grade_Length_mile","Turnout","Inspection_Fre")]
  return(dat)
}

#################data cleaning###############

data.cleaning2=function(dat){
  dat$Start_Age[dat$Start_Age<0] = NA
  dat$Defect_Age[dat$Defect_Age<0] = NA
  dat$Age[dat$Age<0] = NA
  dat$Failure_Age[dat$Failure_Age<0] = NA
  dat$F_Age[dat$F_Age<0] = NA
  return(dat)
}


#################data cleaning###############

data.cleaning3=function(dat){
  dat$Start_Age[dat$Start_Age<0] = NA
  dat$Defect_Age[dat$Defect_Age<0] = NA
  dat$Age[dat$Age<0] = NA
  dat$Failure_Age[dat$Failure_Age<0] = NA
  dat$F_Age[dat$F_Age<0] = NA
  dat$Speed_mph[dat$Prefix == "BE" & dat$Speed_mph == 62] = 30
  dat$Speed_mph[dat$Prefix == "A" & dat$Speed_mph == 70] = 60
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
  dat$F_Age = as.factor(cut(dat$F_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Start_Age = as.factor(cut(dat$Start_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Defect_Age = as.factor(cut(dat$Defect_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Age = as.factor(cut(dat$Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  #dat$Rail_Quality[dat$Rail_Quality == "H",] = NA
  #dat$Rail_Quality[dat$Rail_Quality == "Q",] = NA
  dat$Grade_Percent = abs(dat$Grade_Percent)
  a = is.na(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,0.5,Inf)))
  dat$Grade_Percent[is.na(dat$Grade_Percent)]="0"
  dat$Grade_Percent[a] = NA
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding>=5,">=5",dat$Grinding))
  b= is.na(dat$Curve_Degree_D)
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,Inf)))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D[b] = NA
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Ballast = as.factor(ifelse(dat$Ballast>1,">1",round(dat$Ballast)))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,122,132,136,141,Inf),include.lowest = T,right = F))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T,right = F))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),include.lowest = T))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  dat$Inspection_Fre = as.factor(cut(dat$Inspection_Fre,c(0,3,6,9,Inf),include.lowest = T))
  dat$VTI = as.factor(ifelse(dat$VTI == 0,0,">=1"))
  return(dat)
}

##########Data_Category3##################

data.category3=function(dat)
{
  dat$Defect_RailAge_MGT = as.factor(cut(dat$Defect_RailAge_MGT,c(0,500,1000,1500,2000,2500,Inf),include.lowest = T))
  dat$Car_Pass = as.factor(cut(dat$Car_Pass,c(0,100000,300000,500000,700000,900000,1100000,1300000),include.lowest = T))
  dat$Failure_Age = as.factor(cut(dat$Failure_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$F_Age = as.factor(cut(dat$F_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Start_Age = as.factor(cut(dat$Start_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Defect_Age = as.factor(cut(dat$Defect_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Age = as.factor(cut(dat$Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Rail_Quality[dat$Rail_Quality == "H"] = NA
  dat$Rail_Quality[dat$Rail_Quality == "Q"] = NA
 # dat$Grade_Percent = abs(dat$Grade_Percent)
  a = is.na(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,0.5,Inf)))
  dat$Grade_Percent[is.na(dat$Grade_Percent)] = "0"
  dat$Grade_Percent[a] = NA
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding>=3,">=3",dat$Grinding))
  b= is.na(dat$Curve_Degree_D)
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,Inf)))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D[b] = NA
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Ballast = as.factor(ifelse(dat$Ballast>=1,">=1",round(dat$Ballast)))
  #dat$Traf_Den_MGT = as.factor(cut(dat$Traf_Den_MGT,c(0,5,10,20,30,40,50,60,70,Inf),include.lowest = T))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,122,132,136,141,Inf),include.lowest = T,right = F))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60,Inf),include.lowest = T))
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  #dat$Inspection_Fre = as.factor(cut(dat$Inspection_Fre,c(0,3,6,9,Inf),include.lowest = T))
  dat$VTI = as.factor(ifelse(dat$VTI == 0,0,">=1"))
  return(dat)
}


##########Data_Category4##################

data.category4=function(dat)
{
  dat$Curve_Tangent = as.factor(dat$Curve_Tangent)
  dat$Joint_Weld = as.factor(dat$Joint_Weld)
  dat$Rail_Quality[dat$Rail_Quality == "H"] = NA
  dat$Rail_Quality[dat$Rail_Quality == "Q"] = NA
  dat$Failure_Age = as.factor(cut(dat$Failure_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$F_Age = as.factor(cut(dat$F_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Age = as.factor(cut(dat$Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Start_Age = as.factor(cut(dat$Start_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Defect_Age = as.factor(cut(dat$Defect_Age,c(0,10,20,30,40,50,60,70,80,Inf),include.lowest = T))
  dat$Defect_RailAge_MGT = as.factor(cut(dat$Defect_RailAge_MGT,c(0,500,1000,1500,2000,2500,Inf),include.lowest = T))
  dat$Ave_Car_Load_Ton = as.factor(cut(dat$Ave_Car_Load_Ton,c(0,80,85,90,Inf),include.lowest = T))
  dat$Car_Pass = as.factor(cut(dat$Car_Pass,c(0,100000,300000,500000,700000,900000,1100000,1300000),include.lowest = T))
  dat$Ballast = as.factor(ifelse(dat$Ballast>=1,">=1",round(dat$Ballast)))
  b= is.na(dat$Curve_Degree_D)
  dat$Curve_Degree_D = as.character(cut(dat$Curve_Degree_D,c(0,5,10,Inf)))
  dat$Curve_Degree_D[is.na(dat$Curve_Degree_D)] = "0"
  dat$Curve_Degree_D[b] = NA
  dat$Curve_Degree_D = as.factor(dat$Curve_Degree_D)
  dat$Grade_Percent = abs(dat$Grade_Percent)
  a = is.na(dat$Grade_Percent)
  dat$Grade_Percent = as.character(cut(dat$Grade_Percent,c(0,0.5,Inf)))
  dat$Grade_Percent[is.na(dat$Grade_Percent)] = "0"
  dat$Grade_Percent[a] = NA
  dat$Grade_Percent = as.factor(dat$Grade_Percent)
  dat$Grinding = as.factor(ifelse(dat$Grinding>=5,">=5",dat$Grinding))
  dat$Inspection_Fre = as.factor(cut(dat$Inspection_Fre,c(0,3,6,9,Inf),include.lowest = T))
  dat$Prior_Defect = as.factor(ifelse(dat$Prior_Defect>1,">1",dat$Prior_Defect))
  dat$Rail_Size_lbs_yard = as.factor(cut(dat$Rail_Size_lbs_yard,c(0,122,132,136,141,Inf),include.lowest = T,right = F))
  dat$Speed_mph = as.factor(cut(dat$Speed_mph,c(0,10,25,40,60),include.lowest = T))
  dat$Geo_Defect = as.factor(ifelse(dat$Geo_Defect == 0, "0", "1"))
  dat$VTI = as.factor(ifelse(dat$VTI == 0,0,">=1"))
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


#####################################data.aggregate by Prefix####################################
data.aggregate1 = function(dat)
{
  ###Aggregate Category
  ###Curve Tangent
  dat$Curve_Tangent[dat$Curve_Tangent == "H"] = 3
  dat$Curve_Tangent[dat$Curve_Tangent == "T"] = 2
  dat$Curve_Tangent[dat$Curve_Tangent == "L"] = 1
  dat$Curve_Tangent = as.integer(dat$Curve_Tangent)
  
  ###Jointed and Welded
  dat$Joint_Weld[dat$Joint_Weld == "W"] = 1
  dat$Joint_Weld[dat$Joint_Weld == "J"] = 2
  dat$Joint_Weld = as.integer(dat$Joint_Weld)
  
  ###Rail Quailty 
  dat$Rail_Quality[dat$Rail_Quality == "H"] = NA
  dat$Rail_Quality[dat$Rail_Quality == "Q"] = NA
  dat$Rail_Quality[dat$Rail_Quality == "P"] = 1
  dat$Rail_Quality[dat$Rail_Quality == "N"] = 2
  dat$Rail_Quality[dat$Rail_Quality == "R"] = 3
  dat$Rail_Quality = as.integer(dat$Rail_Quality)
  
  dat$Mile = 0.01
  
  require(data.table)
  dat1 = as.data.table(dat)
  dat2 = dat1[, lapply(.(Defect_Not,Failure_Not,Traf_Den_MGT,Mile),sum), by = 'Prefix,Year']
  names(dat2)[3:6] = c("Total_Defect_Not","Total_Failure_Not","Total_Traf_Den_MGT","Mileage")
  dat3 = dat1[,lapply(.(Age,F_Age,Ave_Car_Load_Ton,Ballast,Car_Pass,Curve_Degree_D,Defect_RailAge_MGT,Grade_Percent,Grinding,Inspection_Fre,Prior_Defect,Rail_Size_lbs_yard,Speed_mph,Geo_Defect,VTI),function(x) mean(x,na.rm = T)), by = 'Prefix,Year']
  names(dat3)[3:17] = c("Age","F_Age","Ave_Car_Load_Ton","Ballast","Car_Pass","Curve_Degree_D","Defect_RailAge_MGT","Grade_Percent","Grinding","Inspection_Fre","Prior_Defect","Rail_Size_lbs_yard","Speed_mph","Geo_Defect","VTI")
  dat = merge(dat2,dat3)
  dat4 = dat1[,lapply(.(Curve_Tangent,Joint_Weld,Rail_Quality),function(x) mean(x,na.rm = T)),by = 'Prefix,Year']
  names(dat4)[3:5] = c("Curve_Tangent","Joint_Weld","Rail_Quality")
  dat = merge(dat,dat4)
  return(dat)
}

#####################table functions##########################
table1 = function(n=NA,x){
  x = factor(x , exclude = NULL)
  a = table(x,dat$Defect_Not)
  t4 = aggregate(0.01*dat$Traf_Den_MGT,list(x),sum)
  aa = a[,2]/t4$x *1000
  aa1 = t(rbind(a[,2],t4$x,aa))
  if(!anyNA(n)){rownames(aa1) = factor_names(n)}
  write.table(aa1,"t.csv",col.names=T, append = T, sep=",")
}

table2 = function(n=NA,x){
  x = factor(x , exclude = NULL)
  a = table(x,dat1$Defect_Not)
  t4 = aggregate(0.01*dat1$Traf_Den_MGT,list(x),sum)
  aa = a[,2]/t4$x *1000
  aa1 = t(rbind(a[,2],t4$x,aa))
  if(!anyNA(n)){rownames(aa1) = factor_names(n)}
  write.table(aa1,"t.csv",col.names=T, append = T, sep=",")
}

table3 = function(n=NA,x){
  x = factor(x , exclude = NULL)
  a = table(x,dat2$Defect_Not)
  t4 = aggregate(0.01*dat2$Traf_Den_MGT,list(x),sum)
  aa = a[,2]/t4$x *1000
  aa1 = t(rbind(a[,2],t4$x,aa))
  if(!anyNA(n)){rownames(aa1) = factor_names(n)}
  write.table(aa1,"t.csv",col.names=T, append = T, sep=",")
}
##########Get the name############
factor_names = function(n)
{
  n1 = NULL
  for (i in 1: length(n)-1)
    n1 = cbind(n1,paste(n[i],n[i+1],sep = " - "))
  write.table(n1[-1], "t1.csv", col.names=T, append = T, sep=",")
}



##### further investigate into the interaction between grinidng vs curvature, traffic density, as well as 
### grinding vs prior defect
### use the 1-mile data and beofe the log transformation

dat$Defect=ifelse(dat$Defect_Number!=0,1,0)
dat$Grinding_C = cut(dat$Grinding,c(0,1,2,3,Inf),include.lowest = F)
dat$Grinding_C=as.character(dat$Grinding_C)
nrow=which(dat$Grinding==0)
dat$Grinding_C[nrow]="0"

attach(dat)

table1=function()


  aa=table(dat$Grinding_C,dat$Defect)
bb=aggregate(dat$Defect,list(dat$Grinding_C),sum)

cc=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Grinding_C),sum)

Grind=data.frame(Grinding=cc[,1],Defect=cc[,2],Ton_Mile1=cc[,3],Length=cc[,4],Ton_Mile=cc[,5])
Grind=Grind[c(5,1:4),]
Grind$RateMile=Grind$Defect/Grind$Length
Grind$RateTonmile1=Grind$Defect/Grind$Ton_Mile1
Grind$RateTonmile=Grind$Defect/Grind$Ton_Mile
library(xlsx)
write.xlsx(Grind,file = "EDA_Grinding_Joint.xlsx",sheetName = "Grind",row.names = FALSE)

#### grinding vs prior defect

dat$PriorD_B=""
dat$PriorD_B[dat$Prior_Defect==0]=as.character("0")
dat$PriorD_B[dat$Prior_Defect==1]=as.character("1")
dat$PriorD_B[dat$Prior_Defect==2]=as.character("2")
dat$PriorD_B[dat$Prior_Defect==3]=as.character("3")
dat$PriorD_B[dat$Prior_Defect>3]=as.character(">3")
dat$PriorD_B=as.factor(dat$PriorD_B)
attach(dat)
dd2=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Grinding_C, PriorD_B),sum)
colnames(dd2)=c("Grinding","PriorD","DefectNumber","Ton_Mile1","Length","Ton_Mile")
d1 = xtabs( DefectNumber~ Grinding + PriorD, data=dd2 )
d2 = xtabs( Length~Grinding + PriorD, data=dd2 )
d3 = xtabs( Ton_Mile~Grinding + PriorD, data=dd2 )
d4 = xtabs( Ton_Mile1~Grinding + PriorD, data=dd2 )

write.table(d1,file = "tem.csv",row.names = T,append = T, sep = ",")
           
write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")

           

#### Grinding vs traffic density

dat$TF = cut(dat$Traffic_Density,c(0,10,20,30,40,Inf),include.lowest = T)
dd=aggregate(list(Grinding*Length,Grinding*Ton_Mile, Length,Ton_Mile),list(Grinding_C,TF),sum)
colnames(dd)=c("Grinding_C","Traffic","GrindMile","GrindTonmile","Length","Ton_Mile")


d1 = xtabs( GrindMile~ Grinding_C + Traffic, data=dd )
d2 = xtabs( GrindTonmile~Grinding_C + Traffic, data=dd )
d3 = xtabs( Length~Grinding_C + Traffic, data=dd )
d4 = xtabs( Ton_Mile~Grinding_C + Traffic, data=dd )

write.table(d1,file = "tem.csv",row.names = T,append = T, sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")


ee=aggregate(list(Grinding*Length,Grinding*Ton_Mile, Length,Ton_Mile),list(TF),sum)
GrindTF=data.frame(Traffic=ee[,1],GrindLen=ee[,2],GrindTonmile=ee[,3],Length=ee[,4],Ton_Mile=ee[,5])
GrindTF$RateLen=GrindTF$GrindLen/GrindTF$Length
GrindTF$RateTonmile=GrindTF$GrindTonmile/GrindTF$Ton_Mile
write.xlsx(GrindTF,file = "EDA_Grinding_Joint.xlsx",sheetName = "GrindTF",row.names = FALSE,append = T)


###defect rate by grinding & trffic density
dd=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Grinding_C,TF),sum)

colnames(dd)=c("Grinding","Traffic","DefectNumber","Ton_Mile1","Length","Ton_Mile")
d1 = xtabs( DefectNumber~ Grinding + Traffic, data=dd )
d2 = xtabs( Length~Grinding + Traffic, data=dd)
d3 = xtabs( Ton_Mile~Grinding + Traffic, data=dd)
d4 = xtabs( Ton_Mile1~Grinding + Traffic, data=dd )

write.table(d1,file = "tem.csv",row.names = T,append = T, sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")


### defect rate by grinding & curve degree

dat$CurveC = cut(dat$Curve_D,c(-3,0,2,5,Inf),include.lowest = T)
dat$CurveC=as.character(dat$CurveC)
dat$CurveC[dat$CurveC=="[-3,0]"]="0"
dat$CurveC=as.factor(dat$CurveC)

dd=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Grinding_C,CurveC),sum)

colnames(dd)=c("Grinding","Curve","DefectNumber","Ton_Mile1","Length","Ton_Mile")
d1 = xtabs( DefectNumber~ Grinding + Curve, data=dd )
d2 = xtabs( Length~Grinding + Curve, data=dd)
d3 = xtabs( Ton_Mile~Grinding + Curve, data=dd)
d4 = xtabs( Ton_Mile1~Grinding + Curve, data=dd )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")


##### grinding , traffic density, curve degree
dd=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Grinding_C,CurveC,TF),sum)
colnames(dd)=c("Grinding","Curve","Traffic","DefectNumber","Ton_Mile1","Length","Ton_Mile")
save=function(x){
d1 = xtabs( x~ Traffic + Curve, data=dd[dd[,1]=="0",] )
d2 = xtabs( x~ Traffic + Curve, data=dd[dd[,1]=="(0,1]",] )
d3 = xtabs( x~ Traffic + Curve, data=dd[dd[,1]=="(1,2]",] )
d4 = xtabs( x~ Traffic + Curve, data=dd[dd[,1]=="(2,3]",] )
d5 = xtabs( x~ Traffic + Curve, data=dd[dd[,1]=="(3,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d5,file = "tem.csv",row.names = T,append = T, sep = ",")
}

d1 = xtabs( Length~ Traffic + Curve, data=dd[dd[,1]=="0",] )
d2 = xtabs( Length~ Traffic + Curve, data=dd[dd[,1]=="(0,1]",] )
d3 = xtabs( Length~ Traffic + Curve, data=dd[dd[,1]=="(1,2]",] )
d4 = xtabs( Length~ Traffic + Curve, data=dd[dd[,1]=="(2,3]",] )
d5 = xtabs( Length~ Traffic + Curve, data=dd[dd[,1]=="(3,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d5,file = "tem.csv",row.names = T,append = T, sep = ",")

d1 = xtabs( Ton_Mile1~ Traffic + Curve, data=dd[dd[,1]=="0",] )
d2 = xtabs( Ton_Mile1~ Traffic + Curve, data=dd[dd[,1]=="(0,1]",] )
d3 = xtabs( Ton_Mile1~ Traffic + Curve, data=dd[dd[,1]=="(1,2]",] )
d4 = xtabs( Ton_Mile1~ Traffic + Curve, data=dd[dd[,1]=="(2,3]",] )
d5 = xtabs( Ton_Mile1~ Traffic + Curve, data=dd[dd[,1]=="(3,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d5,file = "tem.csv",row.names = T,append = T, sep = ",")

d1 = xtabs( Ton_Mile~ Traffic + Curve, data=dd[dd[,1]=="0",] )
d2 = xtabs( Ton_Mile~ Traffic + Curve, data=dd[dd[,1]=="(0,1]",] )
d3 = xtabs( Ton_Mile~ Traffic + Curve, data=dd[dd[,1]=="(1,2]",] )
d4 = xtabs( Ton_Mile~ Traffic + Curve, data=dd[dd[,1]=="(2,3]",] )
d5 = xtabs( Ton_Mile~ Traffic + Curve, data=dd[dd[,1]=="(3,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d5,file = "tem.csv",row.names = T,append = T, sep = ",")



###############curve , grind vs traffic density. fix the curve degree

d1 = xtabs( DefectNumber~ Traffic + Grinding, data=dd[dd[,2]=="0",] )
d2 = xtabs( DefectNumber~ Traffic + Grinding, data=dd[dd[,2]=="(0,2]",] )
d3 = xtabs( DefectNumber~ Traffic + Grinding, data=dd[dd[,2]=="(2,5]",] )
d4 = xtabs( DefectNumber~ Traffic + Grinding, data=dd[dd[,2]=="(5,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")


d1 = xtabs( Length~ Traffic + Grinding, data=dd[dd[,2]=="0",] )
d2 = xtabs( Length~ Traffic + Grinding, data=dd[dd[,2]=="(0,2]",] )
d3 = xtabs( Length~ Traffic + Grinding, data=dd[dd[,2]=="(2,5]",] )
d4 = xtabs( Length~ Traffic + Grinding, data=dd[dd[,2]=="(5,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")

d1 = xtabs( Ton_Mile1~ Traffic + Grinding, data=dd[dd[,2]=="0",] )
d2 = xtabs( Ton_Mile1~ Traffic + Grinding, data=dd[dd[,2]=="(0,2]",] )
d3 = xtabs( Ton_Mile1~ Traffic + Grinding, data=dd[dd[,2]=="(2,5]",] )
d4 = xtabs( Ton_Mile1~ Traffic + Grinding, data=dd[dd[,2]=="(5,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")


d1 = xtabs( Ton_Mile~ Traffic + Grinding, data=dd[dd[,2]=="0",] )
d2 = xtabs( Ton_Mile~ Traffic + Grinding, data=dd[dd[,2]=="(0,2]",] )
d3 = xtabs( Ton_Mile~ Traffic + Grinding, data=dd[dd[,2]=="(2,5]",] )
d4 = xtabs( Ton_Mile~ Traffic + Grinding, data=dd[dd[,2]=="(5,Inf]",] )

write.table(d1,file = "tem.csv",row.names = T,sep = ",")

write.table(d2,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d3,file = "tem.csv",row.names = T,append = T, sep = ",")
write.table(d4,file = "tem.csv",row.names = T,append = T, sep = ",")


##########JOint

dat$Joint_WeldW_C=cut(dat$Joint_WeldW_L,c(0,0.2,0.4,0.6,0.8,1),include.lowest = T)
dat$Joint_WeldW_C=as.factor(dat$Joint_WeldW_C)
cc=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(Joint_WeldW_C),sum)
Welded=data.frame(Welded=cc[,1],Defect=cc[,2],Ton_Mile1=cc[,3],Length=cc[,4],Ton_Mile=cc[,5])
Welded$RateMile=Welded$Defect/Welded$Length
Welded$RateTonmile1=Welded$Defect/Welded$Ton_Mile1
Welded$RateTonmile=Welded$Defect/Welded$Ton_Mile
write.xlsx(Welded,file = "EDA_Grinding_Joint.xlsx",sheetName = "Welded",row.names = FALSE,append = T)


###add a new variable the ratio between the length of welded and joint rail

dat$JWRatio[dat$Joint_WeldJ_L<=0.33]="<0.5"
dat$JWRatio[dat$Joint_WeldJ_L<0.5 & dat$Joint_WeldJ_L>0.33]="0.5-1"
dat$JWRatio[dat$Joint_WeldJ_L<0.67 & dat$Joint_WeldJ_L>=0.5]="1-2"
dat$JWRatio[dat$Joint_WeldJ_L>=0.67]=">2"
dat$JWRatio=as.factor(dat$JWRatio)
cc=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(JWRatio),sum)
Welded=data.frame(JvsW=cc[,1],Defect=cc[,2],Ton_Mile1=cc[,3],Length=cc[,4],Ton_Mile=cc[,5])

####Pure joint rail or pure welded rail
dat$JW=""
dat$JW[dat$Joint_WeldJ_L==1]=as.character("J")
dat$JW[dat$Joint_WeldJ_L==0]=as.character("W")
dat$JW[dat$Joint_WeldJ_L>dat$Joint_WeldW_L & dat$Joint_WeldJ_L!=1]="JW"
dat$JW[dat$Joint_WeldJ_L<dat$Joint_WeldW_L & dat$Joint_WeldJ_L!=0]="WJ"
dat$JW[dat$Joint_WeldJ_L==0.5]="J/W"
dat$JW=as.factor(dat$JW)
cc=aggregate(list(Defect,Traffic_Density,Length,Ton_Mile),list(JW),sum)
Welded=data.frame(JvsW=cc[,1],Defect=cc[,2],Ton_Mile1=cc[,3],Length=cc[,4],Ton_Mile=cc[,5])

Welded$RateMile=Welded$Defect/Welded$Length
Welded$RateTonmile1=Welded$Defect/Welded$Ton_Mile1
Welded$RateTonmile=Welded$Defect/Welded$Ton_Mile

Welded=Welded[order(Welded[,8]),]

write.xlsx(Welded,file = "EDA_Grinding_Joint.xlsx",sheetName = "JW combination",row.names = FALSE,append = T)























