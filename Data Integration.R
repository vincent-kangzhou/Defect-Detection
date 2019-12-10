
###### Function##############################################

#####Geometry

Geo_F=function(x,y){
  RRR<-which(x>= GEP$Min & x<=GEP$Max )
  GEP_int=GEP[RRR,]
  a_2011=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2009:2010))
  a_2012=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2010:2011))
  a_2013=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2011:2012))
  a_2014=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2012:2013))
  a_2015=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2013:2014))
  a_2016=length(which(as.numeric(format(GEP_int$Date,"%Y")) %in% 2014:2015))
  
  
  my_list=list(c(a_2011,a_2012,a_2013,a_2014,a_2015,a_2016))
  
  return(my_list)
  
}

#######VTI

VTI_F=function(x){
  
  VTI_Int=VTIP[which( VTIP$MILEPOST==x),]
  
  a_2011=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2010))
  a_2012=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2011))
  a_2013=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2012))
  a_2014=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2013))
  a_2015=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2014))
  a_2016=length(which(as.numeric(format(VTI_Int$EXCEPTION.DATE,"%Y"))==2015))
  
  
  my_list=list(c(a_2011,a_2012,a_2013,a_2014,a_2015,a_2016))
  
  return(my_list)
  
}


############ convert the column type

Convert_Type=function(dat){
  
  dat$Side<-as.character(dat$Side)
  dat$Defect_Type_2011=as.character(dat$Defect_Type_2011)
  dat$Defect_Type_2012=as.character(dat$Defect_Type_2012)
  dat$Defect_Type_2013=as.character(dat$Defect_Type_2013)
  dat$Defect_Type_2014<-as.character(dat$Defect_Type_2014)
  dat$Defect_Type_2015<-as.character(dat$Defect_Type_2015)
  dat$Defect_Type_2016<-as.character(dat$Defect_Type_2016)
  dat$Curve_Direction<-as.character(dat$Curve_Direction)
  dat$Traf_Den_2011_MGT=as.numeric(dat$Traf_Den_2011_MGT)
  dat$Traf_Den_2012_MGT=as.numeric(dat$Traf_Den_2012_MGT)
  dat$Traf_Den_2013_MGT=as.numeric(dat$Traf_Den_2013_MGT)
  dat$Traf_Den_2014_MGT=as.numeric(dat$Traf_Den_2014_MGT)
  dat$Traf_Den_2015_MGT=as.numeric(dat$Traf_Den_2015_MGT)
  dat$Traf_Den_2016_MGT=as.numeric(dat$Traf_Den_2016_MGT)
  dat$Ave_Car_Load_2011_Ton=as.numeric(dat$Ave_Car_Load_2011_Ton)
  dat$Ave_Car_Load_2012_Ton=as.numeric(dat$Ave_Car_Load_2012_Ton)
  dat$Ave_Car_Load_2013_Ton=as.numeric(dat$Ave_Car_Load_2013_Ton)
  dat$Ave_Car_Load_2014_Ton=as.numeric(dat$Ave_Car_Load_2014_Ton)
  dat$Ave_Car_Load_2015_Ton=as.numeric(dat$Ave_Car_Load_2015_Ton)
  dat$Ave_Car_Load_2016_Ton=as.numeric(dat$Ave_Car_Load_2016_Ton)
  dat$ Car_Pass_2011=as.numeric(dat$Car_Pass_2011)  
  dat$ Car_Pass_2012=as.numeric(dat$Car_Pass_2012)
  dat$ Car_Pass_2013=as.numeric(dat$Car_Pass_2013)
  dat$ Car_Pass_2014=as.numeric(dat$Car_Pass_2014)
  dat$ Car_Pass_2015=as.numeric(dat$Car_Pass_2015)
  dat$ Car_Pass_2016=as.numeric(dat$Car_Pass_2016)
  
  dat$Start_RailAge_2011_MGT=as.numeric(dat$Start_RailAge_2011_MGT)
  dat$Start_RailAge_2012_MGT=as.numeric(dat$Start_RailAge_2012_MGT)
  dat$Start_RailAge_2013_MGT=as.numeric(dat$Start_RailAge_2013_MGT)
  dat$Start_RailAge_2014_MGT=as.numeric(dat$Start_RailAge_2014_MGT)
  dat$Start_RailAge_2015_MGT=as.numeric(dat$Start_RailAge_2015_MGT)
  dat$Start_RailAge_2016_MGT=as.numeric(dat$Start_RailAge_2016_MGT)
  dat$Defect_RailAge_2011_MGT=as.numeric(dat$Defect_RailAge_2011_MGT)
  dat$Defect_RailAge_2012_MGT=as.numeric(dat$Defect_RailAge_2012_MGT)
  dat$Defect_RailAge_2013_MGT=as.numeric(dat$Defect_RailAge_2013_MGT)
  dat$Defect_RailAge_2014_MGT=as.numeric(dat$Defect_RailAge_2014_MGT)
  dat$Defect_RailAge_2015_MGT=as.numeric(dat$Defect_RailAge_2015_MGT)
  dat$Defect_RailAge_2016_MGT=as.numeric(dat$Defect_RailAge_2016_MGT)
  dat$Failure_RailAge_2011_MGT=as.numeric(dat$Failure_RailAge_2011_MGT)
  dat$Failure_RailAge_2012_MGT=as.numeric(dat$Failure_RailAge_2012_MGT)
  dat$Failure_RailAge_2013_MGT=as.numeric(dat$Failure_RailAge_2013_MGT)
  dat$Failure_RailAge_2014_MGT=as.numeric(dat$Failure_RailAge_2014_MGT)
  dat$Failure_RailAge_2015_MGT=as.numeric(dat$Failure_RailAge_2015_MGT)
  dat$Failure_RailAge_2016_MGT=as.numeric(dat$Failure_RailAge_2016_MGT)
  
  dat$Failure_Type_2011=as.character(dat$Failure_Type_2011)
  dat$Failure_Type_2012=as.character(dat$Failure_Type_2012)
  dat$Failure_Type_2013=as.character(dat$Failure_Type_2013)
  dat$Failure_Type_2014=as.character(dat$Failure_Type_2014)
  dat$Failure_Type_2015=as.character(dat$Failure_Type_2015)
  dat$Failure_Type_2016=as.character(dat$Failure_Type_2016)
  
  dat$VTI_2011=as.numeric(dat$VTI_2011)
  dat$VTI_2012=as.numeric(dat$VTI_2012)
  dat$VTI_2013=as.numeric(dat$VTI_2013)
  dat$VTI_2014=as.numeric(dat$VTI_2014)
  dat$VTI_2015=as.numeric(dat$VTI_2015)
  dat$VTI_2016=as.numeric(dat$VTI_2016)
  
  dat$F_Tem_2011=as.numeric(dat$F_Tem_2011)
  dat$F_Tem_2012=as.numeric(dat$F_Tem_2012)
  dat$F_Tem_2013=as.numeric(dat$F_Tem_2013)
  dat$F_Tem_2014=as.numeric(dat$F_Tem_2014)
  dat$F_Tem_2015=as.numeric(dat$F_Tem_2015)
  dat$F_Tem_2016=as.numeric(dat$F_Tem_2016)
  dat$Track_Type=as.character(dat$Track_Type)
  return(dat)
}


PL=unique(TF$TRACK_SEGMENT_PREFIX_I[TF$TRACK_TYPE_C=="1"])

##Aver_Fre=function(x,y){    #define a function 
##if ( y>0){
##Re=round(x*100/y,digits = 3)
##return(Re)
##}
##else {
##return(NA)
#}
#}
ptm=proc.time()
# for (k in 1: length(PL)) {
for (k in 1: 30) {
  ##RN<-which(TF$TRACK_SEGMENT_PREFIX_I==PL[k] & TF$TRACK_TYPE_C %in% c("1","2","3","4"))   # Trackfile_Row_: to see how many row are there satisfying :we just focus on SG track type and "ANB" prefix, only change the key word in the quotes to change selection types
  
  RN<-which(TF$TRACK_SEGMENT_PREFIX_I==PL[k] & TF$TRACK_TYPE_C=="1")   # Trackfile_Row_: to see how many row are there satisfying :we just focus on SG track type and "ANB" prefix, only change the key word in the quotes to change selection types
  TFP<-TF[RN,] # create a new subdatabase with the name of "TF_ANB", the only difference will be "ANB"
  TFP<-TFP[order(TFP$BEGIN_ENGINEER_MILEPOST_I),] #reorder the subdatabase "TF_ANB" by BEGIN_ENGINEER_MILEPOST_I ASC
  
  SR=sum(TFP$Interval)
  SSP=matrix(data = NA,nrow = 2*SR,ncol = length(COL_NAME))
  SSP=as.data.frame(SSP)
  names(SSP)=COL_NAME
  SSP=Convert_Type(SSP)
  
  ####### Single track
  
  SSP$Track_Type="1"
  
  
  Beg=1
  
  for (i in 1: nrow(TFP)){
    for (j in (Beg):sum(TFP$Interval[1:i]))
    {
      # print(j)
      SSP$Milepost[j]=round(as.numeric(TFP$BEGIN_ENGINEER_MILEPOST_I[i]+(j-sum(TFP$Interval[1:i-1])-1)*0.01),digits = 2)
    }
    Beg=round(sum(TFP$Interval[1:i])+1)
  }
  
  #distinguish the whole comprehensive database into two parts: left rail(upper half) and right rail(lower half).
  
  Half=nrow(SSP)/2
  Half1=nrow(SSP)/2+1
  SSP$Milepost[Half1:nrow(SSP)]=SSP$Milepost[1:Half]
  
  SSP$Side[1:Half]="L"
  SSP$Side[Half1:nrow(SSP)]="R"
  SSP$Milepost=round(SSP$Milepost,digits = 2)
  SSP$Prefix=PL[k]
  
  ##### create subsets of each database containing the prefix-specific records####
  
  DFP=subset(Defect_File,Defect_File$PREFIX==PL[k] & Defect_File$TRACK.TYPE=="1") # create a rail-defect subdatabase, and select targeted prefix. We only focus in the defects on single track.
  RLP=subset(RL,RL$TRACK_SEGMENT_PREFIX_I==PL[k] & RL$TRACK_TYPE_C=="1")   # create a subdataset to store the laid year with targeted prefix.
  
  
  CCP<-subset(CCurve,CCurve$TRACK_SEGMENT_PREFIX_I==PL[k] & CCurve$TRACK_TYPE_C=="1") # create a subdataset to store the curve feathures with targeted prefix.
  GGP<-subset(GGrade,GGrade$TRACK_SEGMENT_PREFIX_I==PL[k])# create a subdataset to store the grade features with targeted prefix.
  GRP<-subset(Grind,Grind$X.Customer.Line.Segment==PL[k] & Grind$X.Track.ID=="1") # create a subdataset to store the grinding information with targeted prefix.
  BAP<-subset(Ballast,Ballast$Corridor==PL[k] & Ballast$Track=="1") # create a subdataset to store the ballast cleaning information with targeted prefix.
  TTP=subset(TT,TT$Prefix==PL[k] & TT$TrackType=="1")
  TTP_EX=subset(TT,TT$TrackType=="%%" & TT$Prefix==PL[k])
  TFP=subset(TF,TF$TRACK_SEGMENT_PREFIX_I==PL[k])
  SGP=subset(SSignal,SSignal$Prefix==PL[k])
  TUP=subset(Turnout,Turnout$TRACK_SEGMENT_PREFIX_I==PL[k] & Turnout$TRACK_C=="M" & Turnout$TRACK_N=="1")
  GEP=subset(Geo_D,Geo_D$Prefix==PL[k] & Geo_D$Track=="1")   # Create a subset to hold the geometry defects with a specific prefix and track type
  
  # SDP=subset(Com_SP,Com_SP$Prefix==PL[k])
  # INP=subset(Inspection,Inspection$Prefix==PL[k])
  VTIP=subset(VTI,VTI$PREFIX==PL[k])
  FP=subset(Failure,Failure$PREFIX==PL[k]& Failure$TRACK.TYPE=="1" & Failure$SIDE %in% c("R", "L"))
  
  
  
  # We have rail defects records from 2011 to 2016. Select each of these six years for our analysis. Choose the first day of each year as the beginning of observasion
  
  
  SSP$Start_Date_2011<-as.Date("2011-01-01","%Y-%m-%d") 
  SSP$Start_Date_2012<-as.Date("2012-01-01","%Y-%m-%d") 
  SSP$Start_Date_2013<-as.Date("2013-01-01","%Y-%m-%d") 
  SSP$Start_Date_2014<-as.Date("2014-01-01","%Y-%m-%d") 
  SSP$Start_Date_2015<-as.Date("2015-01-01","%Y-%m-%d")
  SSP$Start_Date_2016<-as.Date("2016-01-01","%Y-%m-%d")
  
  #Defect_Not represents whether there were defects happened in the targeted year.
  #It is a binary variable. Set "0" as defult number.
  
  SSP$Defect_Not_2011<-0
  SSP$Defect_Not_2012<-0
  SSP$Defect_Not_2013<-0
  SSP$Defect_Not_2014<-0
  SSP$Defect_Not_2015<-0
  SSP$Defect_Not_2016<-0
  
  #fill in  six columns: Prior_Defect_2011, Prior_Defect_2012, Prior_Defect_2013, Prior_Defect_2014, Prior_Defect_2015, Prior_Defect_2016, Test_Date_2011, Test_Date_2012, Test_Date_2013,
  ##Test_Date_2014,Test_Date_2015,Test_Date_2016, Defect_Not_2011, Defect_Not_2012, Defect_Not_2013, Defect_Not_2014,Defect_Not_2015,Defect_Not_2016.
  
  DFP<-DFP[order(DFP[,8],decreasing = TRUE),]  ##  order the defects by the prefix, track type, and test date
  "%%"=function(x,y) paste(x,y,sep = "",collapse = NULL)
  
  if (nrow(DFP)>=1) {
    for (i in 1: nrow(DFP)) {
      #select the row number of the defect records in the comprehensive database. Those defects occured in these locations
      Num_row<-which(SSP$Milepost==floor(DFP$MP[i]*100)/100 & SSP$Side==DFP$SIDE[i])
      CN1="Defect_Not_"%% as.character(format(DFP$DATE.FOUND[i],"%Y"))
      CN2="Test_Date_"%% as.character(format(DFP$DATE.FOUND[i],"%Y"))
      CN3="Defect_Type_"%% as.character(format(DFP$DATE.FOUND[i],"%Y"))
      
      ### CN1<-paste("Defect_Not_",as.character(format(DFP$DATE.FOUND[i],"%Y")),sep = "",collapse = NULL) # check which year did the defects happen.
      ### CN2<-paste("Test_Date_",as.character(format(DFP$DATE.FOUND[i],"%Y")),sep = "",collapse = NULL)  # check which year did the defects happen.
      ### CN3<-paste("Defect_Type_",as.character(format(DFP$DATE.FOUND[i],"%Y")),sep = "",collapse = NULL) #check which year did the defects happen.
      SSP[Num_row,CN1]<-1 #fill in "1" to Defect_Not in the targeted year.
      SSP[Num_row,CN2]<-format(DFP$DATE.FOUND[i],"%Y-%m-%d") #fill in test dates to Test_Date in the targeted year.
      SSP[Num_row,CN3]<-DFP$DEFECT.TYPE[i]
    }
  }
  ## if there is no defect detected out, the test date is the last day of the test year.
  SSP$Test_Date_2011[SSP$Defect_Not_2011==0]<-as.character("2011-12-31")
  SSP$Test_Date_2012[SSP$Defect_Not_2012==0]<-as.character("2012-12-31")
  SSP$Test_Date_2013[SSP$Defect_Not_2013==0]<-as.character("2013-12-31")
  SSP$Test_Date_2014[SSP$Defect_Not_2014==0]<-as.character("2014-12-31")
  SSP$Test_Date_2015[SSP$Defect_Not_2015==0]<-as.character("2015-12-31")
  SSP$Test_Date_2016[SSP$Defect_Not_2016==0]<-as.character("2016-12-31")
  #transfer these six columns: Test_Date_2011, Test_Date_2012, Test_Date_2013, Test_Date_2014, Test_Date_2015, and Test_Date_2016, into Data type.
  SSP$Test_Date_2011<-as.Date(SSP$Test_Date_2011,"%Y-%m-%d") 
  SSP$Test_Date_2012<-as.Date(SSP$Test_Date_2012,"%Y-%m-%d")
  SSP$Test_Date_2013<-as.Date(SSP$Test_Date_2013,"%Y-%m-%d")
  SSP$Test_Date_2014<-as.Date(SSP$Test_Date_2014,"%Y-%m-%d") 
  SSP$Test_Date_2015<-as.Date(SSP$Test_Date_2015,"%Y-%m-%d")
  SSP$Test_Date_2016<-as.Date(SSP$Test_Date_2016,"%Y-%m-%d")
  
  ##########prior defect before the test date, the trace back period is two years, about 730 days.######
  ##That means if the prior defects occurrs more than 730 days earlier than this targeted defect, we assume that the prior defects
  ###has no impact on this targeted defect####
  
  for (i in 1: nrow(SSP)) {
    #count how many defects happented before the test dates in the targeted year.
    ###use 730 days as the trace back period
    #Num_row_2011<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2011[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2011[i]-DFP$DATE.FOUND)>0)
    #Num_row_2012<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2012[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2012[i]-DFP$DATE.FOUND)>0)
    #Num_row_2013<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2013[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2013[i]-DFP$DATE.FOUND)>0)
    #Num_row_2014<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2014[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2014[i]-DFP$DATE.FOUND)>0) 
    #Num_row_2015<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2015[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2015[i]-DFP$DATE.FOUND)>0)
    #Num_row_2016<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(SSP$Test_Date_2016[i]-DFP$DATE.FOUND)<=730 &  as.numeric(SSP$Test_Date_2016[i]-DFP$DATE.FOUND)>0)
    
    ### use two years as the track back period
    Num_row_2011<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2009:2010)
    Num_row_2012<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2010:2011)
    Num_row_2013<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2011:2012)
    Num_row_2014<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2012:2013) 
    Num_row_2015<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2013:2014)
    Num_row_2016<-which(SSP$Milepost[i]==floor(DFP$MP*100)/100 & SSP$Side[i]==DFP$SIDE & as.numeric(format(DFP$DATE.FOUND,"%Y")) %in% 2014:2015)
    
    #Assgin the number of previous defects we count to Prior_Defect column according to their year respectively. Prior_Defect represents the number of defects happened before the test dates, becuase we assume that pre-defects would affect the occurrence of post-defects or service failures on either side of the rail.
    ### For one location, remove the repats of defects with the same type and test date. 
    SSP$Prior_Defect_2011[i]<-length(Num_row_2011)
    SSP$Prior_Defect_2012[i]<-length(Num_row_2012)
    SSP$Prior_Defect_2013[i]<-length(Num_row_2013)
    SSP$Prior_Defect_2014[i]<-length(Num_row_2014)
    SSP$Prior_Defect_2015[i]<-length(Num_row_2015)
    SSP$Prior_Defect_2016[i]<-length(Num_row_2016)
  }
  
  
  ###  count the geometry defects which occurred before the test date. No distinguish about which side of the track the geometry defect occurs
  Geo_Y=paste("Geo_Defect_",Year_Ve,sep = "",collapse = NULL)
  
  
  
  
  ###use two years as the trace back period
  
  #Num_row_2011<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2009:2010)
  #Num_row_2012<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2010:2011)
  #Num_row_2013<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2011:2012)
  #Num_row_2014<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2012:2013)
  #Num_row_2015<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2013:2014)
  #Num_row_2016<-which(SSP$Milepost[i] %in% min(GEP$B_MP,GEP$E_MP):max(GEP$B_MP,GEP$E_MP) & as.numeric(format(GEP$Date,"%Y")) %in% 2014:2015)
  
  GGG=matrix(data=unlist(lapply(SSP[,"Milepost"], function(z) Geo_F(z))),ncol = 6,byrow = T)
  SSP[,Geo_Y]=GGG[,1:6]
  
  SSP[,Geo_Y]=unlist(SSP[,Geo_Y])
  
  
  
  ####   Rail Laid Year information
  
  for (i in 1: nrow(RLP)){
    Num_row=which(SSP$Milepost>=RLP$BEGIN_ENGINEER_MILEPOST_I[i] & SSP$Milepost<RLP$END_ENGINEER_MILEPOST_I[i] & SSP$Side==RLP$RAIL_SIDE_C[i])
    SSP$Laid_Year[Num_row]=format(RLP$RAIL_LAID_D[i],"%Y-%m-%d")
    SSP$Rail_Size_lbs_yard[Num_row]<-RLP$RAIL_WEIGHT_Q[i]
    SSP$Joint_Weld[Num_row]<-RLP$JOINT_WELD_C[i]
    SSP$Rail_Quality[Num_row]=RLP$NEW_RELAY_C[i]
  }
  
  SSP$Laid_Year<-as.Date(SSP$Laid_Year,"%Y-%m-%d")
  SSP$Rail_Size_lbs_yard=as.numeric(SSP$Rail_Size_lbs_yard)
  SSP$Joint_Weld[SSP$Joint_Weld==""]=NA
  SSP$Rail_Quality[SSP$Rail_Quality==""]=NA
  
  
  # for (i in 1: nrow(SSP)){   # fill "laid year, rail weight, joint_weld" from "Rail Laid" database
  #  Num_row<-which(SSP$Milepost[i]>=RLP$BEGIN_ENGINEER_MILEPOST_I & SSP$Milepost[i]<RLP$END_ENGINEER_MILEPOST_I & SSP$Side[i]==RLP$RAIL_SIDE_C)
  # if (length(Num_row)==1) {#fill in "laid year, rail weight, joint_weld" from "Rail Laid" database.
  #  SSP$Laid_Year[i]<-format(RLP$RAIL_LAID_D[Num_row],"%Y-%m-%d")
  #  SSP$Rail_Size_lbs_yard[i]<-RLP$RAIL_WEIGHT_Q[Num_row]
  ## SSP$Joint_Weld[i]<-RLP$JOINT_WELD_C[Num_row]
  #  SSP$Rail_Quality[i]=RLP$NEW_RELAY_C[Num_row]
  #}
  
  #  }
  # SSP$Laid_Year<-as.Date(SSP$Laid_Year,"%Y-%m-%d")
  
  ### Traffic information
  Tra_F=function (x, y){
    Num_row1=which(x>=TTP$B_MP & x<TTP$E_MP & TTP$YEAR==y)
    Num_row2=which(x>=TTP_EX$B_MP & x<TTP_EX$E_MP & TTP_EX$YEAR==y)
    Type_Num=length(unique(TFP$TRACK_TYPE_C[which(x>=TFP$BEGIN_ENGINEER_MILEPOST_I & x<TFP$END_ENGINEER_MILEPOST_I)]))
    RE=sum(TTP$G_Ton[Num_row1])+sum(TTP_EX$G_Ton[Num_row2])/Type_Num
    return(RE)
  }
  
  
  Car_Pass_F=function(x,y){
    Num_row1=which(x>=TTP$B_MP & x<TTP$E_MP & TTP$YEAR==y)
    Num_row2=which(x>=TTP_EX$B_MP & x<TTP_EX$E_MP & TTP_EX$YEAR==y)
    Type_Num=length(unique(TFP$TRACK_TYPE_C[which(x>=TFP$BEGIN_ENGINEER_MILEPOST_I & x<TFP$END_ENGINEER_MILEPOST_I)]))
    Tot=sum(TTP$CARS_Q[Num_row1])+sum(TTP_EX$CARS_Q[Num_row2])/Type_Num
    return(Tot)
  }
  
  
  
  Car_F=function(z,w){
    if (w>0) {
      RE=z/w
      return(RE)
    }
    else {
      return(NA)
    }
  }
  
  Year_Ve=c("2011","2012","2013","2014","2015","2016")
  Den_Year=paste("Traf_Den_", Year_Ve, "_MGT",sep = "",collapse = NULL)
  Ave_Car_Year=paste("Ave_Car_Load_",Year_Ve,"_Ton",sep = "",collapse = NULL) 
  Car_Pass=paste("Car_Pass_",Year_Ve,sep = "",collapse = NULL)
  
  for (i in 1 : nrow(SSP)){   #  fill "traffic density, total car passes" based on "TT" traffic tonnage database. 
    
    SSP[i,Den_Year]=lapply(Year_Ve, function(z) Tra_F(SSP$Milepost[i],z))
    SSP[i,Car_Pass]=lapply(Year_Ve, function(z) Car_Pass_F(SSP$Milepost[i],z))
    SSP[i,Ave_Car_Year]=lapply(Year_Ve, function(z) Car_F(SSP[i,paste("Traf_Den_", z , "_MGT",sep = "",collapse = NULL)],SSP[i,paste("Car_Pass_", z ,sep = "",collapse = NULL)]))
    
    ## SSP$Ave_Car_Load_2011_Ton[i]<-sum(TTP$CARS_Q[Num_row_2011])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2011")])/Type_Num
    #SSP$Ave_Car_Load_2012_Ton[i]<-sum(TTP$CARS_Q[Num_row_2012])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2012")])/Type_Num
    #SSP$Ave_Car_Load_2013_Ton[i]<-sum(TTP$CARS_Q[Num_row_2013])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2013")])/Type_Num
    #SSP$Ave_Car_Load_2014_Ton[i]<-sum(TTP$CARS_Q[Num_row_2014])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2014")])/Type_Num
    #SSP$Ave_Car_Load_2015_Ton[i]<-sum(TTP$CARS_Q[Num_row_2015])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2015")])/Type_Num
    #SSP$Ave_Car_Load_2016_Ton[i]<-sum(TTP$CARS_Q[Num_row_2016])+ sum(TTP_EX$CARS_Q[which(SSP$Milepost[i]>=TTP_EX$B_MP & SSP$Milepost[i]<TTP_EX$E_MP & TTP_EX$YEAR=="2016")])/Type_Num
  }
  
  SSP[,Den_Year]=round(SSP[,Den_Year]/1000000,digits = 2)
  SSP[,Ave_Car_Year]=round(SSP[,Ave_Car_Year],digits = 2)
  
  ###### curve information#######
  ### if one segment location matches with two curve records, we will leave the curve information in this location blank. And when we do analysis, we will remove all the recoreds with "NA"
  
  for (i in 1: nrow(SSP)) {   #  fill the curve information here. 
    Num_row<-which(SSP$Milepost[i]>=CCP$BEGIN_ENGINEER_MILEPOST_I & SSP$Milepost[i]<CCP$END_ENGINEER_MILEPOST_I)
    if (length(Num_row)==1) {## In the curve database, it is possible that for one location, two different curve records are for one location.
      # if there are two different curve records for one location, we ignore it. When we do the data analysis, we will delete this location. 
      SSP$Curve_Degree_D[i]<-CCP$CURVE_DEGREES_Q[Num_row]
      SSP$Curve_Direction[i]<-CCP$CURVE_DIRECTION_C[Num_row]
      SSP$Curve_Length_mile[i]<-CCP$END_ENGINEER_MILEPOST_I[Num_row]-CCP$BEGIN_ENGINEER_MILEPOST_I[Num_row]
      SSP$Curve_Ele[i]<-CCP$CURVE_SUPERELEVATION_Q[Num_row]
      SSP$Curve_Offset_ft[i]<-CCP$OFFSET_FEET_M[Num_row]
    }
    if (length(Num_row)==0) {#If there is no curve in this location: set Curve_degree, Curve_Ele, Curve_Offset_ft, and Curve_Length to "0," and set Curve_Direction to "T".
      SSP$Curve_Degree_D[i]<-0
      SSP$Curve_Direction[i]<-"T"
      SSP$Curve_Length_mile[i]<-0
      SSP$Curve_Ele[i]<-0
      SSP$Curve_Offset_ft[i]<-0
    }
  }
  
  ######### sometimes in the curve database, when the curve degree equal to zero, the curve direction is not "tangent". We should correct this
  
  SSP$Curve_Direction[SSP$Curve_Degree_D==0]="T"
  
  ########### grade database###########
  
  for (i in 1:nrow(SSP)) { # fill in grade information from "Grade" database.
    Num_row<-which(SSP$Milepost[i]>=GGP$BEGIN_ENGINEER_MILEPOST_I & SSP$Milepost[i]< GGP$END_ENGINEER_MILEPOST_I)
    if (length(Num_row)==1){
      SSP$Grade_Percent[i]<-GGP$BOUNDRY_C[Num_row]
      SSP$Grade_Length_mile[i]<-GGP$END_ENGINEER_MILEPOST_I[Num_row]-GGP$BEGIN_ENGINEER_MILEPOST_I[Num_row]
    }
    if (length(Num_row)==0){
      
      SSP$Grade_Percent[i]<-0
      SSP$Grade_Length_mile[i]<-0
    }
  }
  
  ## check whether there is signal setting.
  
  SSP$Signal=0
  for (i in 1: nrow(SGP)){
    Num_row=which(SSP$Milepost>=SGP$From.MP[i] & SSP$Milepost<SGP$To.MP[i])
    SSP$Signal_Code[Num_row]=SGP$Code[i]
  }
  
  #### signaled: "CP" , "YL-S", "TWC-ABS", "COT", "CP/CSS"
  S1=c("CP","YL-S","TWC-ABS","COT","CP/CSS")
  
  ### Non-signaled:  "TWC-D","YL"
  S2= c("TWC-D","YL") 
  
  ### Unkown  "OTMT", "NSRULES", "UPRULES", "NORAC", "METRA", "TC", "CNRULES", "CROR-OCS", "CROR-94",  "TRK-OOS",  "311"  
  S3=unique(SSignal$Code)
  S3=S3[(! S3 %in% S1) & (! S3 %in% S2) ]
  
  SSP$Signal[which(SSP$Signal_Code %in% S1)]=1
  SSP$Signal[which(SSP$Signal_Code %in% S2)]=0
  SSP$Signal[which(SSP$Signal_Code %in% S3)]=2
  
  
  # If the curve direction is "T"(no curve), then its curve tangent is also "T". If there is a curve here, then its curve tangent will depend on: if the curve direction and side have the same value, then the curve tangent will be "L" (low). If the curve direction and its side have different values, the curve tangent will be "H" (high).
  SSP$Curve_Tangent[SSP$Curve_Direction=="T"]="T"
  SSP$Curve_Tangent[SSP$Curve_Direction!="T" & SSP$Curve_Direction!=SSP$Side]="H"
  SSP$Curve_Tangent[SSP$Curve_Direction!="T" & SSP$Curve_Direction==SSP$Side]="L"
  
  # We fill in Defect_RailAge, Start_RaiAge from 2011 to 2016 to this location respectively.
  
  Num_row1=which(SSP$Start_Date_2011>=SSP$Laid_Year)    #   laid year is less than the start point of 2011, which is the least time
  
  SSP$Start_RailAge_2011_MGT[Num_row1]=SSP$Traf_Den_2011_MGT[Num_row1]*as.numeric(SSP$Start_Date_2011[Num_row1]-SSP$Laid_Year[Num_row1])/365
  SSP$Defect_RailAge_2011_MGT[Num_row1]=SSP$Traf_Den_2011_MGT[Num_row1]*as.numeric(SSP$Test_Date_2011[Num_row1]-SSP$Laid_Year[Num_row1])/365
  SSP$Start_RailAge_2012_MGT[Num_row1]=SSP$Start_RailAge_2011_MGT[Num_row1]+SSP$Traf_Den_2011_MGT[Num_row1]
  SSP$Defect_RailAge_2012_MGT[Num_row1]=SSP$Start_RailAge_2012_MGT[Num_row1]+SSP$Traf_Den_2012_MGT[Num_row1]*as.numeric(SSP$Test_Date_2012[Num_row1]-SSP$Start_Date_2012[Num_row1])/365
  SSP$Start_RailAge_2013_MGT[Num_row1]=SSP$Start_RailAge_2012_MGT[Num_row1]+SSP$Traf_Den_2012_MGT[Num_row1]
  SSP$Defect_RailAge_2013_MGT[Num_row1]=SSP$Start_RailAge_2013_MGT[Num_row1]+SSP$Traf_Den_2013_MGT[Num_row1]*as.numeric(SSP$Test_Date_2013[Num_row1]-SSP$Start_Date_2013[Num_row1])/365
  SSP$Start_RailAge_2014_MGT[Num_row1]=SSP$Start_RailAge_2013_MGT[Num_row1]+SSP$Traf_Den_2013_MGT[Num_row1]
  SSP$Defect_RailAge_2014_MGT[Num_row1]=SSP$Start_RailAge_2014_MGT[Num_row1]+SSP$Traf_Den_2014_MGT[Num_row1]*as.numeric(SSP$Test_Date_2014[Num_row1]-SSP$Start_Date_2014[Num_row1])/365
  SSP$Start_RailAge_2015_MGT[Num_row1]=SSP$Start_RailAge_2014_MGT[Num_row1]+SSP$Traf_Den_2014_MGT[Num_row1]
  SSP$Defect_RailAge_2015_MGT[Num_row1]=SSP$Start_RailAge_2015_MGT[Num_row1]+SSP$Traf_Den_2015_MGT[Num_row1]*as.numeric(SSP$Test_Date_2015[Num_row1]-SSP$Start_Date_2015[Num_row1])/365
  SSP$Start_RailAge_2016_MGT[Num_row1]=SSP$Start_RailAge_2015_MGT[Num_row1]+SSP$Traf_Den_2015_MGT[Num_row1]
  SSP$Defect_RailAge_2016_MGT[Num_row1]=SSP$Start_RailAge_2016_MGT[Num_row1]+SSP$Traf_Den_2016_MGT[Num_row1]*as.numeric(SSP$Test_Date_2016[Num_row1]-SSP$Start_Date_2016[Num_row1])/365
  
  Num_row2=which(SSP$Start_Date_2011<SSP$Laid_Year & SSP$Test_Date_2011>=SSP$Laid_Year)   #  laid year is located between the start time of 2011 and test time of 2011
  
  SSP$Start_RailAge_2011_MGT[Num_row2]=0
  SSP$Defect_RailAge_2011_MGT[Num_row2]=SSP$Traf_Den_2011_MGT[Num_row2]*as.numeric(SSP$Test_Date_2011[Num_row2]-SSP$Laid_Year[Num_row2])/365
  SSP$Start_RailAge_2012_MGT[Num_row2]=SSP$Defect_RailAge_2011_MGT[Num_row2]+SSP$Traf_Den_2011_MGT[Num_row2]*as.numeric(SSP$Start_Date_2012[Num_row2]-SSP$Test_Date_2011[Num_row2])/365
  SSP$Defect_RailAge_2012_MGT[Num_row2]=SSP$Start_RailAge_2012_MGT[Num_row2]+SSP$Traf_Den_2012_MGT[Num_row2]*as.numeric(SSP$Test_Date_2012[Num_row2]-SSP$Start_Date_2012[Num_row2])/365
  SSP$Start_RailAge_2013_MGT[Num_row2]=SSP$Start_RailAge_2012_MGT[Num_row2]+SSP$Traf_Den_2012_MGT[Num_row2]
  SSP$Defect_RailAge_2013_MGT[Num_row2]=SSP$Start_RailAge_2013_MGT[Num_row2]+SSP$Traf_Den_2013_MGT[Num_row2]*as.numeric(SSP$Test_Date_2013[Num_row2]-SSP$Start_Date_2013[Num_row2])/365
  SSP$Start_RailAge_2014_MGT[Num_row2]=SSP$Start_RailAge_2013_MGT[Num_row2]+SSP$Traf_Den_2013_MGT[Num_row2]
  SSP$Defect_RailAge_2014_MGT[Num_row2]=SSP$Start_RailAge_2014_MGT[Num_row2]+SSP$Traf_Den_2014_MGT[Num_row2]*as.numeric(SSP$Test_Date_2014[Num_row2]-SSP$Start_Date_2014[Num_row2])/365
  SSP$Start_RailAge_2015_MGT[Num_row2]=SSP$Start_RailAge_2014_MGT[Num_row2]+SSP$Traf_Den_2014_MGT[Num_row2]
  SSP$Defect_RailAge_2015_MGT[Num_row2]=SSP$Start_RailAge_2015_MGT[Num_row2]+SSP$Traf_Den_2015_MGT[Num_row2]*as.numeric(SSP$Test_Date_2015[Num_row2]-SSP$Start_Date_2015[Num_row2])/365
  SSP$Start_RailAge_2016_MGT[Num_row2]=SSP$Start_RailAge_2015_MGT[Num_row2]+SSP$Traf_Den_2015_MGT[Num_row2]
  SSP$Defect_RailAge_2016_MGT[Num_row2]=SSP$Start_RailAge_2016_MGT[Num_row2]+SSP$Traf_Den_2016_MGT[Num_row2]*as.numeric(SSP$Test_Date_2016[Num_row2]-SSP$Start_Date_2016[Num_row2])/365
  
  Num_row3=which(SSP$Start_Date_2012<=SSP$Laid_Year & SSP$Test_Date_2012>=SSP$Laid_Year)
  
  SSP$Start_RailAge_2011_MGT[Num_row3]=0
  SSP$Defect_RailAge_2011_MGT[Num_row3]=0
  SSP$Start_RailAge_2012_MGT[Num_row3]=0
  SSP$Defect_RailAge_2012_MGT[Num_row3]=SSP$Traf_Den_2012_MGT[Num_row3]*as.numeric(SSP$Test_Date_2012[Num_row3]-SSP$Laid_Year[Num_row3])/365
  SSP$Start_RailAge_2013_MGT[Num_row3]=SSP$Defect_RailAge_2012_MGT[Num_row3]+SSP$Traf_Den_2012_MGT[Num_row3]*as.numeric(SSP$Start_Date_2013[Num_row3]-SSP$Test_Date_2012[Num_row3])/365
  SSP$Defect_RailAge_2013_MGT[Num_row3]=SSP$Start_RailAge_2013_MGT[Num_row3]+SSP$Traf_Den_2013_MGT[Num_row3]*as.numeric(SSP$Test_Date_2013[Num_row3]-SSP$Start_Date_2013[Num_row3])/365
  SSP$Start_RailAge_2014_MGT[Num_row3]=SSP$Start_RailAge_2013_MGT[Num_row3]+SSP$Traf_Den_2013_MGT[Num_row3]
  SSP$Defect_RailAge_2014_MGT[Num_row3]=SSP$Start_RailAge_2014_MGT[Num_row3]+SSP$Traf_Den_2014_MGT[Num_row3]*as.numeric(SSP$Test_Date_2014[Num_row3]-SSP$Start_Date_2014[Num_row3])/365
  SSP$Start_RailAge_2015_MGT[Num_row3]=SSP$Start_RailAge_2014_MGT[Num_row3]+SSP$Traf_Den_2014_MGT[Num_row3]
  SSP$Defect_RailAge_2015_MGT[Num_row3]=SSP$Start_RailAge_2015_MGT[Num_row3]+SSP$Traf_Den_2015_MGT[Num_row3]*as.numeric(SSP$Test_Date_2015[Num_row3]-SSP$Start_Date_2015[Num_row3])/365
  SSP$Start_RailAge_2016_MGT[Num_row3]=SSP$Start_RailAge_2015_MGT[Num_row3]+SSP$Traf_Den_2015_MGT[Num_row3]
  SSP$Defect_RailAge_2016_MGT[Num_row3]=SSP$Start_RailAge_2016_MGT[Num_row3]+SSP$Traf_Den_2016_MGT[Num_row3]*as.numeric(SSP$Test_Date_2016[Num_row3]-SSP$Start_Date_2016[Num_row3])/365
  
  Num_row4=which(SSP$Start_Date_2013<=SSP$Laid_Year & SSP$Test_Date_2013>=SSP$Laid_Year)
  SSP$Start_RailAge_2011_MGT[Num_row4]=0
  SSP$Defect_RailAge_2011_MGT[Num_row4]=0
  SSP$Start_RailAge_2012_MGT[Num_row4]=0
  SSP$Defect_RailAge_2012_MGT[Num_row4]=0
  SSP$Start_RailAge_2013_MGT[Num_row4]=0
  SSP$Defect_RailAge_2013_MGT[Num_row4]=SSP$Traf_Den_2013_MGT[Num_row4]*as.numeric(SSP$Test_Date_2013[Num_row4]-SSP$Laid_Year[Num_row4])/365
  SSP$Start_RailAge_2014_MGT[Num_row4]=SSP$Defect_RailAge_2013_MGT[Num_row4]+SSP$Traf_Den_2013_MGT[Num_row4]*as.numeric(SSP$Start_Date_2014[Num_row4]-SSP$Test_Date_2013[Num_row4])/365
  SSP$Defect_RailAge_2014_MGT[Num_row4]=SSP$Start_RailAge_2014_MGT[Num_row4]+SSP$Traf_Den_2014_MGT[Num_row4]*as.numeric(SSP$Test_Date_2014[Num_row4]-SSP$Start_Date_2014[Num_row4])/365
  SSP$Start_RailAge_2015_MGT[Num_row4]=SSP$Start_RailAge_2014_MGT[Num_row4]+SSP$Traf_Den_2014_MGT[Num_row4]
  SSP$Defect_RailAge_2015_MGT[Num_row4]=SSP$Start_RailAge_2015_MGT[Num_row4]+SSP$Traf_Den_2015_MGT[Num_row4]*as.numeric(SSP$Test_Date_2015[Num_row4]-SSP$Start_Date_2015[Num_row4])/365
  SSP$Start_RailAge_2016_MGT[Num_row4]=SSP$Start_RailAge_2015_MGT[Num_row4]+SSP$Traf_Den_2015_MGT[Num_row4]
  SSP$Defect_RailAge_2016_MGT[Num_row4]=SSP$Start_RailAge_2016_MGT[Num_row4]+SSP$Traf_Den_2016_MGT[Num_row4]*as.numeric(SSP$Test_Date_2016[Num_row4]-SSP$Start_Date_2016[Num_row4])/365
  
  Num_row5=which(SSP$Start_Date_2014<=SSP$Laid_Year & SSP$Test_Date_2014>=SSP$Laid_Year)
  SSP$Start_RailAge_2011_MGT[Num_row5]=0
  SSP$Defect_RailAge_2011_MGT[Num_row5]=0
  SSP$Start_RailAge_2012_MGT[Num_row5]=0
  SSP$Defect_RailAge_2012_MGT[Num_row5]=0
  SSP$Start_RailAge_2013_MGT[Num_row5]=0
  SSP$Defect_RailAge_2013_MGT[Num_row5]=0
  SSP$Start_RailAge_2014_MGT[Num_row5]=0
  SSP$Defect_RailAge_2014_MGT[Num_row5]=SSP$Traf_Den_2014_MGT[Num_row5]*as.numeric(SSP$Test_Date_2014[Num_row5]-SSP$Laid_Year[Num_row5])/365
  SSP$Start_RailAge_2015_MGT[Num_row5]=SSP$Defect_RailAge_2014_MGT[Num_row5]+SSP$Traf_Den_2014_MGT[Num_row5]*as.numeric(SSP$Start_Date_2015[Num_row5]-SSP$Test_Date_2014[Num_row5])/365
  SSP$Defect_RailAge_2015_MGT[Num_row5]=SSP$Start_RailAge_2015_MGT[Num_row5]+SSP$Traf_Den_2015_MGT[Num_row5]*as.numeric(SSP$Test_Date_2015[Num_row5]-SSP$Start_Date_2015[Num_row5])/365
  SSP$Start_RailAge_2016_MGT[Num_row5]=SSP$Start_RailAge_2015_MGT[Num_row5]+SSP$Traf_Den_2015_MGT[Num_row5]
  SSP$Defect_RailAge_2016_MGT[Num_row5]=SSP$Start_RailAge_2016_MGT[Num_row5]+SSP$Traf_Den_2016_MGT[Num_row5]*as.numeric(SSP$Test_Date_2016[Num_row5]-SSP$Start_Date_2016[Num_row5])/365
  
  Num_row6=which(SSP$Start_Date_2015<=SSP$Laid_Year & SSP$Test_Date_2015>=SSP$Laid_Year)
  SSP$Start_RailAge_2011_MGT[Num_row6]=0
  SSP$Defect_RailAge_2011_MGT[Num_row6]=0
  SSP$Start_RailAge_2012_MGT[Num_row6]=0
  SSP$Defect_RailAge_2012_MGT[Num_row6]=0
  SSP$Start_RailAge_2013_MGT[Num_row6]=0
  SSP$Defect_RailAge_2013_MGT[Num_row6]=0
  SSP$Start_RailAge_2014_MGT[Num_row6]=0
  SSP$Defect_RailAge_2014_MGT[Num_row6]=0
  SSP$Start_RailAge_2015_MGT[Num_row6]=0
  SSP$Defect_RailAge_2015_MGT[Num_row6]=SSP$Traf_Den_2015_MGT[Num_row6]*as.numeric(SSP$Test_Date_2015[Num_row6]-SSP$Laid_Year[Num_row6])/365
  SSP$Start_RailAge_2016_MGT[Num_row6]=SSP$Defect_RailAge_2015_MGT[Num_row6]+SSP$Traf_Den_2015_MGT[Num_row6]*as.numeric(SSP$Start_Date_2016[Num_row6]-SSP$Test_Date_2015[Num_row6])/365
  SSP$Defect_RailAge_2016_MGT[Num_row6]=SSP$Start_RailAge_2016_MGT[Num_row6]+SSP$Traf_Den_2016_MGT[Num_row6]*as.numeric(SSP$Test_Date_2016[Num_row6]-SSP$Start_Date_2016[Num_row6])/365
  
  Num_row7=which(SSP$Start_Date_2016<=SSP$Laid_Year & SSP$Test_Date_2016>=SSP$Laid_Year)
  SSP$Start_RailAge_2011_MGT[Num_row7]=0
  SSP$Defect_RailAge_2011_MGT[Num_row7]=0
  SSP$Start_RailAge_2012_MGT[Num_row7]=0
  SSP$Defect_RailAge_2012_MGT[Num_row7]=0
  SSP$Start_RailAge_2013_MGT[Num_row7]=0
  SSP$Defect_RailAge_2013_MGT[Num_row7]=0
  SSP$Start_RailAge_2014_MGT[Num_row7]=0
  SSP$Defect_RailAge_2014_MGT[Num_row7]=0
  SSP$Start_RailAge_2015_MGT[Num_row7]=0
  SSP$Defect_RailAge_2015_MGT[Num_row7]=0
  SSP$Start_RailAge_2016_MGT[Num_row7]=0
  SSP$Defect_RailAge_2016_MGT[Num_row7]=SSP$Traf_Den_2016_MGT[Num_row7]*as.numeric(SSP$Test_Date_2016[Num_row7]-SSP$Laid_Year[Num_row7])/365
  
  # reserves two decimal fractions to the Start_RailAge_2014_MGT, Start_RailAge_2015_MGT, Start_RailAge_2016_MGT,Defect_RailAge_2014_MGT, Defect_RailAge_2015_MGT, Defect_RailAge_2016_MGT
  SSP$Start_RailAge_2011_MGT<-round(SSP$Start_RailAge_2011_MGT,digits = 2)
  SSP$Start_RailAge_2012_MGT<-round(SSP$Start_RailAge_2012_MGT,digits = 2)
  SSP$Start_RailAge_2013_MGT<-round(SSP$Start_RailAge_2013_MGT,digits = 2)
  SSP$Start_RailAge_2014_MGT<-round(SSP$Start_RailAge_2014_MGT,digits = 2)
  SSP$Start_RailAge_2015_MGT<-round(SSP$Start_RailAge_2015_MGT,digits = 2)
  SSP$Start_RailAge_2016_MGT<-round(SSP$Start_RailAge_2016_MGT,digits = 2)
  SSP$Defect_RailAge_2011_MGT<-round(SSP$Defect_RailAge_2011_MGT,digits = 2)
  SSP$Defect_RailAge_2012_MGT<-round(SSP$Defect_RailAge_2012_MGT,digits = 2)
  SSP$Defect_RailAge_2013_MGT<-round(SSP$Defect_RailAge_2013_MGT,digits = 2)
  SSP$Defect_RailAge_2014_MGT<-round(SSP$Defect_RailAge_2014_MGT,digits = 2)
  SSP$Defect_RailAge_2015_MGT<-round(SSP$Defect_RailAge_2015_MGT,digits = 2)
  SSP$Defect_RailAge_2016_MGT<-round(SSP$Defect_RailAge_2016_MGT,digits = 2)
  
  ################Grinding frequency#############
  
  for (i in 1: nrow(SSP)) { # counting grinding frequency. Note: Counting all the history grinding frequency before the test date, which also means that grinding frequency won't be limited by the targeted year.
    
    
    Num_row_2011<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2009:2010)
    Num_row_2012<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2010:2011)
    Num_row_2013<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2011:2012)
    Num_row_2014<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2012:2013)
    Num_row_2015<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2013:2014)
    Num_row_2016<-which(SSP$Milepost[i]>=GRP$X.Begin.Milepost & SSP$Milepost[i]<GRP$X.End.Milepost & as.numeric(format(GRP$X.Date,"%Y")) %in% 2014:2015)
    
    GL_2011=sum(GRP$X.Low.Rail.Passes[Num_row_2011])    ### total grinding at low rail in each year
    GL_2012=sum(GRP$X.Low.Rail.Passes[Num_row_2012])  
    GL_2013=sum(GRP$X.Low.Rail.Passes[Num_row_2013])  
    GL_2014=sum(GRP$X.Low.Rail.Passes[Num_row_2014])  
    GL_2015=sum(GRP$X.Low.Rail.Passes[Num_row_2015])  
    GL_2016=sum(GRP$X.Low.Rail.Passes[Num_row_2016])  
    
    GH_2011=sum(GRP$X.High.Rail.Passes[Num_row_2011])   ###total grinding at high rail in each year
    GH_2012=sum(GRP$X.High.Rail.Passes[Num_row_2012])
    GH_2013=sum(GRP$X.High.Rail.Passes[Num_row_2013])
    GH_2014=sum(GRP$X.High.Rail.Passes[Num_row_2014])
    GH_2015=sum(GRP$X.High.Rail.Passes[Num_row_2015])
    GH_2016=sum(GRP$X.High.Rail.Passes[Num_row_2016])
    
    if (!is.na(SSP$Curve_Direction[i])){
      if ((SSP$Curve_Tangent[i]=="T" & SSP$Side[i]=="L")| SSP$Curve_Tangent[i]=="L"){
        SSP$Grinding_2011[i]=GL_2011
        SSP$Grinding_2012[i]=GL_2012
        SSP$Grinding_2013[i]=GL_2013
        SSP$Grinding_2014[i]=GL_2014
        SSP$Grinding_2015[i]=GL_2015
        SSP$Grinding_2016[i]=GL_2016
      }
      else {
        SSP$Grinding_2011[i]=GH_2011
        SSP$Grinding_2012[i]=GH_2012
        SSP$Grinding_2013[i]=GH_2013
        SSP$Grinding_2014[i]=GH_2014
        SSP$Grinding_2015[i]=GH_2015
        SSP$Grinding_2016[i]=GH_2016
      }
    }
    
    # We assume in each year, ballast cleaning will be finished at the beginning of this year. Becuase we only have the year of ballast, not specific date information.
    # counting ballast frequency. Note: Counting all the history ballast frequency before the test date, which also means that ballast frequency won't be limited by the targeted year.
    
    
    ########ballast cleaning######
    
    ### use two years as trace back period, including the research year
    #Num_row_2011<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2011 & as.numeric(BAP$year)>=2009)
    #Num_row_2012<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2012 & as.numeric(BAP$year)>=2010)
    #Num_row_2013<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2013 & as.numeric(BAP$year)>=2011)
    #Num_row_2014<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2014 & as.numeric(BAP$year)>=2012)
    #Num_row_2015<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2015 & as.numeric(BAP$year)>=2013)
    #Num_row_2016<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2016 & as.numeric(BAP$year)>=2014)
    
    
    #####  not includign the research year
    Num_row_2011<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2010 & as.numeric(BAP$year)>=2009)
    Num_row_2012<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2011 & as.numeric(BAP$year)>=2010)
    Num_row_2013<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2012 & as.numeric(BAP$year)>=2011)
    Num_row_2014<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2013 & as.numeric(BAP$year)>=2012)
    Num_row_2015<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2014 & as.numeric(BAP$year)>=2013)
    Num_row_2016<-which(SSP$Milepost[i]>=BAP$Start.Location &SSP$Milepost[i]<BAP$End.Location & as.numeric(BAP$year)<=2015 & as.numeric(BAP$year)>=2014)
    
    SSP$Ballast_2011[i]=length(Num_row_2011)   # divided by the year numbers since 2011
    SSP$Ballast_2012[i]=length(Num_row_2012)
    SSP$Ballast_2013[i]=length(Num_row_2013)
    SSP$Ballast_2014[i]=length(Num_row_2014)
    SSP$Ballast_2015[i]=length(Num_row_2015)
    SSP$Ballast_2016[i]=length(Num_row_2016)
  }
  
  
  ###########turnout database##########
  ###because the precion in turnout database is 0.1. for instance, the turnout milepost is 0.5, we will set the location milepost 0.50-0.51 has the turnout. 
  SSP$Turnout=0
  XXX=match(SSP[,"Milepost"],c(unique(TUP$Correct_MP)))
  SSP$Turnout[!is.na(XXX)]=1
  
  ##########Inspection frequency###
  
  
  In_Year=paste("Inspection_Fre_",Year_Ve,sep = "",collapse = NULL)
  Year_Num=paste("Year_",Year_Ve,sep = "",collapse = NULL)
  # SSP[,In_Year]=INP[1,Year_Num]
  
  
  ############VTI#########   count the number occurring before the test date
  
  VTI_Year=paste("VTI_",Year_Ve,sep = "",collapse = NULL)
  
  GGG=matrix(data=unlist(lapply(SSP[,"Milepost"], function(z) VTI_F(z))),ncol = 6,byrow = T)
  SSP[,VTI_Year]=GGG[,1:6]
  SSP[,VTI_Year]=unlist(SSP[,VTI_Year])
  
  
  ##############failure database##########
  
  FP<-FP[order(FP[,1],decreasing = TRUE),]
  SSP$Failure_Not_2011=0
  SSP$Failure_Not_2012=0
  SSP$Failure_Not_2013=0
  SSP$Failure_Not_2014=0
  SSP$Failure_Not_2015=0
  SSP$Failure_Not_2016=0
  
  if (nrow(FP)>=1){
    for (i in 1: nrow(FP)){
      Num_row_F=which(SSP$Milepost==FP$MILEPOST[i] & SSP$Side==FP$SIDE[i])
      CN1="Failure_Not_" %% as.character(format(FP$DATE.FOUND[i],"%Y"))
      CN2="Failure_Date_"%% as.character(format(FP$DATE.FOUND[i],"%Y"))
      CN3="Failure_Type_"%% as.character(format(FP$DATE.FOUND[i],"%Y"))
      CN4="F_Tem_"%% as.character(format(FP$DATE.FOUND[i],"%Y"))
      CN5="F_NearJoint_" %% as.character(format(FP$DATE.FOUND[i],"%Y"))
      CN6="F_NearWeld_" %% as.character(format(FP$DATE.FOUND[i],"%Y"))
      
      SSP[Num_row_F,CN1]<-1 #fill in "1" to Defect_Not in the targeted year.
      SSP[Num_row_F,CN2]<-format(FP$DATE.FOUND[i],"%Y-%m-%d") #fill in test dates to Test_Date in the targeted year.
      SSP[Num_row_F,CN3]<-FP$DEFECT.TYPE[i]
      SSP[Num_row_F,CN4]=FP$TEMPERATURE[i]
      SSP[Num_row_F,CN5]=FP$NEAR.JOINT[i]
      SSP[Num_row_F,CN6]=FP$NEAR.WELD[i]
      
    }
  }
  
  SSP$Failure_Date_2011[SSP$Failure_Not_2011==0]=as.character("2011-12-31")
  SSP$Failure_Date_2012[SSP$Failure_Not_2012==0]=as.character("2012-12-31")
  SSP$Failure_Date_2013[SSP$Failure_Not_2013==0]=as.character("2013-12-31")
  SSP$Failure_Date_2014[SSP$Failure_Not_2014==0]=as.character("2014-12-31")
  SSP$Failure_Date_2015[SSP$Failure_Not_2015==0]=as.character("2015-12-31")
  SSP$Failure_Date_2016[SSP$Failure_Not_2016==0]=as.character("2016-12-31")
  
  SSP$Failure_Date_2011=as.Date(SSP$Failure_Date_2011,"%Y-%m-%d")
  SSP$Failure_Date_2012=as.Date(SSP$Failure_Date_2012,"%Y-%m-%d")
  SSP$Failure_Date_2013=as.Date(SSP$Failure_Date_2013,"%Y-%m-%d")
  SSP$Failure_Date_2014=as.Date(SSP$Failure_Date_2014,"%Y-%m-%d")
  SSP$Failure_Date_2015=as.Date(SSP$Failure_Date_2015,"%Y-%m-%d")
  SSP$Failure_Date_2016=as.Date(SSP$Failure_Date_2016,"%Y-%m-%d")
  
  ##### rail age (MGT) when failure test date
  
  
  
  Num_row1=which(SSP$Start_Date_2011>=SSP$Laid_Year)    #   laid year is less than the start point of 2011, which is the least time
  
  SSP$Failure_RailAge_2011_MGT[Num_row1]=SSP$Traf_Den_2011_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2011[Num_row1]-SSP$Laid_Year[Num_row1])/365
  SSP$Failure_RailAge_2012_MGT[Num_row1]=SSP$Start_RailAge_2012_MGT[Num_row1]+SSP$Traf_Den_2012_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2012[Num_row1]-SSP$Start_Date_2012[Num_row1])/365
  SSP$Failure_RailAge_2013_MGT[Num_row1]=SSP$Start_RailAge_2013_MGT[Num_row1]+SSP$Traf_Den_2013_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2013[Num_row1]-SSP$Start_Date_2013[Num_row1])/365
  SSP$Failure_RailAge_2014_MGT[Num_row1]=SSP$Start_RailAge_2014_MGT[Num_row1]+SSP$Traf_Den_2014_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2014[Num_row1]-SSP$Start_Date_2014[Num_row1])/365
  SSP$Failure_RailAge_2015_MGT[Num_row1]=SSP$Start_RailAge_2015_MGT[Num_row1]+SSP$Traf_Den_2015_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2015[Num_row1]-SSP$Start_Date_2015[Num_row1])/365
  SSP$Failure_RailAge_2016_MGT[Num_row1]=SSP$Start_RailAge_2016_MGT[Num_row1]+SSP$Traf_Den_2016_MGT[Num_row1]*as.numeric(SSP$Failure_Date_2016[Num_row1]-SSP$Start_Date_2016[Num_row1])/365
  
  Num_row2=which(SSP$Start_Date_2011<SSP$Laid_Year & SSP$Failure_Date_2011>=SSP$Laid_Year)   #  laid year is located between the start time of 2011 and test time of 2011
  
  SSP$Failure_RailAge_2011_MGT[Num_row2]=SSP$Traf_Den_2011_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2011[Num_row2]-SSP$Laid_Year[Num_row2])/365
  SSP$Failure_RailAge_2012_MGT[Num_row2]=SSP$Start_RailAge_2012_MGT[Num_row2]+SSP$Traf_Den_2012_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2012[Num_row2]-SSP$Start_Date_2012[Num_row2])/365
  SSP$Failure_RailAge_2013_MGT[Num_row2]=SSP$Start_RailAge_2013_MGT[Num_row2]+SSP$Traf_Den_2013_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2013[Num_row2]-SSP$Start_Date_2013[Num_row2])/365
  SSP$Failure_RailAge_2014_MGT[Num_row2]=SSP$Start_RailAge_2014_MGT[Num_row2]+SSP$Traf_Den_2014_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2014[Num_row2]-SSP$Start_Date_2014[Num_row2])/365
  SSP$Failure_RailAge_2015_MGT[Num_row2]=SSP$Start_RailAge_2015_MGT[Num_row2]+SSP$Traf_Den_2015_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2015[Num_row2]-SSP$Start_Date_2015[Num_row2])/365
  SSP$Failure_RailAge_2016_MGT[Num_row2]=SSP$Start_RailAge_2016_MGT[Num_row2]+SSP$Traf_Den_2016_MGT[Num_row2]*as.numeric(SSP$Failure_Date_2016[Num_row2]-SSP$Start_Date_2016[Num_row2])/365
  
  
  Num_row3=which(SSP$Start_Date_2012<=SSP$Laid_Year & SSP$Failure_Date_2012>=SSP$Laid_Year)
  
  SSP$Failure_RailAge_2011_MGT[Num_row3]=0
  SSP$Failure_RailAge_2012_MGT[Num_row3]=SSP$Traf_Den_2012_MGT[Num_row3]*as.numeric(SSP$Failure_Date_2012[Num_row3]-SSP$Laid_Year[Num_row3])/365
  SSP$Failure_RailAge_2013_MGT[Num_row3]=SSP$Start_RailAge_2013_MGT[Num_row3]+SSP$Traf_Den_2013_MGT[Num_row3]*as.numeric(SSP$Failure_Date_2013[Num_row3]-SSP$Start_Date_2013[Num_row3])/365
  SSP$Failure_RailAge_2014_MGT[Num_row3]=SSP$Start_RailAge_2014_MGT[Num_row3]+SSP$Traf_Den_2014_MGT[Num_row3]*as.numeric(SSP$Failure_Date_2014[Num_row3]-SSP$Start_Date_2014[Num_row3])/365
  SSP$Failure_RailAge_2015_MGT[Num_row3]=SSP$Start_RailAge_2015_MGT[Num_row3]+SSP$Traf_Den_2015_MGT[Num_row3]*as.numeric(SSP$Failure_Date_2015[Num_row3]-SSP$Start_Date_2015[Num_row3])/365
  SSP$Failure_RailAge_2016_MGT[Num_row3]=SSP$Start_RailAge_2016_MGT[Num_row3]+SSP$Traf_Den_2016_MGT[Num_row3]*as.numeric(SSP$Failure_Date_2016[Num_row3]-SSP$Start_Date_2016[Num_row3])/365
  
  
  Num_row4=which(SSP$Start_Date_2013<=SSP$Laid_Year & SSP$Failure_Date_2013>=SSP$Laid_Year)
  
  SSP$Failure_RailAge_2011_MGT[Num_row4]=0
  SSP$Failure_RailAge_2012_MGT[Num_row4]=0
  SSP$Failure_RailAge_2013_MGT[Num_row4]=SSP$Traf_Den_2013_MGT[Num_row4]*as.numeric(SSP$Failure_Date_2013[Num_row4]-SSP$Laid_Year[Num_row4])/365
  SSP$Failure_RailAge_2014_MGT[Num_row4]=SSP$Start_RailAge_2014_MGT[Num_row4]+SSP$Traf_Den_2014_MGT[Num_row4]*as.numeric(SSP$Failure_Date_2014[Num_row4]-SSP$Start_Date_2014[Num_row4])/365
  SSP$Failure_RailAge_2015_MGT[Num_row4]=SSP$Start_RailAge_2015_MGT[Num_row4]+SSP$Traf_Den_2015_MGT[Num_row4]*as.numeric(SSP$Failure_Date_2015[Num_row4]-SSP$Start_Date_2015[Num_row4])/365
  SSP$Failure_RailAge_2016_MGT[Num_row4]=SSP$Start_RailAge_2016_MGT[Num_row4]+SSP$Traf_Den_2016_MGT[Num_row4]*as.numeric(SSP$Failure_Date_2016[Num_row4]-SSP$Start_Date_2016[Num_row4])/365
  
  
  Num_row5=which(SSP$Start_Date_2014<=SSP$Laid_Year & SSP$Failure_Date_2014>=SSP$Laid_Year)
  
  SSP$Failure_RailAge_2011_MGT[Num_row5]=0
  SSP$Failure_RailAge_2012_MGT[Num_row5]=0
  SSP$Failure_RailAge_2013_MGT[Num_row5]=0
  SSP$Failure_RailAge_2014_MGT[Num_row5]=SSP$Traf_Den_2014_MGT[Num_row5]*as.numeric(SSP$Failure_Date_2014[Num_row5]-SSP$Laid_Year[Num_row5])/365
  SSP$Failure_RailAge_2015_MGT[Num_row5]=SSP$Start_RailAge_2015_MGT[Num_row5]+SSP$Traf_Den_2015_MGT[Num_row5]*as.numeric(SSP$Failure_Date_2015[Num_row5]-SSP$Start_Date_2015[Num_row5])/365
  SSP$Failure_RailAge_2016_MGT[Num_row5]=SSP$Start_RailAge_2016_MGT[Num_row5]+SSP$Traf_Den_2016_MGT[Num_row5]*as.numeric(SSP$Failure_Date_2016[Num_row5]-SSP$Start_Date_2016[Num_row5])/365
  
  
  Num_row6=which(SSP$Start_Date_2015<=SSP$Laid_Year & SSP$Failure_Date_2015>=SSP$Laid_Year)
  
  SSP$Failure_RailAge_2011_MGT[Num_row6]=0
  SSP$Failure_RailAge_2012_MGT[Num_row6]=0
  SSP$Failure_RailAge_2013_MGT[Num_row6]=0
  SSP$Failure_RailAge_2014_MGT[Num_row6]=0
  SSP$Failure_RailAge_2015_MGT[Num_row6]=SSP$Traf_Den_2015_MGT[Num_row6]*as.numeric(SSP$Failure_Date_2015[Num_row6]-SSP$Laid_Year[Num_row6])/365
  SSP$Failure_RailAge_2016_MGT[Num_row6]=SSP$Start_RailAge_2016_MGT[Num_row6]+SSP$Traf_Den_2016_MGT[Num_row6]*as.numeric(SSP$Failure_Date_2016[Num_row6]-SSP$Start_Date_2016[Num_row6])/365
  
  
  Num_row7=which(SSP$Start_Date_2016<=SSP$Laid_Year & SSP$Failure_Date_2016>=SSP$Laid_Year)
  
  SSP$Failure_RailAge_2011_MGT[Num_row7]=0
  SSP$Failure_RailAge_2012_MGT[Num_row7]=0
  SSP$Failure_RailAge_2013_MGT[Num_row7]=0
  SSP$Failure_RailAge_2014_MGT[Num_row7]=0
  SSP$Failure_RailAge_2015_MGT[Num_row7]=0  
  SSP$Failure_RailAge_2016_MGT[Num_row7]=SSP$Traf_Den_2016_MGT[Num_row7]*as.numeric(SSP$Failure_Date_2016[Num_row7]-SSP$Laid_Year[Num_row7])/365
  
  # reserves two decimal fractions to the Start_RailAge_2014_MGT, Start_RailAge_2015_MGT, Start_RailAge_2016_MGT,Defect_RailAge_2014_MGT, Defect_RailAge_2015_MGT, Defect_RailAge_2016_MGT
  
  SSP$Failure_RailAge_2011_MGT<-round(SSP$Failure_RailAge_2011_MGT,digits = 2)
  SSP$Failure_RailAge_2012_MGT<-round(SSP$Failure_RailAge_2012_MGT,digits = 2)
  SSP$Failure_RailAge_2013_MGT<-round(SSP$Failure_RailAge_2013_MGT,digits = 2)
  SSP$Failure_RailAge_2014_MGT<-round(SSP$Failure_RailAge_2014_MGT,digits = 2)
  SSP$Failure_RailAge_2015_MGT<-round(SSP$Failure_RailAge_2015_MGT,digits = 2)
  SSP$Failure_RailAge_2016_MGT<-round(SSP$Failure_RailAge_2016_MGT,digits = 2)
  
  DI=rbind(DI,SSP)
  rm(BAP,CCP,DFP,GGP,GRP,RLP,TTP,TTP_EX,GEP,SGP,TFP,TUP,SSP,VTIP,FP)
}
proc.time()-ptm



