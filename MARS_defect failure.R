dat$Milepost = round(dat$Milepost*10)/10
dat$Year = format(dat[,"Test_Date"],"%Y")
#summary(dat$Milepost);table(dat$Year)
names(dat1)[c(6,7,10)] = c("Prefix","Milepost","Side")
#names(dat1)
dat2 = merge(dat,dat1,by = c("Prefix","Milepost","Side","Year"))
#names(dat2)
#sum(dat2$Defect_Not)
#table(dat2$Defect_Type)
#table(dat1$DEFECT.TYPE)
dat4 = aggregate(Defect_Not ~ Prefix + Milepost + Side + Year, data = dat,sum)
dat3 = data.frame(Length_in_miles = nrow(dat)*0.01,
                  Reported_Rail_Defect = sum(dat$Defect_Not),
                  Unique_Rail_Defect = sum(1*(dat4$Defect_Not>=1)),
                  Service_Failure = nrow(dat1),
                  Matches = sum(dat2$Defect_Not),
                  TDD = table(dat1$DEFECT.TYPE)["TDD"],
                  Matched_TDD = table(dat2$Defect_Type)["TDD"])
dat3$Percent_of_all_matched = round(100*dat3$Matches/dat3$Service_Failure,digits = 2)
dat3$Percent_of_TDD_matched = round(100*dat3$Matched_TDD/dat3$TDD, digits = 2)


dat5 = data.frame(
  Service_Failure = nrow(dat1),
  Service_Failure_On_Curve = sum(1*(dat1$CURVE.DEGREE == 0)),
  Matches = sum(dat2$Defect_Not),
  Matches_On_Curves = sum(dat2[dat2$CURVE.DEGREE!=0,]$Defect_Not),
  TDD = table(dat1$DEFECT.TYPE)["TDD"],
  TDD_On_Curves = table(dat1[dat1$CURVE.DEGREE!=0,]$DEFECT.TYPE)["TDD"],
  Matched_TDD = table(dat2$Defect_Type)["TDD"],
  Matched_TDD_On_Curves = table(dat2[dat2$CURVE.DEGREE!=0,]$Defect_Type)["TDD"]
)
dat5$Percent_of_all_matched = round(100*dat5$Matches/dat5$Service_Failure,digits = 2)
dat5$Percent_of_TDD_matched = round(100*dat5$Matched_TDD/dat5$TDD, digits = 2)

table(dat2$Defect_Type,dat2$DEFECT.TYPE)


