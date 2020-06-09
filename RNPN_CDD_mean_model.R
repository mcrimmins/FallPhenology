# Calculate mean GDD threshold model
# 11/21/15 MAC
# 12/2/15 - updated with Daymet data

library(data.table)
library(moments)
library(dplyr)
library(jsonlite)
library(RCurl)
#library(DaymetR)
library(birk) # convert units


# load dataset from RNPN_download.R
load("/cloud/project/RNPN_CDD_498_redmaple_base77F_052920.Rdata")
#load("/cloud/project/RNPN_CDD_498_stripedmaple_base77F_052920.Rdata")
#load("/cloud/project/RNPN_CDD_498_sugarmaple_base77F_052920.Rdata")
#load("/cloud/project/RNPN_CDD_498_quakingaspen_base77F_052920.Rdata")

# thin on latitudes; based on rpart
#siteMean<-subset(siteMean, latitude>35 & latitude<42.4)
# thin on daylength; based on rpart
siteMean<-subset(siteMean, dayLength>10.9 & dayLength<13.3)

# analysis, add in daylength corr, spearman rank corr...uses data.table
dataT <- data.table(siteMean)
# trim years used in calculation of mean
#dataT<-dataT[!(as.numeric(as.character(dataQ$first_yes_year))==2016),]
dataT$count<-1
dataSmry <- dataT[, .(pCor = cor(prismGDD,as.numeric(as.character(day_of_year)),use = "na.or.complete",method="pearson" ), 
                      spCor = cor(prismGDD,as.numeric(as.character(day_of_year)),use = "na.or.complete",method="spearman"),
                      n=sum(count),
                      medianDOY=median(as.numeric(as.character(day_of_year))),
                      meanDOY=mean(as.numeric(as.character(day_of_year)),na.rm = TRUE),
                      sdDOY=sd(as.numeric(as.character(day_of_year)),na.rm = TRUE),
                      skDOY=skewness(as.numeric(as.character(day_of_year))),
                      meanLat=mean(latitude),
                      sdLat=sd(latitude),
                      meanLon=mean(longitude),
                      sdLon=sd(longitude),
                      meanGDD=mean(prismGDD,na.rm = TRUE),
                      sdGDD=sd(prismGDD,na.rm = TRUE),
                      meanPCPN=mean(prismPCPN,na.rm = TRUE),
                      sdPCPN=sd(prismPCPN,na.rm = TRUE)), by=common_name]


# join tables with mean GGD for model
dataJoin<-left_join(siteMean,dataSmry,by='common_name')

# get GDD based on lat/lon and dates, ADD second gdd base of 50F
dataJoin$predDOYprism<-NA

for(i in 1:nrow(dataJoin)){
  lat<-dataJoin$latitude[i]
  lon<-dataJoin$longitude[i]
  base<-77 # SET base temperature, 32 for breaking leaf buds, 
  sdate<-paste0(dataJoin$year[i],"-06-21") # SET to -01-01 for breaking leaf buds, -08-01
  edate<-paste0(dataJoin$year[i],"-12-31") # SET to -06-21 for breaking leaf buds in spring, -08-01 for flower
  # PRISM
  jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"hdd',base,'","sdate":"',sdate,'","edate":"',edate,'"}')
  out<-postForm("http://data.rcc-acis.org/GridData", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  temp<-as.data.frame(out$data)
  temp$accum<-cumsum(as.numeric(as.character(temp$V2)))
  dataJoin$predDOYprism[i]<-which.min(temp$accum <=dataJoin$meanGDD[i])+(as.numeric(format(as.Date(sdate),"%j"))+1) # add start date-1  
  
  print((i/nrow(dataJoin))*100)
}
# save.image("C:/Users/Crimmins/Google Drive/MAC/r-stats/NPN/NPN_GDD_371_model.RData") saved
save(dataJoin, file="RNPN_CDD_498_redmaple_base77_model_060820_dayLength_10_13.RData")
