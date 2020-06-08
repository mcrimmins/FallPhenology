# NPN XML download, ACIS GDD and precip download
# MAC 11/25/15
# added DAYMET download 11/30/15

library(XML)
library(RCurl)
library(jsonlite)
#library(ggmap)
library(reshape2)
library(tidyr)
library(dplyr)
library(geosphere)
library(data.table)
#library(DaymetR)
library(birk) # convert units

# create variables to paste into REST request
## all locations
start_date="2012-01-01"
end_date="2019-08-01"
species_id="3" #  
phenophase_id="498" #371=Breaking Leaf Buds; 498=colored leaves; open flowers=501; ripe fruit= 390, leaves=483, increasing leaves=467
baseurl="http://www.usanpn.org/npn_portal/observations/getSummarizedData.xml?"
XMLq<-paste(baseurl,"start_date=",start_date,"&end_date=",end_date,"&species_id=",species_id,"&phenophase_id=",phenophase_id,"&request_src=TheresaR",sep="")
#XMLq<-paste(baseurl,"start_date=",start_date,"&end_date=",end_date,"&phenophase_id=",phenophase_id,"&additional_field=Multiple_FirstY&request_src=TheresaR",sep="")


#XMLq="http://www.usanpn.org/npn_portal/observations/getSummarizedData.xml?start_date=2012-01-01&end_date=2012-06-01&request_src=rest_test"

mydat <- getURL(XMLq)
dataQ<-xmlParse(mydat)

# parse out data into lists and dataFrame
dataQ<-xmlParse(XMLq)
xmlData <- xmlToList(dataQ)
dataQ<-as.data.frame(t(data.frame(as.list(xmlData))))

# thin out multiple first y's -- only one individual_id per year for a single phenophase
dataQ<-dataQ %>% group_by(individual_id,first_yes_year) %>% filter(first_yes_doy==min(as.numeric(as.character(first_yes_doy))))

#test<-data.frame(lapply(xmlData, type.convert), stringsAsFactors=FALSE)
dataQ$latitude<-as.numeric(as.character(dataQ$latitude)) # check for valid nums...
dataQ$longitude<-as.numeric(as.character(dataQ$longitude))
# only valid lat/lons
dataQ<-dataQ[!(dataQ$latitude<0),]
dataQ<-dataQ[!(dataQ$longitude>0),]
# thin out prior no's = -9999
dataQ<-dataQ[!(dataQ$numdays_since_prior_no==-9999),]
# thin out prior no's on value
dataQ<-dataQ[!(as.numeric(as.character(dataQ$numdays_since_prior_no))>15),] # change to smaller? 15?

# drop AK using subset
#dataQ<-subset(dataQ, state != "AK" & state != "BC")
dataQ<-dataQ[!(dataQ$state %in% c("AK","BC","NS","ON","MB")),]
dataQ<-dataQ[!(dataQ$state==-9999),]

# thin out by number of records by spp, n=30? PUT AT END after dropping AK/CANADIAN spp
sppN<-as.data.frame(table(dataQ$common_name))
sppN<-sppN[!(sppN$Freq<30),]
dataQ <- dataQ[(dataQ$common_name %in% sppN$Var1),]

# develop dates for pheno calendars
# dataQ$firstDate<-as.Date(strptime(paste(as.numeric(as.character(dataQ$first_yes_year)),
#                                         as.numeric(as.character(dataQ$first_yes_month)),
#                                         as.numeric(as.character(dataQ$first_yes_day))),format="%Y %m %d"))
# dataQ$lastDate<-as.Date(strptime(paste(as.numeric(as.character(dataQ$last_yes_year)),
#                                        as.numeric(as.character(dataQ$last_yes_month)),
#                                        as.numeric(as.character(dataQ$last_yes_day))),format="%Y %m %d"))

# site level mean
# test<-summarise(group_by(dataQ, site_id, species_id, first_yes_year), mean(as.numeric(as.character(first_yes_doy))))
# add in all other vars to reconstruct the table...

# forcings
# chilling degree days 
# photoperiod


# get GDD based on lat/lon and dates, ADD second gdd base of 50F
dataQ$prismGDD<-NA
dataQ$prismPCPN<-NA
dataQ$dayLength<-NA
dataQ$daymetGDD<-NA
dataQ$daymetPCPN<-NA

for(i in 1:nrow(dataQ)){
  lat<-dataQ$latitude[i]
  lon<-dataQ$longitude[i]
  base<-32 # SET base temperature, 32 for breaking leaf buds, 
  sdate<-paste0(dataQ$first_yes_year[i],"-01-01") # SET to -01-01 for breaking leaf buds, -08-01
  edate<-dataQ$firstDate[i]
  if (as.Date(sdate)<as.Date(edate)){
    dataQ$dayLength[i]<-daylength(lat,as.numeric(as.character(dataQ$first_yes_doy[i])))
    
    jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"gdd',base,',pcpn","sdate":"',sdate,'","edate":"',edate,'"}')
    out<-postForm("http://data.rcc-acis.org/GridData", 
                  .opts = list(postfields = jsonQuery, 
                               httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
    out<-fromJSON(out)
    temp<-as.data.frame(out$data)
    dataQ$prismGDD[i]<-sum(as.numeric(as.character(temp$V2)))
    dataQ$prismPCPN[i]<-sum(as.numeric(as.character(temp$V3)))
  } else {dataQ$prismGDD[i]<-NA
  }
  # add in DAYMET...CHANGE to year-1
  if(as.numeric(as.character(dataQ$first_yes_year[i]))<2016){
    try(download.daymet("temp",lat=lat,lon=lon,start_yr=as.numeric(as.character(dataQ$first_yes_year[i])),end_yr=as.numeric(as.character(dataQ$first_yes_year[i])), internal = TRUE, quiet = TRUE))
    temp<-temp$data
    temp$GDD<-((conv_unit(temp$tmax..deg.c.,"C","F")+conv_unit(temp$tmin..deg.c.,"C","F"))/2)-base
    temp$GDD[temp$GDD < 0] <- 0
    dataQ$daymetGDD[i]<-sum(temp$GDD[1:as.numeric(as.character(dataQ$first_yes_doy[i]))])
    dataQ$daymetPCPN[i]<-conv_unit(sum(temp$prcp..mm.day.[1:as.numeric(as.character(dataQ$first_yes_doy[i]))]),"mm","inch")
  } else {
    dataQ$daymetGDD[i]<-NA
    dataQ$daymetPCPN[i]<-NA
  }
  print((i/nrow(dataQ))*100)
}

# convert variables to numeric
# write out GDD vars to attr
# set attributes
attr(dataQ,"start_date" )<-start_date
attr(dataQ,"end_date" )<-end_date
attr(dataQ,"GDD_base_F" )<-base

save(dataQ, file="NPN_GDD_390_daymet_base32F_082916.Rdata")
