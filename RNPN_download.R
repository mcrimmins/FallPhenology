# NPN Fall Modeling 
# R NPN package download
# MAC 05/29/20

# rnpn package
library(jsonlite)
library(RCurl)
library(reshape2)
library(tidyr)
library(dplyr)
library(geosphere)
library(data.table)
#library(DaymetR)
library(birk) # convert units
library(rnpn)
library(geosphere)

#npn_species()

# download data
dataQ<-npn_download_status_data(
  request_source="FartFace2020",
  years=c(seq(2012,2019,1)),
  species_id=c("27"),
  phenophase_ids = "498"
)

# adjust data types
dataQ$observation_date<-as.Date(dataQ$observation_date, format = "%Y-%m-%d")
dataQ$year<-as.numeric(format(dataQ$observation_date,"%Y"))

# thin data for mean model
dataQ<-dataQ[!(dataQ$intensity_value=="-9999"),]
dataQ<-dataQ[!(dataQ$intensity_value=="Less than 5%"),]
dataQ<-dataQ[(dataQ$day_of_year>=171),]
# only valid lat/lons
dataQ<-dataQ[!(dataQ$latitude>50),]
dataQ<-dataQ[!(dataQ$latitude<24),]
dataQ<-dataQ[(dataQ$longitude<(-66)),]
dataQ<-dataQ[(dataQ$longitude>(-124)),]

# thin out multiple first y's -- only one individual_id per year for a single phenophase
dataQ<-dataQ %>% group_by(individual_id,year) %>% filter(day_of_year==min(as.numeric(as.character(day_of_year))))

# site level mean
siteMean<-summarise(group_by(dataQ, site_id, year),  mean(as.numeric(as.character(day_of_year))),
                                                  mean(as.numeric(as.character(latitude))),
                                                  mean(as.numeric(as.character(longitude))),
                                                  mean(as.numeric(as.character(elevation_in_meters))),
                                                  first(state),
                                                  first(species_id),
                                                  first(common_name),
                                                  first(phenophase_id))
                                                  
colnames(siteMean)<-c("site_id","year","day_of_year","latitude","longitude","elevation_m","state","species_id","common_name","phenophase_id")

# more data cleaning  
# only valid lat/lons
siteMean<-siteMean[!(siteMean$latitude<0),]
siteMean<-siteMean[!(siteMean$longitude>0),]
# drop AK using subset
#dataQ<-subset(dataQ, state != "AK" & state != "BC")
siteMean<-siteMean[!(siteMean$state %in% c("AK","BC","NS","ON","MB")),]
siteMean<-siteMean[!(siteMean$state==-9999),]                                                                 


# get GDD based on lat/lon and dates, ADD second gdd base of 50F
siteMean$prismGDD<-NA
siteMean$prismPCPN<-NA
siteMean$dayLength<-NA
#siteMean$daymetGDD<-NA
#dataQ$daymetPCPN<-NA

# add in necessary cols
siteMean$avgDate<-as.Date(paste0(siteMean$year,"-",siteMean$day_of_year),format="%Y-%j")

for(i in 1:nrow(siteMean)){
  lat<-siteMean$latitude[i]
  lon<-siteMean$longitude[i]
  base<-77 # SET base temperature, 25C/77F for Acer rubrum 
  sdate<-paste0(siteMean$year[i],"-06-21") # SET to -01-01 for breaking leaf buds, -08-01
  edate<-siteMean$avgDate[i]
  if (as.Date(sdate)<as.Date(edate)){
    siteMean$dayLength[i]<-daylength(lat,as.numeric(as.character(siteMean$day_of_year[i])))
    jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"hdd',base,',pcpn","sdate":"',sdate,'","edate":"',edate,'"}')
    out<-postForm("http://data.rcc-acis.org/GridData", 
                  .opts = list(postfields = jsonQuery, 
                               httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
    out<-fromJSON(out)
    temp<-as.data.frame(out$data)
    siteMean$prismGDD[i]<-sum(as.numeric(as.character(temp$V2)))
    siteMean$prismPCPN[i]<-sum(as.numeric(as.character(temp$V3)))
    } else {dataQ$prismGDD[i]<-NA
  }
  print((i/nrow(siteMean))*100)
}

# convert variables to numeric
# write out GDD vars to attr
# set attributes
attr(siteMean,"start_date" )<-sdate
#attr(dataQ,"end_date" )<-end_date
attr(siteMean,"CDD_base_F" )<-base

save(siteMean, file="RNPN_CDD_498_quakingaspen_base77F_052920.Rdata")
