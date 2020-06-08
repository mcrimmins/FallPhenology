# use rNPN to download data for fall modeling
# MAC 03/27/2020

library(rnpn)

# create variables to paste into REST request
## all locations
# start_date="2012-01-01"
# end_date="2018-08-01"
# #species_id="3"
# phenophase_id="390" #371=Breaking Leaf Buds; 498=colored leaves; open flowers=501; ripe fruit= 390, leaves=483, increasing leaves=467
# baseurl="http://www.usanpn.org/npn_portal/observations/getSummarizedData.xml?"
# #XMLq<-paste(baseurl,"start_date=",start_date,"&end_date=",end_date,"&species_id=",species_id,"&phenophase_id=",phenophase_id,"&request_src=TheresaR",sep="")
# XMLq<-paste(baseurl,"start_date=",start_date,"&end_date=",end_date,"&phenophase_id=",phenophase_id,"&additional_field=Multiple_FirstY&request_src=TheresaR",sep="")

test<-npn_allobssp(speciesid = 390, startdate='2012-01-01', enddate='2019-12-31')