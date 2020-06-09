# RNPN CDD model analysis
# 05/29/20 MAC
# use output from RNPN_CDD_mean_model.R

library(data.table)
library(ggplot2)

# load data
load("/cloud/project/RNPN_CDD_498_redmaple_base77_model_052920.RData")
#load("/cloud/project/RNPN_CDD_498_stripedmaple_base77_model_052920.RData")
#load("/cloud/project/RNPN_CDD_498_sugarmaple_base77_model_052920.RData")
#load("/cloud/project/RNPN_CDD_498_quakingaspen_base77_model_052920.RData")
#load("/cloud/project/RNPN_CDD_498_redmaple_base77_model_060820_lat35_42.RData")
load("/cloud/project/RNPN_CDD_498_redmaple_base77_model_060820_dayLength_10_13.RData")

# model diagnostics
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2,na.rm = TRUE))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error), na.rm = TRUE)
}
# create data.table, added CV metrics
dataTemp <- data.table(dataJoin)
dataTemp$count<-1
dataTemp1<-dataTemp
#dataTemp1<-dataTemp1[((dataTemp1$first_yes_year)!=2016),]
modelSmry <- dataTemp1[, .(n=sum(count),
                           rmsePRISM=rmse(as.numeric(as.character(day_of_year))-predDOYprism),
                           
                           maePRISM=mae(as.numeric(as.character(day_of_year))-predDOYprism),
                           
                           pCorPRISM=cor(as.numeric(as.character(day_of_year)),predDOYprism,use = "na.or.complete",method="pearson"),
                           
                           biasPRISM=(mean(predDOYprism, na.rm = TRUE))-mean(as.numeric(as.character(day_of_year))),
                           
                           sppGDDcvPRISM=sd(prismGDD, na.rm = TRUE)/mean(prismGDD, na.rm = TRUE),
                           sppDOYcv=sd(as.numeric(as.character(day_of_year)), na.rm = TRUE)/mean(as.numeric(as.character(day_of_year)), na.rm = TRUE)),by=common_name]

modelSmry$r2PRISM<-round((modelSmry$pCorPRISM)^2, digits=3)

# facet plot of obs/pred
ggplot(dataJoin,aes(x=as.numeric(as.character(day_of_year)), y = prismGDD)) + 
  facet_wrap(~common_name) +
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="red", high="blue")

# predicted vs observed
ggplot(dataJoin,aes(x=predDOYprism, y =as.numeric(as.character(day_of_year)))) + 
  facet_wrap(~common_name) +
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="grey", high="blue", name="Latitude")

# try rpart, http://www.statmethods.net/advstats/cart.html
library(rpart)
fit <- rpart(prismGDD~dayLength,
             method="anova", data=dataJoin)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
# create additional plots 
#par(mfrow=c(1,2)) # two plots on one page 
#rsq.rpart(fit) # visualize cross-validation results  	
#dev.off()
# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)




