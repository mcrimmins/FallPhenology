# PRISM model analysis
# 05/15/20 MAC

library(data.table)
library(ggplot2)


# load dataset from NPN_XML_GDD.R
#load("/cloud/project/NPN_CDD_498_AcerRubrum_base77_model_051520.RData")
#load("/cloud/project/NPN_CDD_498_QuakingAspen_base77_model_052220.RData")
load("/cloud/project/NPN_CDD_498_TulipTree_base77_model_052220.RData")

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
                           rmsePRISM=rmse(as.numeric(as.character(first_yes_doy))-predDOYprism),
                          
                           maePRISM=mae(as.numeric(as.character(first_yes_doy))-predDOYprism),
        
                           pCorPRISM=cor(as.numeric(as.character(first_yes_doy)),predDOYprism,use = "na.or.complete",method="pearson"),
                           
                           biasPRISM=(mean(predDOYprism, na.rm = TRUE))-mean(as.numeric(as.character(first_yes_doy))),
                           
                           sppGDDcvPRISM=sd(prismGDD, na.rm = TRUE)/mean(prismGDD, na.rm = TRUE),
                           sppDOYcv=sd(as.numeric(as.character(first_yes_doy)), na.rm = TRUE)/mean(as.numeric(as.character(first_yes_doy)), na.rm = TRUE)),by=common_name]

modelSmry$r2PRISM<-round((modelSmry$pCorPRISM)^2, digits=3)

# Year specific model summary...
dataTemp<-dataTemp[((dataTemp$first_yes_year)==2016),]
modelSmrySub <- dataTemp[, .(n=sum(count),
                             rmsePRISM=rmse(as.numeric(as.character(first_yes_doy))-predDOYprism),
                             maePRISM=mae(as.numeric(as.character(first_yes_doy))-predDOYprism),
                             pCorPRISM=cor(as.numeric(as.character(first_yes_doy)),predDOYprism,use = "na.or.complete",method="pearson"),
                             biasPRISM=(mean(predDOYprism, na.rm = TRUE))-mean(as.numeric(as.character(first_yes_doy))),
                             sppGDDcvPRISM=sd(prismGDD, na.rm = TRUE)/mean(prismGDD, na.rm = TRUE),
                             sppDOYcv=sd(as.numeric(as.character(first_yes_doy)), na.rm = TRUE)/mean(as.numeric(as.character(first_yes_doy)), na.rm = TRUE)),by=common_name]

modelSmrySub$r2PRISM<-round((modelSmry$pCorPRISM)^2, digits=3)

## SUMMARY FIGS for PAPER
# plot CV metrics
diags<-melt(modelSmry, id=c("common_name"), measure.vars = c("sppGDDcvPRISM","sppDOYcv"))
p <- ggplot(diags, aes(common_name, value))
p + geom_point(aes(color=variable), size=4)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# scatterplot CV metrics
ggplot(modelSmry, aes(sppGDDcvPRISM, sppDOYcv, size=rmsePRISM))+
  geom_point()

# boxplot of spp DOY
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}
p <- ggplot(dataJoin, aes(common_name, (as.numeric(as.character(first_yes_doy)))))
p + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.data = give.n, geom = "text")

## END PAPER  FIGS


# plot model diagnostics
diags<-melt(modelSmry, id=c("common_name"), measure.vars = c("rmsePRISM"))
p <- ggplot(diags, aes(common_name, value))
p + geom_point(aes(color=variable), size=4)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  ylim(-0.2,1)


# facet plot of obs/pred
ggplot(dataJoin,aes(x=as.numeric(as.character(first_yes_doy)), y = prismGDD)) + 
  facet_wrap(~common_name) +
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="red", high="blue")



# boxplot of spp GDD values
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}
p <- ggplot(dataJoin, aes(common_name, prismGDD))
p + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.data = give.n, geom = "text")

# boxplot of spp GDD errors
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}
p <- ggplot(dataJoin, aes(common_name, (as.numeric(as.character(first_yes_doy))-predDOYprism)))
p + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.data = give.n, geom = "text")



# predicted vs observed
ggplot(dataJoin,aes(x=predDOYprism, y =as.numeric(as.character(first_yes_doy)))) + 
  facet_wrap(~common_name) +
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="grey", high="blue", name="Latitude")

# PRISM vs DAYMET plot
ggplot(dataJoin,aes(x=daymetGDD, y =prismGDD)) + 
  facet_wrap(~common_name) +
  geom_point(aes(color = as.numeric(as.character(dataJoin$first_yes_year))))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  theme(legend.position="bottom")


## SPP level analysis
# select variables v1, v2, v3
dataJoin$elevation_in_meters<-as.numeric(as.character(dataJoin$elevation_in_meters))
dataJoin$first_yes_year<-as.numeric(as.character(dataJoin$first_yes_year))
dataJoin$first_yes_doy<-as.numeric(as.character(dataJoin$first_yes_doy))
vars <- c("latitude", "longitude", "elevation_in_meters","common_name","first_yes_year","first_yes_doy","prismGDD","dayLength","predDOYprism","prismPCPN")
subDataQ <- dataJoin[vars]
subDataQ<-subset(subDataQ,common_name=="tuliptree")

# subset by year...
subDataQ<-subDataQ[((subDataQ$first_yes_year)==2016),]

# add in outlier control...

# var scatterplot...DOY vs GDD
ggplot(subDataQ,aes(x=first_yes_doy, y = prismGDD)) + 
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="red", high="blue")

# var scatterplot...observed vs predicted
ggplot(subDataQ,aes(x=first_yes_doy, y = predDOYprism)) + 
  geom_point(aes(color = latitude))+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_gradient(low="red", high="blue")


# tree model
library(tree)
pairs(subDataQ,panel=panel.smooth)
model<-tree(prismGDD~.-common_name,data=subDataQ)
plot(model)
text(model)
print(model)
summary(model)

# try rpart, http://www.statmethods.net/advstats/cart.html
library(rpart)
fit <- rpart(prismGDD~first_yes_doy+latitude,
             method="anova", data=subDataQ)

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

# prune the tree 
#pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pfit<- prune(fit, cp= 0.07)
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# try Random Forests
# library(randomForest)
# fit <- randomForest(first_yes_doy~latitude+dayLength+prismGDD,data=subDataQ)
# print(fit) # view results 
# importance(fit) # importance of each predictor

# map residuals of each model
library(ggmap)
#qmplot(longitude,latitude, data=dataQ, color=as.numeric(as.character(last_yes_doy)),source="google")
#bb<-c(-125, 25,-66, 50)
map <- get_map(c(left = -125, bottom = 25, right = -66, top = 50))
ggmap(map, crop=FALSE)+
  geom_jitter(data=subDataQ, aes(longitude,latitude,color=(as.numeric(as.character(subDataQ$first_yes_doy))-subDataQ$predDOYprism)),pch=16, size=3,position = position_jitter(width = .5))+
  scale_color_gradient2(mid="white",high="blue",low="red", midpoint = 0, guide_colorbar(title="days (error)"), limits=c(-100,100))+
  ylim(25,50)+
  xlim(-125,-66) +
  theme ( 
    legend.position = c(0.01, 0.01), # put the legend INSIDE the plot area
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = F, fill = "white"),
    legend.key = element_rect (fill = F, colour = F),
    panel.grid.major = element_blank (), # remove major grid
    panel.grid.minor = element_blank (),  # remove minor grid
    axis.text = element_blank (), 
    axis.title = element_blank (),
    axis.ticks = element_blank ()) 

ggsave("map.png", dpi=600)

# write data function...
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(modelSmry)

