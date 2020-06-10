# logistic/ordinal regression for CDD Phenology
# MAC 06/10/2020

library(ggplot2)
#library(tidyr)
library(broom)

# load data
load("~/RProjects/FallPhenology/RNPN_CDD_498_redmaple_base77F_060820_allObs.Rdata")

# subset data
siteMean<-subset(siteMean, prismGDD<=1000)

# set -1 to NA
siteMean$phenophase_status[siteMean$phenophase_status==-1]<-NA
# check obs freq
table(siteMean$phenophase_status)

# plot pheno status
ggplot(siteMean, aes(prismGDD, phenophase_status)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("PRISM-CDD") +
  ylab("Status")
# plot pheno status with daylength
ggplot(siteMean, aes(dayLength, phenophase_status)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("daylength") +
  ylab("Status")

pheno<-glm(phenophase_status ~ prismGDD, data=siteMean, family = "binomial")
  prismGDD <- seq(0,11000, 100)
yweight <- predict(pheno, list(wt = prismGDD),type="response")
plot(siteMean$prismGDD, siteMean$phenophase_status, pch = 16, xlab = "CDD", ylab = "Status")
lines(prismGDD, yweight)

# model diags
pheno<-glm(phenophase_status ~ dayLength, data=siteMean, family = "binomial")

summary(pheno)
tidy(pheno)
pscl::pR2(pheno)["McFadden"]

library(popbio)
test<-siteMean[,c("phenophase_status","prismGDD")]
test<-test[complete.cases(test),]
logi.hist.plot(test$prismGDD,test$phenophase_status,boxp=FALSE,type="hist",col="gray")