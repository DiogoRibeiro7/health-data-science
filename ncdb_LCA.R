#setwd("C:/Users/brs380/OneDrive - Harvard University/NCDB/Data")
setwd("/Users/brianajoy/OneDrive\ -\ Harvard\ University/NCDB/Data")

library(sas7bdat)
early.puf<-read.sas7bdat("puf_early.sas7bdat")

  ## LCA FACTOR LEVELS: race, urban dwelling, hispanic, insurance, age, sES ##
#Race - 3 levels
early.puf$race3<- factor(early.puf$race3,levels=c(1:3),labels=c("White","Black","Other"))

#Urban Dwelling - 3 groups
early.puf$urbandwell <- factor(early.puf$urbandwell,levels=c(1:3),labels=c("Metro","Urban","Rural"))

#Hispanic Status - 3 groups
early.puf$hispanic3 <- factor(early.puf$hispanic3,levels=c(1:3),labels=c("Non-Hispanic","Hispanic","Unknown"))

#Insurance Type - 6 groups
early.puf$insurancetype<-factor(early.puf$insurancetype, levels=c(1:6), labels=c("Not Insured","Private","Medicaid","Medicare","Other Govt","Unknown"))

#Age - binary 
early.puf$age_bin <- factor(early.puf$age_bin,levels=c(1:4), labels=c("18-49","50-64","65-74","75+"))

#CREATE SES VARIABLE
early.puf$SES <- factor(early.puf$SES,levels=c(1:3),labels=c("Low SES","Med SES","High SES"))


## Set up for LCA ##

library(reshape2)
library(plyr)
library(dplyr)
library(poLCA)
library(ggplot2)
library(ggparallel)
library(igraph)
library(tidyr)
library(knitr)
early.puf$facility<-factor(early.puf$FACILITY_TYPE_CD,levels=c(1:4,9),labels=c("Community Cancer","Comprehensive Cancer","Academic/Research","Integrated Network","Other"))


# select variables
lca.earlydata <- early.puf %>% dplyr::select(PUF_CASE_ID,optcare, SEX, facility, DX_RX_STARTED_DAYS, CROWFLY,CDCC_TOTAL_BEST, race3,hispanic3,urbandwell,age_bin,SES,insurancetype)

# define function
eff<-with(lca.earlydata, cbind(race3,hispanic3,urbandwell,age_bin,SES,insurancetype)~1) #

#------ run a sequence of models with 1-8 classes and print out the model with the lowest BIC
# min_bic <- 1000000
# for(i in 2:8){
#   lc <- poLCA(eff, lca.earlydata, nclass=i, maxiter=3000, 
#               tol=1e-5, na.rm=TRUE,  
#               nrep=15, verbose=TRUE, calc.se=TRUE)
#   if(lc$bic < min_bic){
#     min_bic <- lc$bic
#     LCA_best_model<-lc
#   }
# }    	
lc7 <- poLCA(eff,early.puf,nclass=7, maxiter=5000,
             tol=1e-5, na.rm=TRUE,nrep=20,verbose=TRUE, calc.se=TRUE)

p.lc7<-lc7$probs
lc7.race<-p.lc7$race3
lc7.hispanic<-p.lc7$hispanic3
lc7.urban<-p.lc7$urbandwell
lc7.age <-p.lc7$age4
lc7.ses <-p.lc7$SES
lc7.insure<-p.lc7$insurance

## Tables with LCA results ##
class7 <- lc7$predclass
prob7 <- lc7$posterior

#pull membership probability of assigned cluster
  idx <- c(1:length(class7))
  pull <- cbind(idx,class7)
  memclass.prob <- prob7[pull]
lcaprob <- as.data.frame(cbind(class7,memclass.prob))

#create boxplot by latent class assignment 
mem.lcaplot <-ggplot(lcaprob, aes(x=factor(class7), y=memclass.prob)) + geom_boxplot() 
print(mem.lcaplot + ggtitle("LCA Membership Probability"))
aggregate(lcaprob$memclass.prob, list(lc=class7), fivenum)

complete.earlypuflca <- na.omit(lca.earlydata[,c(1,8:13)])

lcacomplete <- cbind(complete.earlypuflca,class7)
lca.pufdata <- merge(early.puf,lcacomplete, by="PUF_CASE_ID")
#Observed Freq 
  #race
tab.race <- table(lcacomplete$class7,lcacomplete$race3)
prop.table(tab.race,1)
  #urban dwelling
prop.table(table(complete.earlypuflca$urbandwell,class7),2)
  #hispanic3
prop.table(table(complete.earlypuflca$hispanic3,class7),2)
  #insurance
prop.table(table(complete.earlypuflca$insurance,class7),2)
  #income
prop.table(table(complete.earlypuflca$SES,class7),2)
  #age
prop.table(table(complete.earlypuflca$age4,class7),2)

#optcare
prop.table(table(lca.pufdata$optcare,class7),2)

  ## non LCA variables
#gender
prop.table(table(lca.pufdata$Gender,class7),2)

#optimal care
prop.table(table(lca.pufdata$optcare,class7),2)
#minimal treatment
prop.table(table(lca.pufdata$mintreat,class7),2)

#charlson-deyo
lca.pufdata$CDCC_TOTAL_BEST<-factor(lca.pufdata$CDCC_TOTAL_BEST,levels=c(0:3),labels=c("0","1","2","3+"))

prop.table(table(lca.pufdata$CDCC_TOTAL_BEST,class7),2)

#facility type
prop.table(table(lca.pufdata$facility,class7),2)

#stage group
lca.pufdata$ANALYTIC_STAGE_GROUP <- factor(lca.pufdata$ANALYTIC_STAGE_GROUP, levels=c(1:2), labels=c("Stage I","Stage II"))
prop.table(table(lca.pufdata$ANALYTIC_STAGE_GROUP,class7),2)
#vital status
prop.table(table(lca.pufdata$PUF_VITAL_STATUS,class7),2)

#region of facility location
lca.pufdata$FACILITY_LOCATION_CD<-factor(lca.pufdata$FACILITY_LOCATION_CD,levels=c(1:9),labels=c("New England","MidAtlantic","S Atlantic","East North Central","ES Central","WN Central","WS Central","Mountain","Pacific"))
lca.pufdata$FACILITY_LOCATION_CD <- as.factor(lca.pufdata$FACILITY_LOCATION_CD)
prop.table(table(lca.pufdata$FACILITY_LOCATION_CD,lca.pufdata$class7),2)
chisq.test(table(lca.pufdata$FACILITY_LOCATION_CD,lca.pufdata$class7))

#Year of Diagnosis
prop.table(table(lca.pufdata$YEAR_OF_DIAGNOSIS,lca.pufdata$class7),2)

## CROWFLY DISTANCE BY LATENT CLASS ##
crow.lc <-ggplot(lca.pufdata, aes(x=factor(class7), y=CROWFLY)) + geom_boxplot() 
print(crow.lc + ggtitle("Crowfly Distance by Latent Class"))

crow.aov <- aov(CROWFLY~class7,data=lca.pufdata)
summary(crow.aov)


crow.mean<-aggregate(lca.pufdata$CROWFLY, list(lc=class7), na.rm=TRUE, mean)
crow.sd<-aggregate(lca.pufdata$CROWFLY, list(lc=class7), na.rm=TRUE, sd)

save(lc7,lca.pufdata,file="lca_earlypuf.RData")

