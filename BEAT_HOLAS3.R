## HELCOM Biodiversity Assessment Tool BEAT ##
# updated version for HOLAS 3

rm(list=ls())

# load needed packages
library(sqldf)
library(dplyr)

# Specify working directory, below as example:
#setwd("D:/R_folder/BEAT")

source('include.r')

# ===================================================================================================

# Specify here the names of files containing SAU/Ecosystem structure and the indicator data

  file_sau <- "./input/SAU_eutro.txt"                    # Spatial assessment unit (SAU) structure
  file_ec <- "./input/EcosystemComponents_HOLAS3.txt"     # Ecosystem component (EC) structure
  file_desc <- "./input/descriptors.txt"           # Descriptors
  file_crit <- "./input/criteria.txt"              # Criteria
  file_indcat <- "./input/IndicatorCatalogue_HOLAS3.txt"  # Indicator Catalogue - distinct indicators
  file_ind <- "./input/HOLAS3_indicator_results_with_bycatch.txt"             # Indicator data - boundary and obs. values associated with an SAU / EC
  file_ooao <- "./input/ooao.txt"                  # (optional) Indicators using one-out all-out (OOAO)
  
  #file_conf <- "./confidence_penalties.R"          # Code for calculating Confidence penalties
                                                   # if the variable is not defined, this will not be done
  
  # ===================================================================================================
  # Specify output files and options
  
  ndigits <- 4 #number of digits to show results with

  out_ind <- "./results/results_indicators.txt"      # EQR results for individual indicators
  out_EC<- "./results/results_EC.txt"            # results by Ecosystem component
  out_SAU <-  "./results/results_SAU.txt"        #results by Spatial Assessment unit
  out_conf_EC<- "./results/results_conf_EC.txt"            # confidence results by Ecosystem component
  out_conf_SAU <-  "./results/results_conf_SAU.txt"        # confidence results by Spatial Assessment unit
  
  # In these two files, for each unit/component there are 2 EQR results specified:
  # (1) EQR_self - EQR calculated ONLY for indicators associated directly with the SAU/EC.
  # (2) EQR      - EQR calculated for all indicators associated with the SAU/EC and with any child units/components.
  
  out_SAU_EC <- "./results/results_SAUEC.txt"
  out_conf_SAU_EC <- "./results/results_conf_SAUEC.txt"
  # a table with all combinations of SAU/EC and their EQR results.
  # In this table, the EQR for an SAU/EC combination includes only indicators
  # which belong to this SAU and this EC.

  out_SAU_EC_aggr <- "./results/results_SAUEC_aggr.txt"
  out_conf_SAU_EC_aggr <- "./results/results_conf_SAUEC_aggr.txt"
  out_count <-  "./results/results_counts.txt"        # Indicator counts
  # Results for SAU/EC combinations which include all child indicators. 
  # For a particular combination of SAU/EC, include all indicators which 
  # belong to any combination of SAUs or ECs which are identical with or
  # children of the SAu and EC in question
  
  OutputECDetailLevel<-5
  #OutputSAUDetailLevel<-
  # Optional variables OutputECDetailLevel,OutputSAUDetailLevel are defined at the head of this file.
  # These specify (optionally) the greatest detail level for SAUs and ECs in the ResultSAUECaggr table.
  
  
# ===================================================================================================
#Calculate individual indicator EQR values 

#Call function to calculate EQR values by comparing Observed indicator value with Status boundaries:
# Required arguments:
# datain - the name of the dataframe containing the indicator data
# Obs - the name of the variable (column) in this data which contains the observed values
# Bad - the name of the variable with value corresponding to EQR = 0.0
# ModGood - the name of the variable with value corresponding to EQR = 0.6 (the GES boundary)
# High - the name of the variable with value corresponding to EQR = 1.0 

# Optional arguments:
# BadPoor - the name of the variable with values corresponding to EQR = 0.2
# PoorMod - the name of the variable with values corresponding to EQR = 0.4
# GoodHigh - the name of the variable with values corresponding to EQR = 0.8
# If these are not specified, we interpolate between 0.0 and 0.6 or 0.6 and 1.0
#
# IndType - presently only 2 types implemented:
#       1 - boundary values increase/decrease monotonically
#       2 - optimum range indicator
# If Type 2 is specified for any of the indicators, then additional variables are required:
# ModGood2 - the name of the variable with 2nd value corresponding to EQR = 0.6
# Bad2 - the name of the variable with 2nd value corresponding to EQR = 0.0
# BadPoor2, PoorMod2 and GoodHigh2 are optional variables for Type2 indicators

#Example:
#ind<-EQR(indData=ind,Obs="ObsValue",Bad="B",BadPoor="BP",PoorMod="PM",ModGood="MG",GoodHigh="GH",High="H",ModGood2="MG2",Bad2="B2")

# ===================================================================================================
# Read input indicator data and structures

sau<-read.table(file_sau, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="") 
EC<-read.table(file_ec, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="") 
desc<-read.table(file_desc, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="") 
crit<-read.table(file_crit, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="") 
indcat<-read.table(file_indcat, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="") 
ind<-read.table(file_ind, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="'") 

# Calculate EQR value for each individual indicator
ind<-EQR(indData=ind,Obs="Obs",Bad="Bad",ModGood="ModGood",High="High",ModGood2="ModGood2",Bad2="Bad2")

# Calculate confidence for each individual indicator
ind<-Confidence(datain=ind,varObs="Obs",varGES="ModGood",varStdErr="StdErr",
                varConfTemp="ConfT",varConfSpat="ConfS",varConfAcc="ConfA",varConfMeth="ConfM")


if (file.exists(file_ooao)){
  bDropOOAO<-FALSE
  ooao<-read.table(file_ooao, header=TRUE,sep=";", stringsAsFactors=FALSE,quote="'") #Indicator data - boundary and obs. values associated with an SAU / EC
  ind[,'OOAO']<-sqldf("SELECT t2.GroupID FROM ind t1 LEFT JOIN ooao t2 ON t1.IndicatorID=t2.IndicatorID")
  ooao<-subset(ind, !is.na(OOAO))
  ooao<-sqldf("SELECT OOAO, SAUID, INDICATORID, EQR FROM ooao GROUP BY SAUID, OOAO HAVING MIN(EQR) ")
  ind[,'OOAO2']<-sqldf("SELECT t2.OOAO FROM ind t1 LEFT JOIN ooao t2 ON t1.SAUID=t2.SAUID and t1.IndicatorID=t2.IndicatorID")
  ind$OOAO3<-ifelse(is.na(ind$OOAO),1,ifelse(is.na(ind$OOAO2),NA,1))
  ind$OOAO<-ind$OOAO3
  ind$OOAO2<-NULL
  ind$OOAO3<-NULL
  }else{
  bDropOOAO<-TRUE
  ind$OOAO<-1
} 
ind$OK2<-ind$OK*ind$OOAO

#Save copy of EC including OOAO for later - then remove OOAO from EC:
#EC_OOAO<-EC
#EC$OOAO<-NULL

sau[,'count_ind'] <- sqldf("SELECT COUNT(t2.OK2) 
                           FROM sau t1 LEFT JOIN ind t2 ON t1.SAUID=t2.SAUID
                           GROUP BY t1.SAUID")
sau[,'count_conf'] <- sqldf("SELECT COUNT(t2.OK) 
                           FROM sau t1 LEFT JOIN ind t2 ON t1.SAUID=t2.SAUID
                           GROUP BY t1.SAUID")
ResultSAU<-sau
ConfSAU<-sau

ind[,'ECID'] <- sqldf("SELECT t2.EcosystemComponentID FROM ind t1 LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID")
ind[,'ParentID'] <- sqldf("SELECT t2.ParentID FROM ind t1 LEFT JOIN EC t2 ON t1.ECID=t2.ECID")
ind[,'EC'] <- sqldf("SELECT t2.EcosystemComponent FROM ind t1 LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID")


# Calculate SAU weights --------------------------------------------------------------
# Weighting for each SAU based on area (km2), allocating weight only to SAUs which have indicators

sau<-distributeweights(data=sau,varLevel='Level',varID='SAUID',varParentID='ParentID',varWeight='Area_km2',varActive='count_ind',levelPrefix="SAU_")
#sum(sau$Weight) #Check sum of weights

# Calculate EC weights --------------------------------------------------------------

# Get ecosysytem component levels
ECall<-EC
ECall[,'count_ind']<-sqldf("SELECT COUNT(t2.OK2) FROM ECall t1 
                           LEFT JOIN ind t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID")
ECall[,'count_conf']<-sqldf("SELECT COUNT(t2.OK) FROM ECall t1 
                           LEFT JOIN ind t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID")
ResultEC<-ECall
ConfEC<-ECall

ECall<-distributeweights(data=ECall,varLevel='EcosystemLevel',varID='ECID',varParentID='ParentID',varActive='count_ind',levelPrefix="EC_")

# Calculate weighting in each Ecosystem Component sub-tree under each SAU

bFirst<-TRUE
for (isau in 1:nrow(sau)){
  sauid<-sau[isau,'SAUID']
  sauwt<-sau[isau,'Weight']
  if(sauwt>0){
    indtemp<-ind[ind$SAUID==sauid,]
    ECtemp<-EC
    ECtemp[,'count_ind']<-sqldf("SELECT COUNT(t2.OK2) 
                                FROM ECtemp t1 LEFT JOIN indtemp t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID")
    ECtemp[,'count_conf']<-sqldf("SELECT COUNT(t2.OK) 
                                FROM ECtemp t1 LEFT JOIN indtemp t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID")
    ECtemp<-distributeweights(data=ECtemp,varLevel='EcosystemLevel',varID='ECID',varParentID='ParentID',varActive='count_ind',levelPrefix="EC_")
    ECtemp<-distributeweights(data=ECtemp,varLevel='EcosystemLevel',varID='ECID',varParentID='ParentID',varActive='count_conf',levelPrefix="EC_",WeightName="WeightConf")
    ECtemp$Weight<-sauwt*ECtemp$Weight
    ECtemp$WeightConf<-sauwt*ECtemp$WeightConf
    ECtemp$SAUID<-sauid
    if (bFirst){
      SAU_EC<-ECtemp
      bFirst<-FALSE
    }else{
      SAU_EC <- sqldf("SELECT * FROM SAU_EC UNION ALL SELECT * FROM ECtemp")
    }
  }
}

  rm(list=c('indtemp','ECtemp','isau','sauwt','sauid'))

# Assign Indicator weights --------------------------------------------------------------
# Assign weights to each indicator according to the Ecosystem Component and SAU they are linked to

ind[,'Weight']<-sqldf("SELECT (t2.Weight/t2.count_ind) AS w FROM ind t1 LEFT JOIN SAU_EC t2 ON t1.SAUID=t2.SAUID AND t1.ECID=t2.ECID")
ind[,'WeightConf']<-sqldf("SELECT (t2.WeightConf/t2.count_conf) AS w FROM ind t1 LEFT JOIN SAU_EC t2 ON t1.SAUID=t2.SAUID AND t1.ECID=t2.ECID")
ind$Weight<-ifelse(is.na(ind$OK2),0,ind$Weight)

#Check sum of indicator weights (should be equal to 1)
cat(paste0("Check: Sum of indicator weights: = ",sum(ind$Weight),"\n","      Sum of confidence weights: = ",sum(ind$WeightConf),"\n"))

## Adjust weighting for phytoplankton 
# Note that these weights will only work if OOAO with zooplankton is used
ind[ind$IndicatorID==153,30] <- 1.2*ind[ind$IndicatorID==153,30] # Higher weight to Seasonal succession of phytoplankton functional groups indicator
ind[ind$IndicatorID==327,30] <- 0.8*ind[ind$IndicatorID==327,30] # Lower weight to cyanobacteria bloom index indicator


# Get results by SAU and by Ecosystem Component
n0<-min(sau[,'Level'])
n1<-max(sau[,'Level'])

#Check output detail level  for SAUs 
if(exists("OutputSAUDetailLevel")){
  if(OutputSAUDetailLevel>(n1-1)) {OutputSAUDetailLevel<-(n1-1)}
  if(OutputSAUDetailLevel<n0) {OutputSAUDetailLevel<-n0}
}else{
  OutputSAUDetailLevel<-n1-1
}

for(i in n0:n1){
  indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.EQR, t1.Weight FROM ind t1 
                        LEFT JOIN sau t2 ON t1.SAUID=t2.SAUID"))
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
  }
}
ResultSAU[,'EQR_self']<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight)/SUM(t2.Weight),",ndigits,") FROM sau t1
                                     LEFT JOIN ind t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))
ResultSAU[,'EQR']<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight)/SUM(t2.Weight),",ndigits,") FROM sau t1
                                LEFT JOIN indsum t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))


ndesc<-nrow(crit)
for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    for(i in n0:n1){
      indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.IndicatorID, t1.EQR, t1.Weight FROM ind t1 
                            LEFT JOIN sau t2 ON t1.SAUID=t2.SAUID WHERE t1.OK2=1"))
      #Find out if Descriptor/Criteria match
      indtemp[,'OK']<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indtemp t1 
                                   LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
      indtemp$OK<-ifelse(is.na(indtemp$OK),0,indtemp$OK)
      if(i==n0){
        indsum<-indtemp
      }else{
        indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
      }
    }
    ResultSAU[,crit[ic,'DC']]<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight*t2.OK)/SUM(t2.Weight*t2.OK),",ndigits,") FROM sau t1
                                            LEFT JOIN indsum t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))
    
  }
}

# Confidence results

  # Check if code for applying confidence penalties has been specified
  if(exists("file_conf")){
    if(file.exists(file_conf)){
      # file has been specified and exists - run the code
      source(file_conf)
    }
  }

for(i in n0:n1){
  indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.Conf, t1.WeightConf FROM ind t1 
                        LEFT JOIN sau t2 ON t1.SAUID=t2.SAUID"))
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
  }
}
ConfSAU[,'Conf_self']<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf)/SUM(t2.WeightConf),",ndigits,") FROM sau t1
                                    LEFT JOIN ind t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))
ConfSAU[,'Conf']<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf)/SUM(t2.WeightConf),",ndigits,") FROM sau t1
                               LEFT JOIN indsum t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))

for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    for(i in n0:n1){
      indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.IndicatorID, t1.Conf, t1.WeightConf FROM ind t1 
                            LEFT JOIN sau t2 ON t1.SAUID=t2.SAUID WHERE t1.OK=1"))
      #Find out if Descriptor/Criteria match
      indtemp[,'OK']<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indtemp t1 
                                   LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
      indtemp$OK<-ifelse(is.na(indtemp$OK),0,indtemp$OK)
      if(i==n0){
        indsum<-indtemp
      }else{
        indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
      }
    }
    ConfSAU[,crit[ic,'DC']]<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf*t2.OK)/SUM(t2.WeightConf*t2.OK),",ndigits,") FROM sau t1
                                            LEFT JOIN indsum t2 ON t1.SAUID=t2.SAUID GROUP BY t1.SAUID"))

  }
}
indsum_test<-indsum

rm(list=c('bFirst','n0','n1','i','indtemp','indsum','ic'))

###################################### ECOSYSTEM COMPONENTS ######################################
n0<-min(EC[,'EcosystemLevel'])
n1<-max(EC[,'EcosystemLevel'])

#Check output detail level  for SAUs 
if(exists("OutputECDetailLevel")){
  if(OutputECDetailLevel>(n1-1)) {OutputECDetailLevel<-(n1-1)}
  if(OutputECDetailLevel<n0) {OutputECDetailLevel<-n0}
}else{
  OutputECDetailLevel<-n1-1
}

for(i in n0:n1){
  indtemp<-sqldf(paste0("SELECT t2.EC_L",i," AS ECID, t1.EQR, t1.Weight FROM ind t1 
                        LEFT JOIN ECall t2 ON t1.ECID=t2.ECID"))
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
  }
}

ResultEC[,'EQR_self']<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight)/SUM(t2.Weight),",ndigits,") FROM ResultEC t1
                                    LEFT JOIN ind t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))
ResultEC[,'EQR']<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight)/SUM(t2.Weight),",ndigits,") FROM ResultEC t1
                               LEFT JOIN indsum t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))

for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    for(i in n0:n1){
      indtemp<-sqldf(paste0("SELECT t2.EC_L",i," AS ECID, t1.IndicatorID, t1.EQR, t1.Weight FROM ind t1 
                            LEFT JOIN ECall t2 ON t1.ECID=t2.ECID WHERE t1.OK2=1"))
      #Find out if Descriptor/Criteria match
      indtemp[,'OK']<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indtemp t1 
                                   LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
      indtemp$OK<-ifelse(is.na(indtemp$OK),0,indtemp$OK)
      if(i==n0){
        indsum<-indtemp
      }else{
        indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
      }
    }
    ResultEC[,crit[ic,'DC']]<-sqldf(paste0("SELECT ROUND(SUM(t2.EQR*t2.Weight*t2.OK)/SUM(t2.Weight*t2.OK),",ndigits,") FROM ResultEC t1
                                           LEFT JOIN indsum t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))
  }
}

rm(list=c('indtemp','indsum'))

# EC Confidence results

for(i in n0:n1){
  indtemp<-sqldf(paste0("SELECT t2.EC_L",i," AS ECID, t1.Conf, t1.WeightConf FROM ind t1 
                        LEFT JOIN ECall t2 ON t1.ECID=t2.ECID"))
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
  }
}

ConfEC[,'Conf_self']<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf)/SUM(t2.WeightConf),",ndigits,") FROM ConfEC t1
                                    LEFT JOIN ind t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))
ConfEC[,'Conf']<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf)/SUM(t2.WeightConf),",ndigits,") FROM ConfEC t1
                               LEFT JOIN indsum t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))

for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    for(i in n0:n1){
      indtemp<-sqldf(paste0("SELECT t2.EC_L",i," AS ECID, t1.IndicatorID, t1.Conf, t1.WeightConf FROM ind t1 
                            LEFT JOIN ECall t2 ON t1.ECID=t2.ECID WHERE t1.OK=1"))
      #Find out if Descriptor/Criteria match
      indtemp[,'OK']<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indtemp t1 
                                   LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
      indtemp$OK<-ifelse(is.na(indtemp$OK),0,indtemp$OK)
      if(i==n0){
        indsum<-indtemp
      }else{
        indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp")
      }
    }
    ConfEC[,crit[ic,'DC']]<-sqldf(paste0("SELECT ROUND(SUM(t2.Conf*t2.WeightConf*t2.OK)/SUM(t2.WeightConf*t2.OK),",ndigits,") FROM ResultEC t1
                                           LEFT JOIN indsum t2 ON t1.ECID=t2.ECID GROUP BY t1.ECID"))
  }
}

#rm(list=c('n0','n1','i','indtemp','indsum','ic'))



# ===================================================================================================
# Additional results EQR


n0<-min(EC[,'EcosystemLevel'])
n1<-max(EC[,'EcosystemLevel'])
for(i in n0:n1){
  
  indtemp<-sqldf(paste0("SELECT t2.SAUID, t3.EC_L",i," AS ECID, t1.IndicatorID, t1.EQR, t1.Weight FROM ind t1,
                        sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID"))
  
  if(i==n0){
    indsum1<-indtemp
  }else{
    indsum1 <- sqldf("SELECT * FROM indsum1 UNION ALL SELECT * FROM indtemp WHERE ECID <> ''")
  }
}

n0<-min(sau[,'Level'])
n1<-max(sau[,'Level'])

for(i in n0:n1){
  
  indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.ECID, t1.IndicatorID, t1.EQR, t1.Weight FROM indsum1 t1,
                        sau t2 WHERE t1.SAUID=t2.SAUID"))
  
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp WHERE SAUID <> ''")
  }
}


for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    indsum[,crit[ic,'DC']]<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indsum t1 
                                         LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
  }
  }


indsum[,'EC_level']<-sqldf(paste0("SELECT t1.EcosystemLevel FROM ECall t1, indsum t2 WHERE t1.ECID=t2.ECID"))



ResultSAUECaggr<-sqldf(paste0("SELECT t1.SAUID, t2.Level AS SAUlevel,t2.SAU, t1.ECID, t3.EcosystemLevel AS EClevel, t3.ParentID, t3.OOAO, t3.EcosystemComponent, 
                              ROUND(SUM(t1.EQR*t1.Weight)/SUM(t1.Weight),",ndigits,") AS EQR 
                              FROM indsum t1, sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID and t3.EcosystemLevel<=",
                              OutputECDetailLevel," AND t2.Level<=",OutputSAUDetailLevel,
                              " GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))




for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    restemp<-sqldf(paste0("SELECT t1.SAUID, t1.ECID, 
                          ROUND(SUM(t1.EQR*t1.Weight)/SUM(t1.Weight),",ndigits,") AS EQR 
                          FROM indsum t1 WHERE t1.",crit$DC[ic],"=1 
                          GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))
    
    ResultSAUECaggr[,crit[ic,'DC']]<-sqldf(paste0("SELECT t2.EQR FROM ResultSAUECaggr t1 
                                                  LEFT JOIN restemp t2 ON t1.SAUID=t2.SAUID AND t1.ECID=t2.ECID"))
  }
  }

CountsSAUECaggr<-sqldf(paste0("SELECT t1.SAUID, t2.Level AS SAUlevel,t2.SAU, t1.ECID, t3.EcosystemLevel AS EClevel,t3.EcosystemComponent, 
                              COUNT(t1.EQR) AS Total 
                              FROM indsum t1, sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID and t3.EcosystemLevel<=",
                              OutputECDetailLevel," AND t2.Level<=",OutputSAUDetailLevel,
                              " GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))
for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    counttemp<-sqldf(paste0("SELECT t1.SAUID, t1.ECID, 
                          COUNT(t1.EQR) AS Count 
                          FROM indsum t1 WHERE t1.",crit$DC[ic],"=1 
                          GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))
    
    CountsSAUECaggr[,crit[ic,'DC']]<-sqldf(paste0("SELECT t2.Count FROM CountsSAUECaggr t1 
                                                  LEFT JOIN counttemp t2 ON t1.SAUID=t2.SAUID AND t1.ECID=t2.ECID"))
  }
}


ResultSAUEC<-sqldf(paste0("SELECT t1.SAUID, t2.Level AS SAUlevel,t2.SAU, t1.ECID, t3.EcosystemLevel AS EClevel, t3.ParentID, t3.EcosystemComponent, t3.OOAO,
                          COUNT(t1.EQR) AS Ind_count,
                          ROUND(SUM(t1.EQR*t1.Weight)/SUM(t1.Weight),",ndigits,") AS EQR, 
                          ROUND(SUM(t1.Weight),",ndigits,") AS Weight 
                          FROM ind t1, sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID 
                          GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))

rm(list=c('indtemp','indsum','indsum1','restemp'))

# ===================================================================================================
# Additional results Confidence


n0<-min(EC[,'EcosystemLevel'])
n1<-max(EC[,'EcosystemLevel'])

for(i in n0:n1){
  
  indtemp<-sqldf(paste0("SELECT t2.SAUID, t3.EC_L",i," AS ECID, t1.IndicatorID, t1.Conf, t1.WeightConf FROM ind t1,
                        sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID"))
  
  if(i==n0){
    indsum1<-indtemp
  }else{
    indsum1 <- sqldf("SELECT * FROM indsum1 UNION ALL SELECT * FROM indtemp WHERE ECID <> ''")
  }
}

n0<-min(sau[,'Level'])
n1<-max(sau[,'Level'])

for(i in n0:n1){
  
  indtemp<-sqldf(paste0("SELECT t2.SAU_L",i," AS SAUID, t1.ECID, t1.IndicatorID, t1.Conf, t1.WeightConf FROM indsum1 t1,
                        sau t2 WHERE t1.SAUID=t2.SAUID"))
  
  if(i==n0){
    indsum<-indtemp
  }else{
    indsum <- sqldf("SELECT * FROM indsum UNION ALL SELECT * FROM indtemp WHERE SAUID <> ''")
  }
}


for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    indsum[,crit[ic,'DC']]<-sqldf(paste0("SELECT t2.",crit$DC[ic]," FROM indsum t1 
                                         LEFT JOIN indcat t2 ON t1.IndicatorID=t2.IndID"))
  }
  }


indsum[,'EC_level']<-sqldf(paste0("SELECT t1.EcosystemLevel FROM ECall t1, indsum t2 WHERE t1.ECID=t2.ECID"))



ConfSAUECaggr<-sqldf(paste0("SELECT t1.SAUID, t2.Level AS SAUlevel,t2.SAU, t1.ECID, t3.EcosystemLevel AS EClevel,t3.EcosystemComponent, 
                              ROUND(SUM(t1.Conf*t1.WeightConf)/SUM(t1.WeightConf),",ndigits,") AS Conf 
                              FROM indsum t1, sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID and t3.EcosystemLevel<=",
                              OutputECDetailLevel," AND t2.Level<=",OutputSAUDetailLevel,
                              " GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))

for(ic in 1:ndesc){
  if(crit$DC[ic] %in% names(indcat)){
    restemp<-sqldf(paste0("SELECT t1.SAUID, t1.ECID, 
                          ROUND(SUM(t1.Conf*t1.WeightConf)/SUM(t1.WeightConf),",ndigits,") AS Conf 
                          FROM indsum t1 WHERE t1.",crit$DC[ic],"=1 
                          GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))
    
    ConfSAUECaggr[,crit[ic,'DC']]<-sqldf(paste0("SELECT t2.Conf FROM ResultSAUECaggr t1 
                                                  LEFT JOIN restemp t2 ON t1.SAUID=t2.SAUID AND t1.ECID=t2.ECID"))
  }
  }

ConfSAUEC<-sqldf(paste0("SELECT t1.SAUID, t2.Level AS SAUlevel,t2.SAU, t1.ECID, t3.EcosystemLevel AS EClevel,t3.EcosystemComponent,
                          COUNT(t1.Conf) AS Ind_count,
                          ROUND(SUM(t1.Conf*t1.WeightConf)/SUM(t1.Weight),",ndigits,") AS Conf, 
                          ROUND(SUM(t1.WeightConf),",ndigits,") AS Weight 
                          FROM ind t1, sau t2, ECall t3 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t3.ECID 
                          GROUP BY t1.SAUID, t1.ECID ORDER BY t1.SAUID, t1.ECID"))

#rm(list=c('n0','n1','i','ic','indtemp','indsum','indsum1','restemp','ECall'))
rm(list=c('n0','n1','i','ic','indtemp','indsum','indsum1','restemp'))


# ===================================================================================================
# Apply one-out all-out on Ecosystem Components
# If OOAO=1 for an ecosystem component, then it's EQR is changed to the lowest EQR value of its children.
# 
EC_OOAO <- ResultSAUECaggr[ResultSAUECaggr$OOAO == 1,1:9]			# pick ecosystem components that should use OOAO
EC_OOAO_lvl5 <- EC_OOAO[EC_OOAO$EClevel == 5,]
EC_OOAO_lvl4 <- EC_OOAO[EC_OOAO$EClevel == 4,]

EC_OOAO1 <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t2.ECID, t2.EcosystemLevel AS EClevel, t2.ParentID, t2.OOAO, t2.EcosystemComponent, t1.EQR 
		FROM EC_OOAO_lvl5 t1, EC t2 WHERE t1.ParentID=t2.ECID GROUP BY t1.SAUID, t2.ECID HAVING MIN(EQR)")
EC_OOAO_lvl4 <- rbind(EC_OOAO_lvl4, EC_OOAO1)
EC_OOAO2 <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t2.ECID, t2.EcosystemLevel AS EClevel, t2.ParentID, t2.OOAO, t2.EcosystemComponent, t1.EQR 
		FROM EC_OOAO_lvl4 t1, EC t2 WHERE t1.ParentID=t2.ECID GROUP BY t1.SAUID, t2.ECID HAVING MIN(EQR)")
EC_OOAO3 <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t2.ECID, t2.EcosystemLevel AS EClevel, t2.ParentID, t2.OOAO, t2.EcosystemComponent, t1.EQR 
		FROM EC_OOAO2 t1, EC t2 WHERE t1.ParentID=t2.ECID GROUP BY t1.SAUID, t2.ECID HAVING MIN(EQR)")
EC_OOAO_fish <- rbind(EC_OOAO1, EC_OOAO2) # for fish

# ===================================================================================================
# Results for Ecosystem components

# Fish results using OOAO between species 
FishResults <- EC_OOAO_fish[EC_OOAO_fish$SAUlevel == 3 & EC_OOAO_fish$ParentID == 4 | EC_OOAO_fish$SAUlevel == 3 & EC_OOAO_fish$ParentID == 14,]
FishResults <- FishResults[order(FishResults$SAUID, -FishResults$ECID),]

Status=0
for (i in 1:nrow(FishResults)) {
  if (FishResults[i,9] >= 0.6) {
    Status[i] <- "GES"
  }
  else if (FishResults[i,9] < 0.6) {
    Status[i] <- "sub-GES"
  }
}
FishResults <- cbind(FishResults, Status)

FishConf <- ConfSAUECaggr[ConfSAUECaggr$ECID == 14 | ConfSAUECaggr$ECID == 64 | ConfSAUECaggr$ECID == 65 | ConfSAUECaggr$ECID == 67 |
                            ConfSAUECaggr$ECID == 75 | ConfSAUECaggr$ECID == 113 | ConfSAUECaggr$ECID == 117 | 
                            ConfSAUECaggr$ECID == 125 | ConfSAUECaggr$ECID == 129,]
FishConf <- FishConf[FishConf$SAUlevel == 3,]
FishConf <- FishConf[order(FishConf$SAUID, -FishConf$ECID),]

ConfClass=0
for (i in 1:nrow(FishConf)) {
  if (FishConf[i,7] > 0.75) {
    ConfClass[i] <- "High"
  }
  else if (FishConf[i,7] >= 0.5) {
    ConfClass[i] <- "Intermediate"
  }
  else if (FishConf[i,7] < 0.5) {
    ConfClass[i] <- "Low"
  }
}

FishConf <- cbind(FishConf[,1:7], ConfClass)
FishResults <- cbind(FishResults[,c(1:5,8:10)],FishConf[,7:8])

# Pelagic habitats results using OOAO between phyto- and zooplankton
PelagicResults <- EC_OOAO3[EC_OOAO3$SAUlevel == 3 & EC_OOAO3$ECID == 5,]
PelagicResults <- PelagicResults[order(PelagicResults$SAUID, -PelagicResults$ECID),]

Status=0
for (i in 1:nrow(PelagicResults)) {
  if (PelagicResults[i,9] >= 0.6) {
    Status[i] <- "GES"
  }
  else if (PelagicResults[i,9] < 0.6) {
    Status[i] <- "sub-GES"
  }
}
PelagicResults <- cbind(PelagicResults, Status)

PelagicConf <- ConfSAUECaggr[ConfSAUECaggr$SAUlevel == 3 & ConfSAUECaggr$ECID == 18 | ConfSAUECaggr$SAUlevel == 3 & ConfSAUECaggr$ECID == 17,]
PelagicConf <- PelagicConf[order(PelagicConf$SAUID, -PelagicConf$ECID),]

ConfClass=0
for (i in 1:nrow(PelagicConf)) {
  if (PelagicConf[i,7] > 0.75) {
    ConfClass[i] <- "High"
  }
  else if (PelagicConf[i,7] >= 0.5) {
    ConfClass[i] <- "Intermediate"
  }
  else if (PelagicConf[i,7] < 0.5) {
    ConfClass[i] <- "Low"
  }
}

PelagicConf <- cbind(PelagicConf[,1:7], ConfClass)
PelagicResults <- cbind(PelagicResults[,c(1:5,8:10)],PelagicConf[,7:8])

# Mammals results using OOAO on criteria
Mammals <- ResultSAUECaggr[ResultSAUECaggr$SAUlevel == 2 & ResultSAUECaggr$ECID == 60 |
                             ResultSAUECaggr$SAUlevel == 2 & ResultSAUECaggr$ECID == 61 |
                             ResultSAUECaggr$SAUlevel == 2 & ResultSAUECaggr$ECID == 62 |
                             ResultSAUECaggr$SAUlevel == 2 & ResultSAUECaggr$ECID == 63, 1:13]

for (i in 1:nrow(Mammals)){
  Mammals[i,9] <- min(Mammals[i,10:13], na.rm = TRUE)
}

Mammals_OOAO <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t2.ECID, t2.EcosystemLevel AS EClevel, t2.ParentID, t2.OOAO, t2.EcosystemComponent, t1.EQR 
		FROM Mammals t1, EC t2 WHERE t1.ParentID=t2.ECID GROUP BY t1.SAUID, t2.ECID HAVING MIN(EQR)")
MammalsResults <- rbind(Mammals_OOAO, Mammals[,1:9])
MammalsResults <- MammalsResults[order(MammalsResults$ECID, MammalsResults$SAUID),]

Status=0
for (i in 1:nrow(MammalsResults)) {
  if (MammalsResults[i,9] >= 0.6) {
    Status[i] <- "GES"
  }
  else if (MammalsResults[i,9] < 0.6) {
    Status[i] <- "sub-GES"
  }
}
MammalsResults <- cbind(MammalsResults, Status)

MammalsConf <- ConfSAUECaggr[ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 12 |
                               ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 13 |
                               ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 60 | 
                               ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 61 | 
                               ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 62 | 
                               ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 63,1:7]
MammalsConf <- MammalsConf[order(MammalsConf$ECID, MammalsConf$SAUID),]

ConfClass=0
for (i in 1:nrow(MammalsConf)) {
  if (MammalsConf[i,7] > 0.75) {
    ConfClass[i] <- "High"
  }
  else if (MammalsConf[i,7] >= 0.5) {
    ConfClass[i] <- "Intermediate"
  }
  else if (MammalsConf[i,7] < 0.5) {
    ConfClass[i] <- "Low"
  }
}

MammalsConf <- cbind(MammalsConf, ConfClass)
MammalsResults <- cbind(MammalsResults[,c(1:5,8:10)],MammalsConf[,7:8])

# Waterbird results using 75% of species in species groups achieving threshold 
Bird_ind <- ind[ind$ParentID == 7 | ind$ParentID == 8 | ind$ParentID == 9 | ind$ParentID == 10 | ind$ParentID == 11,]
Bird_ind <- filter(Bird_ind[Bird_ind$Weight > 0,])
Bird_SAU <- split(Bird_ind, Bird_ind$SAUID)

for (i in 1:length(Bird_SAU)) {
  Group_ind <- split(Bird_SAU[[i]], Bird_SAU[[i]]['ParentID'])
  for (j in 1:length(Group_ind)) {
    result_row <- Group_ind[[j]][1,c(1,28),]
    prop_group <- nrow(subset(Group_ind[[j]], EQR >= 0.6))/nrow(Group_ind[[j]])
    if (prop_group > 0.75) {
      eqr_group <- 0.8
      eqr_status <- "GES"
    } else {
      eqr_group <- 0.3
      eqr_status <- "sub-GES"}
    result_row[,'EQR'] <- eqr_group
    result_row[,'prop'] <- prop_group
    result_row[,'Status'] <- eqr_status
    if (j == 1) {
      BirdGroups <- result_row
    } else {
      BirdGroups <- rbind(BirdGroups,result_row)}
  }
  if (i == 1) {
    BirdGroupsSAU <- BirdGroups
  } else {
    BirdGroupsSAU <- rbind(BirdGroupsSAU, BirdGroups)}
}

BirdGroupResults <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t1.ECID, t1.EClevel, t1.ParentID, t1.OOAO, t1.EcosystemComponent, t2.EQR, t2.prop, t2.Status
		FROM ResultSAUECaggr t1, BirdGroupsSAU t2 WHERE t1.SAUID=t2.SAUID AND t1.ECID=t2.ParentID")
BirdGroupResults <- BirdGroupResults[order(BirdGroupResults$prop),]
BirdResult <- sqldf("SELECT t1.SAUID, t1.SAUlevel, t1.SAU, t2.ECID, t2.EcosystemLevel AS EClevel, t2.ParentID, t2.OOAO, t2.EcosystemComponent, t1.EQR, t1.prop, t1.Status 
		FROM BirdGroupResults t1, EC t2 WHERE t1.ParentID=t2.ECID GROUP BY t1.SAUID, t2.ECID HAVING MIN(EQR)")
BirdResults <- rbind(BirdGroupResults, BirdResult)
BirdResults <- BirdResults[order(BirdResults$EClevel, BirdResults$SAUID, BirdResults$ECID),]

BirdConf <- ConfSAUECaggr[ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 2 | ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 7 |
                            ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 8 | ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 9 |
                            ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 10 | ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 11,] # for sub-basin level
#BirdConf <- ConfSAUECaggr[ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 2 | ConfSAUECaggr$SAUlevel == 2 & ConfSAUECaggr$ECID == 7 |
#                           ConfSAUECaggr$SAUlevel == 1 & ConfSAUECaggr$ECID == 8 | ConfSAUECaggr$SAUlevel == 1 & ConfSAUECaggr$ECID == 9 |
#                           ConfSAUECaggr$SAUlevel == 1 & ConfSAUECaggr$ECID == 10 | ConfSAUECaggr$SAUlevel == 1 & ConfSAUECaggr$ECID == 11,] # for Baltic level
BirdConf <- BirdConf[order(BirdConf$EClevel, BirdConf$SAUID, BirdConf$ECID),]

ConfClass=0
for (i in 1:nrow(BirdConf)) {
  if (BirdConf[i,7] > 0.75) {
    ConfClass[i] <- "High"
  }
  else if (BirdConf[i,7] >= 0.5) {
    ConfClass[i] <- "Intermediate"
  }
  else if (BirdConf[i,7] < 0.5) {
    ConfClass[i] <- "Low"
  }
}

BirdConf <- cbind(BirdConf[,1:7], ConfClass)
BirdResults <- cbind(BirdResults[,c(1:5,8:11)],BirdConf[,7:8])

### Save result files for Ecosystem Components #change file name as needed
write.table(MammalsResults, "./results/results_Mammals.txt", sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(BirdResults, "./results/results_Birds.txt", sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(FishResults, "./results/results_CoastalFish.txt", sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(PelagicResults, "./results/results_Pelagic.txt", sep="\t", na="",row.names=FALSE,quote=FALSE)

# ===================================================================================================
# Save the integration files generated:

if(bDropOOAO){ind$OOAO<-NULL}
ind$OK2<-NULL

ResultSAUEC$Conf<-ConfSAUEC$Conf

write.table(ind, out_ind, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ResultEC, out_EC, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ResultSAU, out_SAU, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ResultSAUEC, out_SAU_EC, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ResultSAUECaggr, out_SAU_EC_aggr, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ConfEC, out_conf_EC, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ConfSAU, out_conf_SAU, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ConfSAUEC, out_conf_SAU_EC, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(ConfSAUECaggr, out_conf_SAU_EC_aggr, sep="\t", na="",row.names=FALSE,quote=FALSE)
write.table(CountsSAUECaggr, out_count, sep="\t", na="",row.names=FALSE,quote=FALSE)
