library(tidyverse)
library(readxl)
library(tidyr)
library(INLA)
library(BAPC)
rm(list = ls())
load("GBD393.Rdata")
pop=read.csv("/Users/Anderson/Desktop/PhDwork/Py-Github/GBD393/WPP2019_PopulationByAgeSex_Medium.csv",header = T)
locdf=read.csv("/Users/Anderson/Desktop/PhDwork/Py-Github/GBD393/Locdf.csv",header = T) %>% select(-1) %>% as_tibble()


GBD393=GBDdf27


apc17=function(preyear=2035,locid=1){
  # locid=1
  # preyear=2035
  loc=locdf %>% filter(location_id==locid)
  print(loc$location_name)
  ########################
  ### select one contry
  dfNum = GBD393 %>% 
    filter(location_id==loc$location_id) %>% ## location id=1 is global
    filter(measure_id==6) %>% ## incidence
    filter(sex_id==3) %>% # both sex
    filter(metric_name=="Number") %>% 
    filter(age_id %in% c(1,6:8))
  
  ### cases number
  agegroup=dfNum %>% select(age_id,age_name) %>% unique()
  Num = dfNum %>% filter(metric_name=="Number") %>% ## number of case
    select(year,val,age_id) %>% 
    mutate(val=round(val,0)) %>% # change to interger
    spread(age_id,val)%>% select(-1) %>% as.data.frame()
  
  
  ### bing to five age group
  ## We have 5 age groups case
  #"0-19"  "20 to 39"  "40 to 59" "60 to 79" "80+"
  Num17 = Num 
  ## predict
  PreNum= tibble(year=rep(c(2020:preyear),each=length(colnames(Num17))),
                 ageid=rep(c(colnames(Num17)),preyear-2019),
                 val=NA) %>% 
    spread(ageid,val)%>% select(-1) %>% as.data.frame()
  
  apcNum=rbind(Num17,PreNum)
  colnames(apcNum) = c(agegroup$age_name)
  rownames(apcNum) =1990:preyear
  ## We have 5 age groups population
  #"0-19"  "20 to 39"  "40 to 59" "60 to 79" "80+"
  
  ########################
  ## Populations from WPP2019
  
  ## select projections of global population
  dfpop=pop %>% filter(Time %in% c(1990:preyear)) %>% # here with 2035
    filter(LocID==loc$LocID) %>% ## location name, should be consist with location_id
    select(Time,AgeGrp,AgeGrpStart,PopTotal) %>% 
    mutate(PopTotal=PopTotal*1000) %>% 
    arrange(Time,AgeGrpStart)
  
  popage=dfpop %>% select(AgeGrp,AgeGrpStart) %>% unique()
  ## change to long dataframe
  Pop=dfpop %>% 
    select(-AgeGrp) %>% 
    spread(AgeGrpStart,PopTotal) %>% select(-1) %>% as.data.frame()
  
  Pop17 = Pop %>% 
    select(`0`,`5`,`10`,`15`)
  colnames(Pop17) = colnames(apcNum)
  rownames(Pop17) =1990:preyear
  
  
  ########################
  ## Apc-list
  gloAPC = APCList(apcNum, Pop17, gf=5,agelab=colnames(Pop17))
  print(gloAPC)
  # perform retrospective projection for 16 years, see section 5
  np=preyear-2019
  #glores = BAPC(gloAPC, predict=list(npredict=np, retro=TRUE))
  
  # to generate figure 2 in the paper use the following command
  # plotBAPC(glores, scale=100000, type="ageSpecRate", 
  #          coladd="grey80", showdata=TRUE)
  
  ## Apc-list with World (WHO 2000-2025) Standards database 
  data(whostandard)
  wstand =c(whostandard[1:4,2])/100
  
  # perform retrospective projection for 10 years, see section 5
  glores = BAPC(gloAPC, predict=list(npredict=np, retro=TRUE),
                verbose=FALSE, stdweight=wstand)
  
  ## 95%CI
  resci = qapc(glores, percentiles=c(0.025, 0.5, 0.975))
  agespec.proj(resci)[[1]]
  
  return(resci)
}


x=apc17(preyear=2035,locid=1)






