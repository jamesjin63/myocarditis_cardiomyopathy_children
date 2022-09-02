library(tidyverse)
library(readxl)
library(stringr)
rm(list = ls())
filter=dplyr::filter
select=dplyr::select
load("Country204name.RData")
source("Tab1.R")


deci=function(a=124376885){
  b=as.character(a)
  lenb=str_length(b)
  if(lenb<3){
    return(b)
  } else if (lenb<7) {
    b1=substr(b,lenb-2,lenb)
    b2=substr(b,1,lenb-3)
    bb=paste0(b2," ",b1)
    return(bb)
  } else{
    b1=substr(b,lenb-2,lenb)
    b2=substr(b,lenb-5,lenb-3)
    b3=substr(b,1,lenb-6)
    bb=paste0(b3," ",b2," ",b1)
    return(bb)
  }
}



a=table1(index="Incidence",dname=499)

df1=a %>% filter(location_id %in% c(name_5,name_21)) %>% 
  mutate(
    count1990=paste0(deci(round(`Num-1990`,0))," (",deci(round(Num_low1990,0))," to ",deci(round(Num_up1990,0)),")"),
    ASR1990=paste0(round(`ASR-1990`,1)," (",round(ASR_low1990,1)," to ",round(ASR_up1990,1),")"),
    count2019=paste0(deci(round(`Num-2019`,0))," (",deci(round(Num_low2019,0))," to ",deci(round(Num_up2019,0)),")"),
    ASR2019=paste0(round(`ASR-2019`,1)," (",round(ASR_low2019,1)," to ",round(ASR_up2019,1),")"),
    PCs=paste0(round(Percentage,1)," (",round(Percentage_low,1)," to ",round(Percentage_up,1),")"),
    EAPCs=paste0(round(EAPC,1)," (",round(EAPC_low,1)," to ",round(EAPC_up,1),")")
  ) %>% 
  select(location_name,location_id,count1990,ASR1990,count2019,ASR2019,PCs,EAPCs)

a=table1(index="Prevalence",dname=499)
df2=a %>% filter(location_id %in% c(name_5,name_21)) %>% 
  mutate(
    count1990=paste0(deci(round(`Num-1990`,0))," (",deci(round(Num_low1990,0))," to ",deci(round(Num_up1990,0)),")"),
    ASR1990=paste0(round(`ASR-1990`,1)," (",round(ASR_low1990,1)," to ",round(ASR_up1990,1),")"),
    count2019=paste0(deci(round(`Num-2019`,0))," (",deci(round(Num_low2019,0))," to ",deci(round(Num_up2019,0)),")"),
    ASR2019=paste0(round(`ASR-2019`,1)," (",round(ASR_low2019,1)," to ",round(ASR_up2019,1),")"),
    PCs=paste0(round(Percentage,1)," (",round(Percentage_low,1)," to ",round(Percentage_up,1),")"),
    EAPCs=paste0(round(EAPC,1)," (",round(EAPC_low,1)," to ",round(EAPC_up,1),")")
  ) %>% 
  select(location_name,location_id,count1990,ASR1990,count2019,ASR2019,PCs,EAPCs)

a=table1(index="Deaths",dname=499)
df3=a %>% filter(location_id %in% c(name_5,name_21)) %>% 
  mutate(
    count1990=paste0(deci(round(`Num-1990`,0))," (",deci(round(Num_low1990,0))," to ",deci(round(Num_up1990,0)),")"),
    ASR1990=paste0(round(`ASR-1990`,1)," (",round(ASR_low1990,1)," to ",round(ASR_up1990,1),")"),
    count2019=paste0(deci(round(`Num-2019`,0))," (",deci(round(Num_low2019,0))," to ",deci(round(Num_up2019,0)),")"),
    ASR2019=paste0(round(`ASR-2019`,1)," (",round(ASR_low2019,1)," to ",round(ASR_up2019,1),")"),
    PCs=paste0(round(Percentage,1)," (",round(Percentage_low,1)," to ",round(Percentage_up,1),")"),
    EAPCs=paste0(round(EAPC,1)," (",round(EAPC_low,1)," to ",round(EAPC_up,1),")")
  ) %>% 
  select(location_name,location_id,count1990,ASR1990,count2019,ASR2019,PCs,EAPCs)


a=table1(index="DALYs (Disability-Adjusted Life Years)",dname=499)
df4=a %>% filter(location_id %in% c(name_5,name_21)) %>% 
  mutate(
    count1990=paste0(deci(round(`Num-1990`,0))," (",deci(round(Num_low1990,0))," to ",deci(round(Num_up1990,0)),")"),
    ASR1990=paste0(round(`ASR-1990`,1)," (",round(ASR_low1990,1)," to ",round(ASR_up1990,1),")"),
    count2019=paste0(deci(round(`Num-2019`,0))," (",deci(round(Num_low2019,0))," to ",deci(round(Num_up2019,0)),")"),
    ASR2019=paste0(round(`ASR-2019`,1)," (",round(ASR_low2019,1)," to ",round(ASR_up2019,1),")"),
    PCs=paste0(round(Percentage,1)," (",round(Percentage_low,1)," to ",round(Percentage_up,1),")"),
    EAPCs=paste0(round(EAPC,1)," (",round(EAPC_low,1)," to ",round(EAPC_up,1),")")
  ) %>% 
  select(location_name,location_id,count1990,ASR1990,count2019,ASR2019,PCs,EAPCs)



