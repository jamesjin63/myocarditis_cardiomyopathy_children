library(tidyverse)
library(tibble)
library(tidyr)
rm(list = ls())
eapc=function(x){
  return(100*(exp(x)-1))
}
filter=dplyr::filter
select=dplyr::select
# data read and process
load("Country204name.RData")
load("~/Desktop/GBD/myocarditis/GBD499_27loc.Rdata")
load("~/Desktop/GBD/myocarditis/GBD499_27locPC.Rdata")

## all data 1990 to 2019
GBD1990and2019=GBDdf27 %>% as_tibble()
GBD1990to2019=GBDdf27PC%>% as_tibble()


table1=function(index="Incidence",dname=499,gender="Both"){
  index=index
  index1=index
  ################################################
  # DALY Number
  df_region=GBD1990and2019 %>% filter(location_id %in% name_all) %>% 
    filter(measure_name==index) %>% 
    filter(age_name=="<20 years") %>%  
    filter(cause_id==dname) %>% 
    filter(sex_name==gender) %>% 
    filter(year %in% c(1990,2019)) %>% 
    filter(metric_name=="Number") %>% 
    group_by(year) %>% 
    nest()
  
  df1count=df_region  %>%  filter(year==2019) %>% purrr::pluck("data", 1) %>% select(location_name,location_id,val,lower,upper) %>% 
    mutate(val=round(val,2),lower=round(lower,2),upper=round(upper,2)) %>% 
    set_names("location_name","location_id",paste0("2017_",index1,"_",c("Num","Num_low","Num_up")))
  
  df2count=df_region  %>%  filter(year==1990) %>% purrr::pluck("data", 1)%>% select(location_name,location_id,val,lower,upper) %>% 
    mutate(val=round(val,2),lower=round(lower,2),upper=round(upper,2)) %>% 
    set_names("location_name","location_id",paste0("1990_",index1,"_",c("Num","Num_low","Num_up")))
  
  df_relative=df_region  %>% filter(year==2019) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val) %>% rename("number_2017"=3) %>% 
    left_join(df_region  %>% filter(year==1990) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val) %>% rename("number_1990"=3)) %>% 
    mutate(relative_number=round(number_2017-number_1990,2),
           relative_change=round(100*relative_number/number_1990,2))
  
  df1=df2count %>% left_join(df1count) %>% left_join(df_relative %>% select(location_name,relative_number,relative_change))
  colnames(df1)=c("location_name","location_id","Num-1990","Num_low1990","Num_up1990",
                  "Num-2019","Num_low2019","Num_up2019","Num-relative",
                  "relative_change")
  
  ################################################
  # DALY ASR
  df_region= GBD1990and2019 %>% filter(location_id %in% name_all) %>% 
    filter(measure_name==index) %>% 
    filter(age_name=="<20 years") %>%  
    filter(cause_id==dname) %>% 
    filter(sex_name==gender) %>% 
    filter(year %in% c(1990,2019)) %>% 
    filter(metric_name=="Rate") %>% 
    group_by(year) %>% 
    nest()
  
  df1asr=df_region %>% filter(year==2019) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val,lower,upper) %>% 
    mutate(val=round(val,2),lower=round(lower,2),upper=round(upper,2)) %>% 
    set_names("location_name","location_id",paste0("2017_",index1,"_",c("ASR","ASR_low","ASR_up")))
  
  df2asr=df_region %>% filter(year==1990) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val,lower,upper) %>% 
    mutate(val=round(val,2),lower=round(lower,2),upper=round(upper,2)) %>% 
    set_names("location_name","location_id",paste0("1990_",index1,"_",c("ASR","ASR_low","ASR_up")))
  
  df_relative=df_region  %>% filter(year==2019) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val) %>% rename("ASR_2017"=3) %>% 
    left_join(df_region  %>% filter(year==1990) %>% purrr::pluck("data",1) %>% select(location_name,location_id,val) %>% rename("ASR_1990"=3)) %>% 
    mutate(relative_ASR=round(ASR_2017-ASR_1990,2),
           relative_change_ASR=round(100*relative_ASR/ASR_1990,2))
  
  df2=df2asr %>% left_join(df1asr) %>% left_join(df_relative %>% select(location_name,relative_ASR,relative_change_ASR))
  colnames(df2)=c("location_name","location_id","ASR-1990","ASR_low1990","ASR_up1990",
                  "ASR-2019","ASR_low2019","ASR_up2019","ASR-relative",
                  "ASR-relchange")
  
  ################################################
  # DALY EAPC
  df_region= GBD1990and2019 %>% 
    filter(location_id %in% name_all) %>% 
    filter(measure_name==index) %>% 
    filter(cause_id==dname) %>% 
    filter(age_name=="<20 years") %>%   #filter(cause_name=="Diabetes mellitus type 2") %>% 
    filter(sex_name==gender) %>% 
    filter(metric_name=="Rate") %>% 
    as_tibble()
  # Anuual year rate
  
  x=df_region %>% filter(val>0)
  if(dim(x)[1]==0){
    df3=tibble(location_id=name_all,
               EAPC=NA,
               EAPC_low=NA,
               EAPC_up=NA)
  }else{
    # Anuual year rate
    # CI
    annual_rate_CI = df_region  %>% 
      split(.$location_id) %>%
      purrr::map(~lm(log(val)~year, data = .x)) %>%
      map_df(confint, .id = 'location_id') %>% t() %>% as_tibble() %>% 
      set_names("Intercept","B") %>% mutate(B=round(B,4))
    
    lower=annual_rate_CI %>% select(B) %>% slice(seq(1,462,2))
    upper=annual_rate_CI %>% select(B) %>% slice(seq(2,462,2))
    
    #Anuual year rate
    annual_rate = df_region  %>% 
      split(.$location_id) %>%
      purrr::map(~lm(log(val)~year, data = .x)) %>%
      map_df(broom::tidy, .id = 'location_id') %>%
      filter(term == 'year') %>% select(location_id,estimate) %>% 
      mutate(lower=lower$B,upper=upper$B)
    # bind
    df3=annual_rate %>% select(location_id,estimate,lower,upper) %>% 
      mutate(estimate=round(eapc(estimate),3),lower=round(eapc(lower),3),upper=round(eapc(upper),3)) %>% 
      set_names("location_id","EAPC","EAPC_low","EAPC_up") %>% 
      mutate(location_id=as.numeric(location_id))
  }
  ################################################
  # DALY percentage Change
  df_region = GBD1990to2019 %>% 
    filter(measure_name==index) %>% 
    filter(age_name=="<20 years") %>% filter(cause_id==dname) %>% 
    filter(sex_name==gender) %>% 
    filter(metric_name=="Rate") %>% 
    as_tibble()
  
  df4=df_region %>% select(location_name,location_id,val,lower,upper) %>% 
    mutate(val=round(val,2),lower=round(lower,2),upper=round(upper,2)) %>% 
    set_names("location_name","location_id","Percentage","Percentage_low","Percentage_up") %>% 
    select(-location_name)
  
  ################################################
  ## Bind all
  df_DALY=df1 %>% left_join(df2) %>% mutate(cause_id=dname,.before=1) %>% 
    left_join(df4) %>% 
    left_join(df3)
  return(df_DALY)
}

# b=c()
# for (i in 1:7) {
#   a=table1(index="DALYs (Disability-Adjusted Life Years)",dname=x[i])
#   b=rbind(a,b)
# }
# 

x=table1(index="Incidence",dname=499,gender="Both")


