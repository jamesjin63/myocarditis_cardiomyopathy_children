library(tidyverse)
library(readxl)
library(ggplot2)
library(ggrepel)
rm(list = ls())
eapc=function(x){
  return(100*(exp(x)-1))
}
filter=dplyr::filter
select=dplyr::select
# data read and process
load("Country204name.RData")
load("GBD499_Inc.Rdata")
load("/Users/Anderson/Desktop/GBD/myocarditis/GBD499_27loc.Rdata")

## all data 1990 to 2019
GBD1990_2019=GBD499_Inc
##############
##############
# load SDI
SDI=readxl::read_excel("/Users/Anderson/Desktop/GBD/SDI.XLSX",2) %>% 
  gather("year","sdi",-Location) %>% 
  mutate(year=as.numeric(year),
         SDI=str_replace(sdi,"·","."),
         sdi=as.numeric(SDI))  %>% select(-SDI) %>% rename("location_name"=1)
# all locations id and name
bb = readxl::read_excel("/Users/Anderson/Desktop/GBD/all.xlsx") %>% unique()
SDI=left_join(SDI,bb) %>% unique()


aa=GBD1990_2019 %>% select(cause_name,cause_id) %>% unique()
aaid=GBD1990_2019%>% select(measure_name,measure_name) %>% unique()

############### ############### ############### ############### ############### 
######### 1.1  DALY SDI 2019 in 204
############### ############### ############### ############### ############### 

###################################### 
######### 1.1.1  national level

asr_sdi=function(index="Deaths",disease=845){
  
  aa=GBD1990_2019 %>% select(cause_name,cause_id) %>% unique()
  bb=aa %>% filter(cause_id==disease) %>% select(cause_name) %>% pull
  ### SDI 2017
  SDI2019 = SDI %>% filter(year==2019) %>% 
    select(year,location_id,location_name,sdi) %>% 
    filter(location_id %in% name_204)
  
  # link
  df=GBD1990_2019 %>%  filter(location_id %in% name_204) %>% 
    filter(measure_name==index) %>% 
    #filter(age_name=="All Ages") %>%  #filter(cause_name=="Diabetes mellitus type 2") %>% 
    filter(age_id==158) %>%  
    filter(sex_name=="Both") %>% 
    filter(metric_name=="Rate") %>% 
    filter(year==2019) %>% 
    filter(cause_id==disease) %>% 
    select(location_id,location_name,val,lower,upper) %>%
    left_join(SDI2019,by="location_id") %>% unique() %>% 
    rename("location_name"=2)
  
  ## plot
  ggplot(df, aes(x=sdi, y=val)) + 
    geom_point(aes(col=location_name),size=0.8) + 
    geom_smooth(method="loess",se = F,color="black") +
    geom_text_repel(aes(label = location_name,col=location_name),size=3.2,  segment.size = 0.2)+
    
    labs(#title = paste0(index," ASR and SDI in National level"),
         x=paste0("SDI (",bb,")"),
         y="Incidence rate per 100,000 population (0 to 19 years old)")+
    theme(legend.position = "none")+
    scale_x_continuous(name=paste0("SDI (",bb,")"),limits = c(0.25,0.95),
                       breaks =  seq(0.25,0.95,0.1))+
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))
  
  # save pdf
  setwd("/Users/Anderson/Desktop/GBD/myocarditis/results")
  ggsave(paste0(index,"-",disease,"-ASR &SDI National level in 2019.pdf"),
         width = 12,height = 8,dpi=300)
  setwd("/Users/Anderson/Desktop/GBD/myocarditis")
  df_incidence_204 =df
  
  ###################################### 
  ######### 1.1.2  regional level
  ## SDI 2017 region 22
  SDI22= SDI %>% filter(location_id %in% c(name_21,1)) %>% 
    select(-location_name) %>% 
    mutate(year=as.integer(year),location_id=as.integer(location_id))
  
  
  DALY_2017 = GBDdf27 %>%
    filter(measure_name==index) %>% 
    filter(cause_id==disease) %>% 
    filter(age_name=="Age-standardized") %>% 
    filter(sex_name=="Both") %>% 
    filter(metric_name=="Rate") %>% 
    filter(location_id %in%  c(name_21,1) ) %>% 
    as.tbl() %>% 
    select(location_name,location_id,year,cause_name,val,upper,lower)
  
  #bin data
  df = left_join(DALY_2017,SDI22) 
  
  # get df
  df1=df %>% filter(sdi>0.2)
  df2=df %>% filter(!location_name=="Global")
  #plot
  ggplot(data=df2,aes(x=sdi,y=val))+
    geom_point(data=df1,aes(x=sdi,y=val,shape=location_name, 
                            color=location_name)) +
    scale_shape_manual(values = c(19,1,2,3,4,
                                  1,2,3,4,7,
                                  1,2,3,4,7,
                                  1,2,3,4,7,
                                  1,2),
                       breaks = c(
                         "Global","High-income Asia Pacific","High-income North America","Western Europe",
                         "Australasia","Andean Latin America", "Tropical Latin America","Central Latin America",
                         "Southern Latin America","Caribbean","Central Europe","Eastern Europe","Central Asia",
                         "North Africa and Middle East","South Asia","Southeast Asia","East Asia","Oceania",
                         "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa",
                         "Southern Sub-Saharan Africa"),
                       labels= c(
                         "Global","High-income Asia Pacific","High-income North America","Western Europe",
                         "Australasia","Andean Latin America", "Tropical Latin America","Central Latin America",
                         "Southern Latin America","Caribbean","Central Europe","Eastern Europe","Central Asia",
                         "North Africa and Middle East","South Asia","Southeast Asia","East Asia","Oceania",
                         "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa",
                         "Southern Sub-Saharan Africa")) +
    scale_color_manual(values = c("black","red","red","red","red",#5
                                  "#D89000","#D89000","#D89000","#D89000","#D89000",
                                  "#39B600","#39B600","#39B600","#00C1AA","#ED8141",
                                  "blue", "blue", "blue","purple","purple",
                                  "purple","purple"),
                       breaks = c(
                         "Global","High-income Asia Pacific","High-income North America","Western Europe",
                         "Australasia","Andean Latin America", "Tropical Latin America","Central Latin America",
                         "Southern Latin America","Caribbean","Central Europe","Eastern Europe","Central Asia",
                         "North Africa and Middle East","South Asia","Southeast Asia","East Asia","Oceania",
                         "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa",
                         "Southern Sub-Saharan Africa"),
                       labels= c(
                         "Global","High-income Asia Pacific","High-income North America","Western Europe",
                         "Australasia","Andean Latin America", "Tropical Latin America","Central Latin America",
                         "Southern Latin America","Caribbean","Central Europe","Eastern Europe","Central Asia",
                         "North Africa and Middle East","South Asia","Southeast Asia","East Asia","Oceania",
                         "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa",
                         "Southern Sub-Saharan Africa")
    )+
    geom_smooth(span = 0.8,  se = F,color="black")+ 
    scale_x_continuous(name=paste0("SDI (",bb,")"),limits = c(0.25,0.95),
                       breaks =  seq(0.25,0.95,0.1))+
    labs(#title = paste0(index," ASR and SDI in National level from 1990-2019"),
         shape="",color="",
         x=paste0("SDI (",bb,")"),
         y="Age-standardised rate per 100,000 population")+
    theme_bw() +
    theme(legend.key.size = unit(0.03,"line"),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_blank(),
          legend.title=element_text(size=12),
          axis.line = element_line(colour = "black"),
          legend.text=element_text(size=8)) +
    guides(shape = guide_legend(nrow = 22))
  
  # save pdf
  setwd("/Users/Anderson/Desktop/GBD/myocarditis/results")
  ggsave(paste0(index,"-",disease ,"-ASR &SDI Regional level in 2019.pdf"),
         width = 12,height = 8,dpi=300)
  setwd("/Users/Anderson/Desktop/GBD/myocarditis")
  df_incidence_22 =df2
  
  return(df=list(df_incidence_204,df_incidence_22))
}

# a = asr_sdi(index="Deaths",disease=845)

a = asr_sdi(index="Incidence",disease=499)




write.csv(a[[1]],"Incidence-SDI.csv")


