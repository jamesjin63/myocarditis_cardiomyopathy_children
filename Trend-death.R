library(tidyverse)
library(readxl)
library(stringr)
library(patchwork)
rm(list = ls())
filter=dplyr::filter
select=dplyr::select
load("Country204name.RData")
load("/Users/Anderson/Desktop/GBD/myocarditis/GBD499_27loc.Rdata")
source("Tab1.R")

############################################################
############ 1. Trends with Four age-group
############################################################
dfNum = GBDdf27 %>% 
  filter(location_id %in% c(1,44634:44637,44639)) %>% ## location id=1 is global
  filter(measure_id==1) %>% ## Deaths
  filter(sex_id==3) %>% # both sex
  filter(metric_name=="Rate") %>% 
  filter(age_id %in% c(1,6,7,8))

#agegroup=GBD393 %>% select(age_id,age_name) %>% unique()


ggplot(data=dfNum)+
  geom_point(aes(year,val,color=location_name),size=0.3)+
  geom_line(aes(year,val,color=location_name))+
  facet_wrap(vars(age_name), ncol = 2,scales = "free")+
  labs(y="Age-standardized deaths rate",x="Time")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.title = element_blank()
  )

ggsave("Trend-4age+5region-Deaths.pdf",height = 8,width = 12,dpi = 500)

############################################################
############ 2. Trends with Gender in male and Female
############################################################

dfNum = GBDdf27 %>% 
  filter(location_id %in% c(1)) %>% ## location id=1 is global
  filter(measure_id==1) %>% ## Deaths
  filter(!sex_id==3) %>% # both sex
  filter(metric_name=="Rate") %>% 
  filter(age_id %in% c(1,6,7,8))

#agegroup=GBD393 %>% select(age_id,age_name) %>% unique()
ggplot(data=dfNum)+
  geom_point(aes(year,val,color=sex_name))+
  geom_line(aes(year,val,color=sex_name))+
  facet_wrap(vars(age_name), ncol = 2,scales = "free")+
  labs(y="Age-standardized deaths rate",x="Time",title = "Global")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.title = element_blank()
  )
ggsave("Trend-sex+globalDeaths.pdf",height = 8,width = 12,dpi = 500)



agegender=function(id=1){
  dfNum = GBDdf27 %>% 
    filter(location_id %in% c(id)) %>% ## location id=1 is global
    filter(measure_id==1) %>% ## Deaths
    filter(!sex_id==3) %>% # both sex
    filter(metric_name=="Rate") %>% 
    filter(age_id %in% c(1,6,7,8))
  
  #agegroup=GBD393 %>% select(age_id,age_name) %>% unique()
  p1=ggplot(data=dfNum)+
    geom_point(aes(year,val,color=sex_name))+
    geom_line(aes(year,val,color=sex_name))+
    facet_wrap(vars(age_name), ncol = 4,scales = "free")+
    labs(y="Age-standardized deaths rate",x="Time")+
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          legend.title = element_blank()
    )
  return(p1)
}

p1=agegender(id=1)
p2=agegender(id=44634)
p3=agegender(id=44635)
p4=agegender(id=44636)
p5=agegender(id=44637)
p6=agegender(id=44639)

library(patchwork)
p1/p2/p3/p4/p5/p6+plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect')

ggsave("Trend-sex+5region-Deaths.pdf",height = 18,width = 8,dpi = 500)





################
## 21 regions
df=GBDdf27 %>% filter(location_id %in% name_21) %>% 
  filter(!sex_name=="Both") %>% 
  filter(cause_id==499) %>% 
  filter(measure_id==1) %>% ## Deaths
  filter(metric_name=="Rate") %>% 
  filter(age_id %in% c(1,6,7,8)) %>% 
  select(location_id,location_name,age_name,cause_id,year,sex_name,val) 

age_name=tibble(age_name=c("Under 5","5-14 years","15-49 years","50-69 years","70+ years"),
                age_id=c(1,6,7,8,9,158))

region = tibble(location_name=c("High-income Asia Pacific","High-income North America","Western Europe",
                                "Australasia","Andean Latin America", "Tropical Latin America","Central Latin America",
                                "Southern Latin America","Caribbean","Central Europe","Eastern Europe","Central Asia",
                                "North Africa and Middle East","South Asia","Southeast Asia","East Asia","Oceania",
                                "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa",
                                "Southern Sub-Saharan Africa"),
                id=c(1:21))



# Male
df1=df %>% filter(sex_name=="Male") %>% 
  filter(year==2019) %>% left_join(region)

df2=df %>% filter(sex_name=="Male") %>% 
  mutate(val=1*val)%>% filter(year==1990) %>% left_join(region)

#femlae
df3=df %>% filter(sex_name=="Female") %>% 
  filter(year==2019) %>% 
  mutate(val=-1*val)%>% left_join(region)

df4=df %>% filter(sex_name=="Female") %>% 
  mutate(val=-1*val) %>% filter(year==1990) %>% left_join(region)

## plot
ggplot() +
  geom_bar(data=df1, aes(x =id, y =val, fill =age_name),stat = "identity",width = 0.25) +
  geom_bar(data=df2, aes(x =df1$id+0.28, y =val, fill =  age_name),stat = "identity",width = 0.25) +
  geom_bar(data=df3, aes(x =id, y =val, fill =  age_name),stat = "identity",width = 0.25) +
  geom_bar(data=df4, aes(x =df1$id+0.28, y =val, fill =  age_name),stat = "identity",width = 0.25) +
  scale_fill_manual(values =rev(c("#d53e4f","#f47c2f","#aad8da","#8ac7a0","#739592")))+
  scale_x_continuous(breaks = c(1:21),labels =  paste0(region$location_name))+
  scale_y_continuous(breaks = seq(-10, 10, 2.5), labels = abs(seq(-10, 10, 2.5)))+
  geom_hline(yintercept = 0)+
  #coord_flip() +
  #scale_x_continuous(breaks = seq(0.5,6.5,1))+
  labs(x="",y="(Female)   Death rate   (Male)",fill="Age")+
  annotate(geom="text", 
           x=c(0.98,1.28,1.98,2.28,2.98,3.28,3.98,4.28,
               4.98,5.28,5.98,6.28,6.98,7.28,7.98,8.28,
               8.98,9.28,9.98,10.28,10.98,11.28,11.98,12.28,
               12.98,13.28,13.98,14.28,14.98,15.28,15.98,16.28,
               16.98,17.28,17.98,18.28,18.98,19.28,19.98,20.28,
               20.98,21.28), y=0, 
           label=c(rep(c("1990","2019"),21)),
           color="white",angle=90,size=3)+
  
  theme_bw() +
  theme(#legend.position = "none",
    strip.text.x = element_text(margin = margin(0, 0, 0, 0)),
    text = element_text(size=10,face="bold"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1,size=9,face="bold"),
    #strip.text.y = element_blank(), 
    strip.background.y = element_rect(fill="gray"),
    #strip.text = element_text(size=10,face="bold"),
    strip.background = element_blank(),
    axis.line = element_line(color = 'black'))


ggsave("Trend-sex+21region-deaths.pdf",height = 8,width = 12,dpi = 500)





