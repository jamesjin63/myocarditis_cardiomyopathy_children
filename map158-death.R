library(tidyverse)
library(readxl)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(mapdata)
rm(list = ls())
filter=dplyr::filter
select=dplyr::select
load("Country204name.RData")
load("GBD499_DALY.Rdata")
load("/Users/Anderson/Desktop/GBD/myocarditis/GBD499_27loc.Rdata")
load("/Users/Anderson/Desktop/GBD/code/Maploc_id.Rdata")
GBD1990_2019=GBD499_DALY



df_incidence=GBD1990_2019 %>% 
  filter(location_id %in% name_204) %>% 
  #filter(age_name=="All Ages") %>%  #filter(cause_name=="Diabetes mellitus type 2") %>% 
  filter(sex_name=="Both") %>% 
  filter(metric_name=="Rate") %>% 
  filter(year==2019) %>% 
  filter(age_id==158) %>% 
  filter(measure_id==1)

world = ne_countries(scale = "medium", returnclass = "sf")
class(world)
df_world = world %>% 
  #dplyr::filter(pop_est>100000) %>% 
  select(name,name_long,brk_a3,pop_est,gdp_md_est) 

# map data
df_loc=loc_id %>% select(brk_a3,location_id) %>% 
  mutate(location_id=ifelse(location_id==8,6,location_id))

df_world=left_join(df_world,df_loc) %>% select(name_long,brk_a3,location_id)



#### Test which country dose not show on map
#### name 204 in loc_id
a=loc_id %>% select(location_id) %>% as_tibble()
b=tibble(location_id=name_204)
aa=setdiff(b,a)
aa %>% left_join(df_incidence %>% select(location_name,location_id));rm(a,aa,b)
#### End 
####################################################
## Figure 1 
## with national country map by 
## In all ages and all gender
## by ASR (2019) /annual year change rate(1990-2019)
####################################################
index = "Incidence"
dfplot = df_incidence 

index=index
dfplot=dfplot  
################################################################################################
# 1.1 ASR
################################################################################################
########################
# Incidence

ASR=dfplot %>%  select(location_id,location_name,val) %>% 
  rename("val"=3)
df_asr=left_join(df_world,ASR) %>% filter(location_id>0)

#plot density
ggplot(df_asr, aes(x=val)) + 
  geom_histogram(aes(y=..density..),bins = 20, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title = "The density of percentage changes",x="ASR 2019") 

# map
ggplot(data =df_asr) +
  geom_sf(aes(fill = val),size = 0.05) +
  scale_fill_viridis_c(name="Incidence ASR",option = "plasma", trans = "sqrt") +
  labs(title = paste0("Incidence ASR of ","da")) +
  theme_light()->p
p

library(RColorBrewer)
# cut braek
df_asr=df_asr %>% mutate(asr=replace_na(val,-99)) %>% 
  mutate(asr_cut=cut(asr,
                     breaks = c(-Inf,1,2,3,Inf),
                     labels = c("0 to 1", "1 to 2",
                                ">2","> 3") ))


# plot map
ggplot(df_asr) +
  geom_sf(aes(geometry = geometry, fill = asr_cut),size = 0.1)+
  labs(title ="") +
  scale_fill_manual(name="Age-standardized death rate",
                    #values = (c(brewer.pal(n = 9, name = "BuPu")[3:9])),
                    values = rev(c(brewer.pal(n = 12, name = "PiYG")[3:6])),
                    guide = guide_legend(reverse=T))->p
p

p1=p+ theme(axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         legend.position = c(0.10, 0.29),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.title=element_text(size=8),
         legend.text=element_text(size=8),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank())

################################################################################################
# 1.3 NUmber
################################################################################################
########################
dfplotx=GBD1990_2019 %>% 
  filter(location_id %in% name_204) %>% 
  filter(measure_id==1) %>% 
  #filter(age_name=="All Ages") %>%  #filter(cause_name=="Diabetes mellitus type 2") %>% 
  filter(sex_name=="Both") %>% 
  filter(metric_name=="Number") %>% 
  filter(year==2019) %>% 
  filter(age_id==158)
# Incidence
ASR=dfplotx %>%  select(location_id,location_name,val) %>% 
  rename("estimate"=3)
df_asr=left_join(df_world,ASR) %>% filter(location_id>0)

# map
ggplot(data =df_asr) +
  geom_sf(aes(fill = estimate/1000),size = 0.05) +
  scale_fill_viridis_c(name="Number of cases \n(per thousand)",option = "mako", direction = -1) +
  theme_light()->p


p2=p+ theme(axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         legend.position = c(0.10, 0.29),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.title=element_text(size=8),
         legend.text=element_text(size=8),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank())


library(patchwork)
p1/p2+plot_annotation(tag_levels = 'A')
# save pdf
setwd("/Users/Anderson/Desktop/GBD/myocarditis/results")
ggsave(paste0("Map 0-20-death.pdf"),height = 12,width = 10,dpi = 500)
setwd("/Users/Anderson/Desktop/GBD/myocarditis")

