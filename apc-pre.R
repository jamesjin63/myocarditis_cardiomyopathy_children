dfres=apc17(preyear=2035,locid=1)

######################################################################
### Age specific rate
dfp=agespec.proj(dfres)[[1]]  %>% as_tibble() %>% 
  mutate(Time=1990:2035,group="Under 5") %>% 
  set_names("val","sd","low","median","up","Time","group") %>% 
  bind_rows(
    agespec.proj(dfres)[[2]]  %>% as_tibble() %>% 
      mutate(Time=1990:2035,group="5 to 9") %>% 
      set_names("val","sd","low","median","up","Time","group")
  ) %>% 
  bind_rows(
    agespec.proj(dfres)[[3]]  %>% as_tibble() %>% 
      mutate(Time=1990:2035,group="10 to 14") %>% 
      set_names("val","sd","low","median","up","Time","group")
  ) %>% 
  bind_rows(
    agespec.proj(dfres)[[4]]  %>% as_tibble() %>% 
      mutate(Time=1990:2035,group="15 to 19") %>% 
      set_names("val","sd","low","median","up","Time","group")
  )

### ggplot
ggplot(data=dfp,aes(Time,val))+
  geom_point(color = '#8ac6d1')+
  geom_line(color = '#8ac6d1')+
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)+
  geom_point(data=dfp %>% filter(Time>2019),aes(Time,val,color="red"))+
  geom_line(data=dfp %>% filter(Time>2019),aes(Time,val,color="red"))+
  geom_vline(xintercept=2019, linetype="dashed", color = "grey")+
  scale_x_continuous(breaks = seq(1990, 2035, by = 5))+
  facet_wrap(vars(group), ncol = 2,scales = "free")+
  labs(y="Incident cases")+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.position="none"
  )

ggsave("AgeSpec.pdf",height = 8,width = 14,dpi = 500)

######################################################################
### ASR

dfa=agestd.proj(dfres) %>% as_tibble() %>% 
  mutate(Time=1990:2035,group="ASR") %>% 
  set_names("val","sd","low","median","up","Time","group")

dfa=agestd.rate(dfres) %>% as_tibble() %>% 
  mutate(Time=1990:2035,group="ASR") %>% 
  set_names("val","sd","low","median","up","Time","group") %>% 
  mutate(val=val*100000,
         low=low*100000,
         up=up*100000
         )


ggplot(data=dfa,aes(Time,val))+
  geom_point(color = '#a696c8')+
  geom_line(color = '#a696c8')+
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)+
  geom_point(data=dfa %>% filter(Time>2019),aes(Time,val,color="red"))+
  geom_line(data=dfa %>% filter(Time>2019),aes(Time,val,color="red"))+
  geom_vline(xintercept=2019, linetype="dashed", color = "grey")+
  scale_x_continuous(breaks = seq(1990, 2035, by = 5))+
  labs(y="ASR")+ylim(4.5,11)+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.position="none"
  )

ggsave("ASR.pdf",height = 4,width = 4,dpi = 500)


pspe=function(nax="Under 5"){
  p1= ggplot(data=dfp %>% filter(group==nax) ,aes(Time,val))+
    geom_point(color = '#8ac6d1')+
    geom_line(color = '#8ac6d1')+
    geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)+
    geom_point(data=dfp %>% filter(Time>2019)%>% filter(group==nax),aes(Time,val,color="red"))+
    geom_line(data=dfp %>% filter(Time>2019)%>% filter(group==nax),aes(Time,val,color="red"))+
    geom_vline(xintercept=2019, linetype="dashed", color = "grey")+
    scale_x_continuous(breaks = seq(1990, 2035, by = 5))+
    labs(y="Incident cases",title = nax)+
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12),
          plot.title=element_text(hjust=0.5),
          legend.position="none"
    )
  return(p1)
}

p1=pspe(nax="Under 5")
p2=pspe(nax="5 to 9")
p3=pspe(nax="10 to 14")
p4=pspe(nax="15 to 19")


library(patchwork)
(p1+p2)/
  (p3+p4)

ggsave("ASRx.pdf",height = 8,width = 12,dpi = 500)






