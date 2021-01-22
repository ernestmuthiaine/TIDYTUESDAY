# TidyTuesday week 4: Kenya census data

#loading the data
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')


#loading tidyverse
library(tidyverse)

meru_crops_plot<-crops %>% 
  select(-Farming) %>% 
  filter(SubCounty=='MERU') %>%
  select(-SubCounty) %>% 
  gather(key='Crops',
         value = 'Population')  %>% 
  mutate(percentage=round(Population/sum(Population,na.rm=T)*100,1)) %>% 
  ggplot(aes(x=reorder(Crops,-percentage),y=percentage,fill=Crops))+
  geom_bar(stat='identity')+
  labs(title='HOUSEHOLDS GROWING PERMANENT CROPS IN MERU COUNTY',
       X='Crops',
       y='percentage',
       caption='Twitter: @MuthiaineErnest')+
  geom_text(aes(label=paste0(percentage,'%')),vjust=-0.5,na.rm=T)+
  theme(plot.title=element_text(size=14,face='bold'),
        panel.background =element_rect(fill=NA),
        axis.text.x = element_text(angle = 90),
        axis.title.y=element_blank(),
        axis.title = element_blank(),
        legend.position = 'none')

ggsave('meru_crops_plot.png',width=5,height=5)
