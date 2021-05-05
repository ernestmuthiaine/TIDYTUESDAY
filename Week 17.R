rm(list=ls())
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

#loading libraries
library(tidyverse)
library(ggthemes)
library(lubridate)
names(water)


kenya<-water %>% filter(country_name=='Kenya')


#most common water sources in kenya
df<-kenya %>% 
  group_by(water_source) %>% 
  count() %>% drop_na() %>%
  ungroup() %>% 
  mutate(percentage=(n/sum(n)*100)) %>% 
  arrange(desc(n))

water_sources_plot<-df %>% 
  ggplot(aes(reorder(water_source,-n),n,fill=water_source))+
  geom_bar(stat='identity')+
  labs(title='Water Sources in Kenya',
       y='number',
       x='water source')+
  coord_flip()+
  theme_hc()+
  theme(legend.position = 'none')
  

ggsave('water_sources_plot.png')


