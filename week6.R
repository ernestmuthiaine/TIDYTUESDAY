#loading the dataset
all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

#loading the packages to use
library(tidyverse)
library(directlabels)

all%>%
  select(Year,Males,Females)%>%
  pivot_longer(cols=c(Males,Females),
               names_to='gender',
               values_to='students')%>%
  ggplot(aes(x=Year,y=students,color=gender))+
  geom_line()+
  geom_point(alpha=0.3)+
  geom_dl(aes(label=gender),method=list("last.points"))+
  xlim(1976,2020)+
  labs(title ='#tidytuesday week6' )+
  theme(panel.background = element_rect(fill=NA),
        plot.title = element_text(face='italic'),
        legend.position = 'none')

#saving the image

ggsave('male female student enrollment.png',height=10,width=20,units = 'cm')
