#loading the dataset
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
library(ggthemes)
library(tidyverse)


#filtering data for kenyan olympians and arranging it per year
kenya <-olympics %>% 
  filter(team=='Kenya') %>% 
  arrange(year)

#separating year and the type of olympics to summer and winter
#then filter summer games
kenya1<-kenya %>% 
  separate(col=games, 
           into =c( 'year','type'),
           sep = ' ') %>% 
  filter(type=='Summer') %>% 
  arrange(year)

kenya1 %>% 
  filter(medal != "NA") %>% 
  group_by(year,medal) %>% 
  count() %>% 
  ggplot(aes(x=year,y=n,fill=medal))+
  geom_bar(stat = 'identity')+
  labs(title='Medals Won By Kenyans (Summer Olympics)',
       x='Year',
       y='Number of medals')+
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),
        axis.text = element_text(angle=90))
ggsave('kenyan_medals.png')



# the kenya vs ethiopia rivalry
kenya_ethiopia <-olympics %>% 
  separate(col=games,
           into = c('games','type'),
           sep=' ') %>% 
  filter(team %in% c('Ethiopia','Kenya'),
         type=='Summer')%>% 
  arrange(year)


kenya_ethiopia_plot<-kenya_ethiopia %>% 
  drop_na() %>% 
  group_by(team,medal) %>% 
  count() %>% 
  ggplot(aes(x=fct_reorder(medal,-n),y=n,fill=team))+
  geom_bar(stat='identity',width=0.5,position='dodge')+
  labs(title="Kenya Vs Ethiopia at the Olympics",
       x='Medal',
       y='medals won')+
  scale_fill_manual(values = c('Yellow','Red'))+
  theme_hc()
ggsave("kenya_vs_ethiopia.png")

