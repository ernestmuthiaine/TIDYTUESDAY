#loading the dataset
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#loading tidyverse
library(tidyverse)

#view the data
view(plastics)

# of countries in the dataset

countries<-plastics %>% 
  select(country) %>% 
  unique()



#filtering the data for the east african countries
ea_plastics<-plastics %>% 
  select(everything()) %>% 
  filter(country %in% c('Kenya','Tanzania','Rwanda'))

view(ea_plastics)


#pivoting the data to create a tidy dataset
ea_plastics1<-ea_plastics %>% 
  gather(key = 'plastic_type',
         value='plastic_count',
         -c(country,year,parent_company,volunteers,grand_total,empty,num_events)) 

#which are the most common pollutants in Kenya,Rwanda and Tanzania?
ea_plastics1 %>% 
  select(everything()) %>% 
  group_by(country,plastic_type) %>% 
  summarise(total=sum(plastic_count)) %>% 
  drop_na(total) %>% 
  ggplot(aes(x=plastic_type,y=total,fill=plastic_type))+
  geom_bar(stat='identity',width = 0.5,na.rm=T)+
  labs(title='plastic pollution in kenya,tanzania and rwanda',
       x='plastic',
       y='count',
       caption = 'Twitter: @MuthiaineErnest')+
  facet_wrap(~country ,scales='free_y',nrow=2) +
  theme(panel.background = element_rect(fill=NA),
        plot.title = element_text(face='bold'),
        axis.text.x = element_text(angle = 90))

#saving the image
ggsave('east africa pollutants.png')

