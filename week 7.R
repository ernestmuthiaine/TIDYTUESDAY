
#tidytuesday week 7: wealth and income over time

#loading the dataset
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')

#loading package(tidyverse#)
library(tidyverse)


#to calculate the difference in Average lifetime earning by gender
df<-lifetime_earn%>%
  pivot_wider(names_from=gender,
              values_from=lifetime_earn)%>%
  mutate(diff=Men-Women)%>%
  pivot_longer(cols=c(Men,Women),
               names_to='gender',
               values_to='lifetime_earn')


males<-df%>%filter(gender=='Men') #filter out men
females<-df%>%filter(gender=='Women') #filter out women 


#visualizing the difference in Average lifetime earning by race/gender
plot<-ggplot(df)+
  geom_segment(data=males,
               aes(x=lifetime_earn,y=race,
                   xend=females$lifetime_earn,yend=females$race),
               color='grey',
               alpha=0.5,size=2.5)+
  geom_point(aes(x=lifetime_earn,y=race,color=gender),size=1.5)+
  geom_text(data=df,aes(label=lifetime_earn,x=lifetime_earn,y=race),
            hjust=0.5,vjust=-0.5,size=2.8)+
  labs(y='race',
       x='lifetime earnings',
       caption='Twitter: @ernestmuthiaine')+
  ggtitle('Average lifetime earning by race/gender')+
  theme(panel.background=element_rect(fill='white'),
        axis.text.x=element_blank(),
        plot.title=element_text(hjust=0.2))

plot

#saving the image

ggsave('plot.png')
