library(tidyverse)
library(readr)

mapping<-read_csv("mapping.csv")

mapping<-mapping%>%
  select(-SpecialNotes)

g<-read_csv("gini_time-series.csv")

g<-merge(mapping, g, by="Country Code")

gini<-g%>%
  select(-`Indicator Code`, -`Indicator Name`, -TableName)%>%
  gather(key=year, value = gc, -`Country Name`, -`Country Code`, -Region, -IncomeGroup)%>%
  rename(country=`Country Name`)%>%
  rename(ccode=`Country Code`)%>%
  mutate(year=as.numeric(year))%>%
  filter(!is.na(gc))%>%
  mutate(IncomeGroup=factor(IncomeGroup, levels=c("Low income",
                                                     "Lower middle income",
                                                     "Upper middle income",
                                                     "High income")))



gini_subset<-gini%>%
  group_by(country)%>%
  filter(n()>10)%>%
  arrange(country, year)

gini_subset%>%
  filter(year>1985)%>%
  ggplot(aes(year, gc, colour=country))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(cols=vars(IncomeGroup))+
  labs(x="", y="Gini coefficient", 
       caption = "Reminder: low gini coefficient means more equal society")



gini_subset%>%filter(IncomeGroup=="High income" & gc>50)
