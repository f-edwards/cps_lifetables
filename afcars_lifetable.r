library(tidyverse)

source("read.r")
source("lifetable.r")

### scratch
dat<-afcars_nat_year%>%
  rename(var = first_fc) %>% 
  ungroup() 
# add total to table
dat<-dat %>% 
  bind_rows(afcars_nat_year %>% 
              group_by(.imp,year, age) %>% 
              summarise(var= sum(first_fc),
                        pop = sum(pop)) %>% 
              mutate(race_ethn = "Total")) #%>% 
  # mutate(sex = case_when(
  #   sex==1 ~ "Male",
  #   sex==2 ~ "Female"
  #))

### run life tables by imp, race_ethnb, sex
race<-unique(dat$race_ethn)
sex<-unique(dat$sex)
years<-unique(dat$year)
tables_out<-list()

counter<-0
for(i in 1:5){
  for(r in 1:length(race)){
    #for(s in 1:length(sex)){
      for(y in 1:length(years)){
        counter<-counter + 1
        
        temp<-dat %>% 
          filter(.imp == i,
                 race_ethn == race[r],
                 #sex == sex[s],
                 year == years[y])
        
        tables_out[[counter]]<-make_life_table(temp)
      }
    }
  }
#}

tables<-bind_rows(tables_out)

ggplot(tables, aes(x = age, y = c ,
                   color = race_ethn)) + 
  geom_line() + 
  facet_wrap(~year) + 
  ylab("Cumulative entry risk") +
  ggsave("./vis/fc_cumulative.png")

tables<-write_csv(tables, "./vis/fc_lifetables.csv")