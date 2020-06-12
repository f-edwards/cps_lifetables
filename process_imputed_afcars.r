library(tidyverse)

### first investigations
afcars<-read.csv("./data/afcars_imputed_all_cases.csv", 
                 stringsAsFactors = F)
afcars_xwalk<-read.csv("./data/afcars_id_xwalk.csv",
                       stringsAsFactors = F)

afcars_fy<-afcars %>% 
  filter(totalrem==1, entered==1) %>% 
  left_join(afcars_xwalk %>% 
              select(fy, stfcid) %>% 
              distinct()) %>% 
  group_by(.imp, state, fy, age, race_ethn) %>% 
  summarise(first_entry = n())

afcars_rem1<-afcars %>% 
  filter(totalrem==1, entered==1) %>% 
  group_by(.imp, state, year, age, race_ethn) %>% 
  summarise(first_entry = n())

afcars_tpr<-afcars %>% 
  filter(istpr==1) %>% 
  left_join(afcars_xwalk %>% 
              select(fy, stfcid) %>% 
              distinct()) %>% 
  group_by(stfcid, .imp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(.imp, state, fy, age, race_ethn) %>% 
  summarise(tpr = n()) %>% 
  ungroup()

### complete the zeroes
temp<-expand_grid(unique(afcars_tpr$.imp),
                  unique(afcars_tpr$state),
                  unique(afcars_tpr$fy),
                  unique(afcars_tpr$age),
                  unique(afcars_tpr$race_ethn)) 

names(temp)<-c(".imp", "state", "fy", "age", "race_ethn")

temp<-left_join(temp,
                afcars_tpr) %>% 
  mutate(tpr = ifelse(is.na(tpr), 0, tpr))


write_csv(afcars_fy, "./data/state_first_fc.csv")
write_csv(temp, "./data/state_tpr.csv")
