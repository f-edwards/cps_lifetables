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
  group_by(stfcid) %>% 
  filter(fy == min(fy)) %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(.imp, state, fy, age, race_ethn) %>% 
  summarise(tpr = n())

write_csv(afcars_fy, "./data/state_first_fc.csv")
write_csv(afcars_tpr, "./data/state_tpr.csv")
