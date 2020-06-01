rm(list=ls())
gc()
library(tidyverse)
library(mice)

### first investigations
ncands_fy<-read_csv("./data/ncands_subyr_xwalk.csv") 
ncands_fy<-ncands_fy %>% 
  mutate(st_id = paste(staterr, chid, sep="")) %>% 
  select(-staterr, -chid) %>% 
  distinct() %>% 
  group_by(st_id, rptdt) %>% 
  filter(subyr == min(subyr)) %>% 
  ungroup()

index_first_investigation<-read_csv("./data/ncands_first_report_index.csv") %>% 
  distinct() %>% 
  left_join(ncands_fy) 

index_first_victim<-read_csv("./data/ncands_first_victim_index.csv") %>% 
  distinct() %>% 
  left_join(ncands_fy) 


files<-list.files("./data")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./data/", ncands_imputations, sep = "")

first_investigations<-list()
first_victims<-list()
  
for(i in 1:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = ""))
  
  temp_first_inv<-index_first_investigation %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid)))
  
  temp_first_victim<-index_first_victim %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid)))
  
  first_investigations[[i]]<-temp_first_inv %>% 
    group_by(.imp, subyr, staterr, race_ethn, age) %>% 
    summarise(first_inv = n()) %>% 
    ungroup()
  
  first_victims[[i]]<-temp_first_victim %>% 
    filter(rptvictim==1) %>% 
    group_by(.imp, subyr, staterr, race_ethn, age) %>% 
    summarise(first_victim = n()) %>% 
    ungroup()
  
  gc()
}

inv_out<-bind_rows(first_investigations) %>% 
  ungroup()

inv_out<-inv_out%>% 
  group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  summarise(first_inv=sum(first_inv))

victim_out<-bind_rows(first_victims)

victim_out<-victim_out %>% 
  group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  summarise(first_victim=sum(first_victim))

write_csv(inv_out, "./data/state_first_inv.csv")
write_csv(victim_out, "./data/state_first_victim_out.csv")