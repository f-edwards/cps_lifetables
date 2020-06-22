rm(list=ls())
gc()
library(tidyverse)
library(mice)

### first investigations
# ncands_fy<-read_csv("./data/ncands_subyr_xwalk.csv") 
# ncands_fy<-ncands_fy %>% 
#   mutate(st_id = paste(staterr, chid, sep="")) %>% 
#   select(-staterr, -chid) %>% 
#   distinct() 

index_first_investigation<-read_csv("./data/ncands_first_report_index.csv") 

index_first_victim<-read_csv("./data/ncands_first_victim_index.csv") 


files<-list.files("./data")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./data/", ncands_imputations, sep = "")
ncands_imputations<-ncands_imputations[6:9]

first_investigations<-list()
first_victims<-list()
  
for(i in 1:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = "")) %>% 
    select(.imp, .id, st_id, chid, staterr, rptdt, age, race_ethn, rptvictim) 

  first_investigations[[i]]<-index_first_investigation %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid))) 
  
  first_victims[[i]]<-index_first_victim %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid)))
  
  # first_investigations[[i]]<-temp_first_inv %>% 
  #   group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  #   summarise(first_inv = n()) %>% 
  #   ungroup()
  # 
  # first_victims[[i]]<-temp_first_victim %>% 
  #   filter(rptvictim==1) %>% 
  #   group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  #   summarise(first_victim = n()) %>% 
  #   ungroup()
  # 
  # gc()
}

inv_out<-bind_rows(first_investigations) %>% 
  ungroup() %>%   
  filter(!staterr%in%c("PA", "GA", "RI")) %>% 
  arrange(rptdt) %>% 
  group_by(.imp, st_id) %>% 
  slice(1)

# ### identical number of unique IDs in index and imputed
# inv_out %>% 
#   group_by(.imp) %>% 
#   summarise(n = n())

inv_out<-inv_out%>% 
  group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  summarise(first_inv=n())

victim_out<-bind_rows(first_victims)%>% 
  arrange(rptdt) %>% 
  group_by(.imp, st_id) %>% 
  slice(1)

victim_out<-victim_out %>% 
  group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  summarise(first_victim=n())

write_csv(inv_out, "./data/state_first_inv.csv")
write_csv(victim_out, "./data/state_first_victim_out.csv")