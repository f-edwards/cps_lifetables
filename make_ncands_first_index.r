rm(list=ls())
gc()
library(tidyverse)
library(mice)

### first investigations

files<-list.files("./data")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./data/", ncands_imputations, sep = "")
ncands_imputations<-ncands_imputations[6:9]

ncands_out<-list()
for(i in 1:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F) %>% 
    filter(.imp==1)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = ""))
  
  ncands_out[[i]]<-temp
}

ncands_out<-bind_rows(ncands_out)

### recode NA on chprior to 9 so it evals correctly below

ncands_index<-read_csv("./data/ncands_subyr_xwalk.csv") %>% 
  mutate(st_id = paste(staterr, chid, sep="")) %>% 
  select(-chid) %>% 
  mutate(chprior = ifelse(is.na(chprior, 9, chprior)))

### this matches the maltreatment report
# ncands_victims<-ncands_index %>% 
#   filter(rptvictim==1) %>% 
#   group_by(staterr, subyr) %>% 
#   summarise(n  = n_distinct(st_id))

### sort by report date, take first ID for each row
first_report<-ncands_out %>% 
  arrange(rptdt) %>% 
  group_by(st_id) %>% 
  slice(1) %>% 
  ungroup() %>%  ### correct for non-linkage states
  filter(!staterr%in%c("PA", "GA", "RI")) %>% 
  select(st_id, rptdt)

first_victim<-ncands_out %>% 
  left_join(ncands_index %>% 
              select(rptdt, st_id, chprior) %>% 
              distinct()) %>% 
  filter(rptvictim=="1", chprior!=1) %>% 
  arrange(rptdt) %>% 
  group_by(st_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(st_id, rptdt)

first_report<-first_report %>% 
  left_join(ncands_index %>% 
              select(st_id, rptdy, subyr) %>% 
              distinct())


write.csv(first_report, 
          "./data/ncands_first_report_index.csv",
          row.names = F)

write.csv(first_victim, 
          "./data/ncands_first_victim_index.csv",
          row.names = F)