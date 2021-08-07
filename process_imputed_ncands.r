#### wrangle imputed ncands into geographic unit time-series for 
#### total and first events
#### last edit 8/5/21ls

library(tidyverse)
library(data.table)
library(mice)

### grab filenames and format
files<-list.files("./imputations")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./imputations/", ncands_imputations, sep = "")
### subset to needed files for processing, this grabs 14-18
ncands_imputations<-ncands_imputations[6:9]

### batch process with mice::complete
load(ncands_imputations[1])
temp<-mice::complete(imps, action = "long", include = F)
ncands<-temp %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  select(-pct_aian, -pct_api, -pct_Black, -pct_Hispanic)


for(i in 2:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = "")) %>% 
    select(-pct_aian, -pct_api, -pct_Black, -pct_Hispanic) 
  ncands<-ncands %>% 
    bind_rows(temp)
}

### join first report index onto imputed
first_inv_index<-read_csv("./data/ncands_first_report_index.csv")

first_inv<-first_inv_index %>% 
  left_join(ncands) %>% 
  filter(!(is.na(.imp)))

### join first victim onto imputed
first_victim_index<-read_csv("./data/ncands_first_victim_index.csv")

first_victim<-first_victim_index %>% 
  left_join(ncands) %>% 
  filter(!(is.na(.imp)))

### Make national, state, county, TS

nat<-first_inv %>% 
  group_by(.imp, age, race_ethn, year) %>% 
  summarise(first_inv = n())

nat<-nat %>% 
  left_join(first_victim %>% 
  group_by(.imp, age, race_ethn, year) %>% 
  summarise(first_victm = n()))

state<-first_inv %>% 
  group_by(.imp, staterr, age, race_ethn, year) %>% 
  summarise(first_inv = n())

state<-state %>% 
  left_join(first_victim %>% 
  group_by(.imp, staterr, age, race_ethn, year) %>% 
  summarise(first_victm = n()))



county<-first_inv %>% 
  group_by(.imp, staterr, rptfips, age, race_ethn, year) %>% 
  summarise(first_inv = n())

county<-county %>% 
  left_join(first_victim %>% 
  group_by(.imp, staterr, rptfips, age, race_ethn, year) %>% 
  summarise(first_victm = n()))

write_csv(nat, "./data/ncands_first_event_national.csv")
write_csv(state, "./data/ncands_first_event_state.csv")
write_csv(county, "./data/ncands_first_event_county.csv")

