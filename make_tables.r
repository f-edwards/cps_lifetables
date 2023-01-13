### make 2014 - 2018 pooled life tables from NCANDS / AFCARS
### corrected for NCANDS pre-2014 cases de-duplication
### using processed data from ndacan_processing

rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(maps)
state_map<-map_data("state")

source("lifetable.r")

pop<-read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "staterr", "state",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

### read in afcars and ncands first event tables
### harmonize names in afcars to names in ncands
afcars<-read_csv("~/Projects/ndacan_processing/data/afcars_first_event_state.csv") %>% 
  filter(year >= 2015, year <= 2019) 

ncands<-read_csv("~/Projects/ndacan_processing/data/ncands_first_event_state.csv")%>% 
  filter(year >= 2015, year <= 2019,
         age<18) %>% 
  mutate(race_ethn = case_when(
    race_ethn ==  "Latinx" ~ "Hispanic",
    race_ethn == "API" ~ "Asian/PI",
    race_ethn == "AIAN" ~ "AI/AN",
    T ~ race_ethn
  ))

### convert state abbrev to fips
data(state.fips)
st_fips<-state.fips %>% 
  select(fips, abb) %>% 
  rename(staterr = abb, state = fips) %>% 
  distinct() %>% 
  bind_rows(data.frame("staterr" = c("AK", "HI"), "state" = c(2, 15)))

# join and harmonize state fips with names

dat<-afcars %>% 
  left_join(st_fips) %>% 
  left_join(ncands %>% 
              left_join(st_fips)) 

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age))

### get state pop
pop_st <- pop %>% 
  filter(age<18, year>=2015, year<=2019) %>% 
  group_by(state, year, staterr, age, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(state = as.numeric(state))


### join to dat
dat<-dat %>% 
  left_join(pop_st) %>% 
  pivot_longer(cols = c(first_entry, first_inv, first_victim, tpr),
               names_to = "varname",
               values_to = "var") %>% 
  group_by(.imp, staterr, state, varname, age, race_ethn) %>% 
  summarise(var = sum(var), pop = sum(pop))

# make total
tab_dat<-dat %>%
  group_by(.imp, staterr, state, varname, age) %>% 
  summarise(var = sum(var), pop = sum(pop)) %>% 
  mutate(race_ethn="Total") %>% 
  ungroup() %>% 
  bind_rows(dat) 

### run life tables by imp, race_ethn, sex
vars<-unique(tab_dat$varname)
race<-unique(tab_dat$race_ethn)
state<-unique(tab_dat$state)
tables_out<-list()

counter<-0
for(h in 1:length(vars)){
  for(i in 1:8){
    for(r in 1:length(state)){
      for(y in 1:length(race)){
          counter<-counter + 1
          
          temp<-tab_dat %>%
            filter(.imp == i)
          
          temp<-temp %>% 
            filter(varname == vars[h])
          
          temp<-temp %>% 
            filter(race_ethn == race[y])
          
          state_temp<-state[r]
          
          temp<-temp %>% 
            filter(state == state_temp)
          
          tables_out[[counter]]<-make_life_table(temp)
        }
      }
    }
  }

tables<-bind_rows(tables_out)


#### FIX SE SO THAT IT's JUST IMPUTATION VARIANCE ESTIMATED

#### combine across imps
tables_within<-tables %>%
  group_by(race_ethn, state, staterr, age, varname) %>%
  summarise(c_mn = mean(c),
            v_within = mean(se)) 

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(race_ethn, state, staterr, age, varname) %>%
  summarise(v_between = 1/7 * sum(c - c_mn)^2)

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(se_tot = sqrt(v_within + (1 + 1/8)*v_between)) %>%
  select(state, staterr, varname, race_ethn, age, c_mn, se_tot)

tables_comb<-tables_comb %>%
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot) %>% 
  filter(age==17) %>% 
  mutate(c_lwr = ifelse(c_lwr<0, 0, c_lwr),
         c_upr = ifelse(c_upr>1, 1, c_upr))

tables_comb<-tables_comb %>% 
  mutate(varname = case_when(
    varname=="first_entry" ~ "Foster Care",
    varname=="first_inv" ~ "Investigation",
    varname=="first_victim" ~ "Confirmed Maltreatment",
    varname=="tpr" ~ "Termination",
    T ~ varname
  )) 

write_csv(tables_comb, "./data/st_tables_combine_tpr_update.csv")