rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(maps)
state_map<-map_data("state")

source("lifetable.r")

pop<-read_fwf("./data/us.1990_2017.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "staterr", "state",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

first_fc<-read_csv("./data/state_first_fc.csv") %>% 
  rename(subyr = fy) %>% 
  filter(subyr>=2008, state!=72) %>% 
  tidyr::complete(.imp, state, subyr, age, 
           race_ethn, fill = list(first_entry = 0))

tpr<-read_csv("./data/state_tpr.csv") %>% 
  rename(subyr = fy) %>% 
  filter(subyr>=2008, state!=72) %>% 
  tidyr::complete(.imp, state, subyr, age, 
                  race_ethn, fill = list(tpr = 0))

### finish complete zeroes
first_inv<-read_csv("./data/state_first_inv.csv") %>% 
  filter(subyr>=2008) %>% 
  tidyr::complete(.imp, staterr, subyr, age, 
                  race_ethn, fill = list(first_inv = 0))
first_victim<-read_csv("./data/state_first_victim_out.csv") %>% 
  filter(subyr>=2008) %>% 
  tidyr::complete(.imp, staterr, subyr, age, 
                  race_ethn, fill = list(first_victim = 0))

### convert state abbrev to fips
data(state.fips)
st_fips<-state.fips %>% 
  select(fips, abb) %>% 
  rename(staterr = abb, state = fips) %>% 
  distinct() %>% 
  bind_rows(data.frame("staterr" = c("AK", "HI"), "state" = c(2, 15)))

dat<-first_fc %>% 
  left_join(tpr) %>% 
  left_join(st_fips) %>% 
  left_join(first_inv %>% 
              left_join(st_fips)) %>% 
  left_join(first_victim %>% 
              left_join(st_fips)) %>% 
  rename(year = subyr)

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
  filter(age<=18, year>=2007) %>% 
  group_by(state, year, staterr, age, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(year = year+1) %>% 
  mutate(state = as.numeric(state))


### join to dat
dat<-dat %>% 
  left_join(pop_st) %>% 
  pivot_longer(cols = c(first_entry, first_inv, first_victim, tpr),
               names_to = "varname",
               values_to = "var")

# make total
tab_dat<-dat %>%
  group_by(.imp, year, staterr, state, varname, age) %>% 
  summarise(var = sum(var), pop = sum(pop)) %>% 
  mutate(race_ethn="Total") %>% 
  ungroup() %>% 
  bind_rows(dat) 

library(brms)
library(lme4)

mdat<-tab_dat %>% 
  filter(.imp==1, varname == "first_entry", race_ethn == "Total") %>% 
  mutate(year_c = year - 2008,
         row_n = 1:n()) 

m0<-brm(var ~ 
          factor(age) + factor(staterr) * year_c + 
          offset(log(pop)),
        family = "negbinomial",
        data = mdat)

m0<-brm(var ~ staterr * factor(year) + factor(age) + 
          offset(log(pop)) + 
          (1|row_n),
        data = mdat, 
        family = "poisson")


### run life tables by imp, race_ethn, sex
vars<-unique(tab_dat$varname)
race<-unique(tab_dat$race_ethn)
state<-unique(tab_dat$state)
years<-unique(tab_dat$year)
tables_out<-list()

counter<-0
for(h in 1:length(vars)){
  for(i in 1:8){
    for(r in 1:length(state)){
      for(y in 1:length(race)){
        for(t in 1:length(years)){
          counter<-counter + 1
          
          temp<-tab_dat %>%
            filter(.imp == i)
          
          temp<-temp %>% 
            filter(year==years[t])
          
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
}

tables<-bind_rows(tables_out)


#### FIX SE SO THAT IT's JUST IMPUTATION VARIANCE ESTIMATED

#### combine across imps
tables_within<-tables %>%
  group_by(race_ethn, state, staterr, age, year, varname) %>%
  summarise(c_mn = mean(c),
            v_within = mean(se)) 

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(race_ethn, state, staterr, age, year, varname) %>%
  summarise(v_between = 1/7 * sum(c - c_mn)^2)

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(se_tot = sqrt(v_within + (1 + 1/8)*v_between)) %>%
  select(state, staterr, varname, year, race_ethn, age, c_mn, se_tot)

tables_comb<-tables_comb %>%
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot) %>% 
  filter(age==18) %>% 
  mutate(c_lwr = ifelse(c_lwr<0, 0, c_lwr))

tables_comb<-tables_comb %>% 
  mutate(varname = case_when(
    varname=="first_entry" ~ "Foster Care",
    varname=="first_inv" ~ "Investigation",
    varname=="first_victim" ~ "Confirmed Maltreatment",
    varname=="tpr" ~ "Termination",
    T ~ varname
  )) 

write_csv(tables_comb, "./vis/st_tables.csv")