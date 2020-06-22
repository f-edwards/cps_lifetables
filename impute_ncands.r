rm(list=ls()); gc()
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")
### set up for 17, 18 imputation
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[1:17]

ncands<-lapply(ncands_files, fread)
### set up to grab rptyear variable, bind to rptdt//chid for index join
for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    mutate(rptdt = ymd(rptdt)) %>%
    mutate(race_ethn =
             ifelse(chracbl==1,
                    "Black",
                    ifelse(chracai==1, "AI/AN",
                           ifelse(chracas==1 | chracnh==1,
                                  "Asian/PI",
                                  ifelse(cethn==1, "Hispanic",
                                         ifelse(chracwh == 1, "White",
                                                NA)))))) %>%
    select(chid, staterr,
           rptfips, rptdt,
           chage, race_ethn,
           chprior,
           rptvictim, subyr)
  }

ncands<-bind_rows(ncands)
ncands<-ncands %>%
  mutate(year = year(rptdt))

ncands_index<-ncands %>% 
  select(chid, staterr, rptdt, subyr, chprior, rptvictim)

write.csv(ncands_index, "./data/ncands_subyr_xwalk.csv",
          row.names = F)

## recode missings
ncands <- ncands %>% 
  rename(age = chage) %>% 
  mutate(age = ifelse(age>18, 
                      NA,
                      age))

pop<-read_fwf("./data/us.1990_2017.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) 

pop_st <- pop %>% 
  filter(age<=18) %>% 
  group_by(state, year, st_fips, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  rename(staterr = state) %>% 
  select(-st_fips)

pop_st<-pop_st %>% 
  group_by(staterr, year) %>% 
  mutate(pct_pop = pop/sum(pop)) %>% 
  select(-pop)

pop_st<-pop_st %>% 
  pivot_wider(id_cols = c(staterr, year),
              names_from=race_ethn,
              values_from=pct_pop,
              names_prefix = "pct_") %>% 
  select(-pct_White)%>% 
  rename(pct_aian = `pct_AI/AN`,
         pct_api = `pct_Asian/PI`) %>% 
  ungroup() %>% 
  mutate(year = year + 1)

ncands_pop<-ncands %>% 
  left_join(pop_st)

ncands_pop<-ncands_pop %>% 
  mutate(staterr = factor(staterr),
         rptfips = as.character(rptfips),
         race_ethn = factor(race_ethn),
         rptvictim = factor(rptvictim)) %>% 
  filter(!staterr%in%c("PR", "XX"))

imps<-mice(ncands_pop[sample(1:nrow(ncands_pop), 1000),], m=1, maxit=0)

pred<-imps$predictorMatrix
### turn off ids, foster parent vars
pred[1,]<-0
pred[,1]<-0
pred[,2]<-0
pred[2,]<-0
pred[3,]<-0
pred[,3]<-0
pred[4,]<-0
pred[,4]<-0

meth<-imps$method

rm(ncands); rm(pop); gc()

years<-unique(ncands_pop$year)

for(i in 17:18){
  imp_dat<-ncands_pop %>% 
    filter(year == years[i])
  imps<-parlmice(imp_dat, 
                 n.imp.core=4,
                 predictorMatrix = pred,
                 method = meth,
                 n.core = 2)
  filename<-paste("ncands_imps", i, ".RData", sep="")
  save(imps, file = filename)
}


# 
# write.csv(imps_out, "./data/ncands_imputed_with_pop.csv",
#           row.names=FALSE)

# q(save="no")
