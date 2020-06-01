rm(list=ls())
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

afcars_path<-"~/Projects/ndacan_data/afcars/"
afcars_files<-paste(afcars_path,
                    list.files(afcars_path),
                    sep = "")

afcars_files<-c(afcars_files[grep("17", afcars_files)],
                afcars_files[grep("18", afcars_files)])

afcars<-lapply(afcars_files, fread)

for(i in 1:length(afcars_files)){
  afcars[[i]]<-afcars[[i]]%>%
    rename_all(tolower) %>%
    mutate(stfcid = paste(state, recnumbr, sep = "")) %>%
    mutate(rem1dt = ymd(rem1dt)) %>%
    mutate(race_ethn = 
             ifelse(blkafram==1,
                    "Black",
                    ifelse(amiakn==1, "AI/AN",
                           ifelse(asian==1 | hawaiipi==1,
                                  "Asian/PI",
                                  ifelse(hisorgin==1, "Hispanic",
                                          ifelse(white == 1, "White",
                                                 NA))))))
}

afcars<-bind_rows(afcars)

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
  mutate(state = as.numeric(st_fips)) %>% 
  select(-st_fips)

pop_st<-pop_st %>% 
  group_by(state, year) %>% 
  mutate(pct_pop = pop/sum(pop)) %>% 
  select(-pop) %>% 
  ungroup()

pop_st<-pop_st %>% 
  pivot_wider(id_cols = c(state, year),
              names_from=race_ethn,
              values_from=pct_pop,
              names_prefix = "pct_") %>% 
  select(-pct_White) %>% 
  rename(pct_aian = `pct_AI/AN`,
         pct_api = `pct_Asian/PI`) %>% 
  ungroup() %>% 
  mutate(year = year + 1)

recode_nas<-function(x){ifelse(is.na(x),0,x)}

afcars<-afcars%>%
  rename(year = fy) %>% 
  mutate(age = floor(as.integer(rem1dt - ymd(dob))/365)) %>%
  mutate(age = ifelse(age<0, NA, age)) %>%
  filter(state!=72) %>% 
  filter(age<=18) %>%
  filter(year>=2000) %>%
  select(stfcid, state, fipscode,
         year, age, race_ethn, sex,
         totalrem, entered, istpr,
         curplset, rf1amakn, rf2amakn,
         rf1nhopi, rf2nhopi) %>% 
  mutate_at(vars(stfcid, state, fipscode), as.character) 

afcars<-afcars%>% 
  mutate_at(vars(rf1amakn, rf2amakn, rf1nhopi, rf2nhopi),
            recode_nas)

### join pop data
afcars_pop<-afcars %>% 
  mutate(state = as.numeric(state)) %>% 
  left_join(pop_st)

afcars_imp<-afcars_pop %>%
  mutate(sex = factor(sex),
         race_ethn = factor(race_ethn),
         curplset = factor(curplset),
         stfcid = as.character(stfcid),
         state = factor(state),
         fipscode = as.character(fipscode))

imps<-mice(afcars_imp[sample(1:nrow(afcars_imp), 1000),], m=1, maxit=0)

pred<-imps$predictorMatrix
### turn off ids, foster parent vars
pred[1,]<-0
pred[,1]<-0
pred[3,]<-0
pred[,3]<-0
pred[12:15,]<-0
pred[,12:15]<-0
meth<-imps$method

imps<-parlmice(afcars_imp, 
           m=4,
           predictorMatrix = pred,
           method = meth,
           n.core = 2)

save(imps, file="./data/afcars_imp_17_18.Rdata")

imps_out<-complete(imps,
                   action = "long",
                   include = TRUE)
# ### looks about right
# ggplot(imps_out %>%
#          group_by(.imp, race_ethn) %>%
#          summarise(n = n()/nrow(afcars)),
#        aes(x = n,
#            y = race_ethn,
#            col = .imp)) +
#   geom_point()

write.csv(imps_out, "./data/afcars_imputed_all_cases_with_pop_17_18.csv",
          row.names=FALSE)

#q(save="no")


