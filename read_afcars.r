rm(list=ls())
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

afcars_path<-"~/Projects/ndacan_data/afcars/"
afcars_files<-paste(afcars_path,
                    list.files(afcars_path),
                    sep = "")

### mod for quality check on DC
afcars_files<-afcars_files[13:19]

afcars<-lapply(afcars_files, read_tsv)

<<<<<<< HEAD
afcars<-afcars%>%
  rename_all(tolower) %>%
  mutate(stfcid = paste(state, recnumbr, sep = ""))
=======
>>>>>>> f7567e4800c498c7b20fce4f102c01f6f6622e68

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

### check DC quality
dc<-afcars %>% 
  filter(st == "DC") %>% 
  mutate(race_ethn = ifelse(is.na(race_ethn), "Missing", race_ethn))
### write out year of submission for alt



afcars_id_xwalk<-afcars %>% 
  select(fy, stfcid, dob, sex) %>% 
  mutate(stfcid = as.character(stfcid)) %>% 
  group_by(stfcid) %>% 
  filter(fy==min(fy)) %>% 
  ungroup() %>% 
  distinct() %>% 
  write_csv("./data/afcars_id_xwalk.csv")

# ### subset to only first entries
# afcars<-afcars%>%
#   select(fy, stfcid, dob, race_ethn, sex,
#          ageatlatrem, rem1dt, totalrem, 
#          entered, state, fipscode) %>%
#   filter(totalrem==1 & entered==1)

recode_nas<-function(x){ifelse(is.na(x),0,x)}

afcars<-afcars%>%
  mutate(year = year(rem1dt),
         month = month(rem1dt)) %>%
  mutate(age = floor(as.integer(rem1dt - ymd(dob))/365)) %>%
  mutate(age = ifelse(age<0, NA, age)) %>%
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

afcars_imp<-afcars %>%
  mutate(sex = factor(sex),
         race_ethn = factor(race_ethn),
         curplset = factor(curplset),
         stfcid = as.character(stfcid),
         state = factor(state),
         fipscode = as.character(fipscode))

imps<-mice(afcars_imp[sample(1:nrow(afcars_imp), 100000),], m=1, maxit=0)

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
           n.core = 4, 
           n.imp.core = 2,
           predictorMatrix = pred,
           method = meth)

imps_out<-mice::complete(imps,
                   action = "long",
                   include = F)

write.csv(imps_out, "./data/afcars_imputed_all_cases.csv",
          row.names=FALSE)

