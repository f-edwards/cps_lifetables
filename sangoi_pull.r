### run lots of descriptives
### all drug involved infants by state / year / rptsrc / race
### tpr for drug cases
### maybe life tables?

rm(list=ls()); gc()
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

pop<-read_fwf("./data/us.1990_2018.singleages.adjusted.txt",
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
  filter(age=="00") %>% 
  group_by(state, year, st_fips, race_ethn) %>% 
  summarise(infant_pop = sum(pop)) %>% 
  ungroup() %>% 
  rename(staterr = state, subyr = year)

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
           rptvictim, subyr, 
           cddrug, fcdrug, rptsrc)
}

ncands1<-bind_rows(ncands)

# ### check for states that ever use the codes
# st_tab<-ncands1 %>% 
#   group_by(staterr, subyr) %>% 
#   summarise(child_drug = sum(cddrug==1, na.rm=T))

### write out NJ county data
# ncands_cnty<-ncands1 %>% 
#   filter(subyr>=2010) %>% 
#   filter(staterr %in% c("NJ")) %>% 
#   filter(!(is.na(cddrug)), !(is.na(fcdrug))) %>% 
#   group_by(staterr, rptvictim, subyr, rptfips, chage, rptsrc) %>% 
#   summarise(child_drug = sum(cddrug==1),
#             parent_drug = sum(fcdrug==1),
#             total_report = n()) %>% 
#   filter(chage<3) %>% 
#   write_csv("nj_pull.csv")


ncands_st_rpt<-ncands1 %>% 
  filter(subyr>=2010) %>% 
  filter(chage<1) %>% 
  filter(!(is.na(cddrug)), !(is.na(fcdrug))) %>% 
  group_by(staterr, subyr, rptsrc, race_ethn) %>% 
  summarise(child_drug = sum(cddrug==1) + sum(fcdrug==1),
            total_report = n()) %>% 
  ungroup() %>% 
  mutate(child_drug = ifelse(child_drug==0, NA, child_drug)) %>% 
  mutate(rptsrc = 
           case_when(
             rptsrc==1 ~ "social services personnel",
             rptsrc==2 ~ "medical personnel",
             rptsrc==3 ~ "mental health personnel",
             rptsrc==4 ~ "legal, law enforcement, or criminal justice",
             rptsrc==5 ~ "education personnel",
             rptsrc==6 ~ "child day care provider",
             rptsrc==7 ~ "substitute care provider",
             rptsrc==99 ~ "unknown",
             rptsrc>7 ~ "community, family, or anonymous"
           )) %>% 
  filter(!(is.na(child_drug)))

###make total pop for join
write_csv(ncands_st_rpt, "./data/ncands_st_rpt.csv")
write_csv(pop_st, "./data/infant_pop.csv")



ncands_st<-ncands1 %>% 
  filter(subyr>=2010) %>% 
  filter(chage<1) %>% 
  filter(!(is.na(cddrug)), !(is.na(fcdrug))) %>% 
  group_by(staterr, subyr) %>% 
  summarise(child_drug = sum(cddrug==1) + sum(fcdrug==1),
            total_report = n()) %>% 
  ungroup() %>% 
  mutate(child_drug = ifelse(child_drug==0, NA, child_drug)) %>% 
  filter(!(is.na(child_drug))) %>% 
  left_join(pop_st_total) %>% 
  mutate(reports_per_infant = total_report/infant_pop,
         drug_reports_per_infant = child_drug / infant_pop,
         drug_prop_total = child_drug / total_report)

ggplot(ncands_st,
       aes(x = subyr, y = reports_per_infant * 1000)) + 
  geom_line() + 
  geom_line(aes(y= drug_reports_per_infant * 1000), color = 2)+
  facet_wrap(~staterr)

ggplot(ncands_st,
       aes(x = subyr, y = child_drug/total_report,
           color = rptsrc)) + 
  geom_line() + 
  facet_wrap(~staterr)

afcars_path<-"~/Projects/ndacan_data/afcars/"
afcars_files<-paste(afcars_path,
                    list.files(afcars_path),
                    sep = "")

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


afcars_st<-afcars %>% 
  filter(entered==1) %>% 
  filter(ageatstart<1) %>% 
  filter(!(is.na(dachild)), !(is.na(daparent))) %>% 
  group_by(fy, st,state,  race_ethn) %>% 
  summarise(drug_removals = sum(dachild==1) + sum(daparent==1),
            total_removals = n())

write_csv(afcars_st, "./data/afcars_st_drugs.csv")

ggplot(afcars_st,
       aes(x = fy, y = child_drugs)) + 
  geom_point() + 
  geom_point(aes(y = parent_drugs), color = 2) + 
  facet_wrap(~st) + 
  ggtitle("AFCARS removal reason: Black = child drugs, red = parent drugs")
