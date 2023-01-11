### make visuals for state life table paper

library(tidyverse)
library(maps)
library(usmap)

plot_dat<-read_csv("./vis/st_tables_combine.csv") %>% 
  select(-c_upr, -c_lwr, -se_tot) %>% 
  filter(varname!="Termination") %>% 
  mutate(varname = factor(varname, 
                          levels = c("Investigation",
                                     "Confirmed Maltreatment",
                                     "Foster Care"))) %>% 
  mutate(c_mn = ifelse(
    staterr == "WV" & varname %in% c("Investigation", "Confirmed Maltreatment"),
    NA,
    c_mn))

### make risk ratio for event sequencing
### remove wv - numbers don't seem trustworthy
inv_dat<-plot_dat %>% 
  ungroup() %>% 
  filter(varname=="Investigation") %>% 
  rename(inv = c_mn) %>% 
  select(-varname)

malt_dat<-plot_dat %>% 
  ungroup() %>% 
  filter(varname=="Confirmed Maltreatment") %>% 
  rename(malt = c_mn) %>% 
  select(-varname)

fc_dat<-plot_dat %>% 
  ungroup() %>%
  filter(varname=="Foster Care") %>% 
  rename(fc = c_mn)%>% 
  select(-varname)

malt_inv<-inv_dat %>% 
  left_join(malt_dat) %>% 
  mutate(malt_inv = malt/inv)

fc_malt<-malt_dat %>% 
  left_join(fc_dat) %>% 
  mutate(fc_malt = fc/malt)

white_dat<-plot_dat %>% 
  filter(race_ethn=="White")

ineq_dat<-plot_dat %>% 
  filter(race_ethn!="White",
         race_ethn!="Total") %>% 
  left_join(white_dat %>% 
              select(-race_ethn) %>% 
              rename(white_mn = c_mn)) %>% 
  mutate(risk_ratio = c_mn/white_mn)

map_dat<-us_map()

ineq_plot_dat<-ineq_dat %>% 
  left_join(map_dat %>% 
              rename(staterr=abbr)) %>% 
  arrange(order, group)

state_dat<-plot_dat %>% 
  left_join(map_dat %>% 
              rename(staterr=abbr)) %>% 
  arrange(order, group)

malt_inv<-malt_inv %>% 
  left_join(map_dat %>% 
              rename(staterr=abbr)) %>% 
  arrange(order, group)

fc_malt<-fc_malt %>% 
  left_join(map_dat %>% 
              rename(staterr=abbr)) %>% 
  arrange(order, group)

plot_dat<-plot_dat %>% 
  left_join(map_dat %>% 
              rename(staterr=abbr)) %>% 
  arrange(order, group)
# 
# ggplot(plot_dat %>% 
#          filter(race_ethn!="Total"),
#        aes(x = c_mn, fill = race_ethn, color = race_ethn)) + 
#   geom_density(alpha = 0.2) + 
#   facet_wrap(~varname, scales = "free_y") + 
#   theme_bw()+
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.title = element_blank()) +
#   xlab("Risk of event by age 18") +
#   theme_bw() +
#   labs(y = "Density", fill = "", color = "") +
#   theme(legend.position = "bottom") +
#   ggsave("./vis/st_race_density.png", width = 7, height = 4) 

state_dat<-state_dat  %>% 
  mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                            "American Indian/Alaska Native",
                            ifelse(race_ethn == "Asian/PI",
                                   "Asian/Pacific Islander",
                            race_ethn))) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "American Indian/Alaska Native",
                              "Asian/Pacific Islander",
                              "Black",
                              "Hispanic",
                              "White")))

plot_dat<-plot_dat  %>% 
  mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                            "American Indian/Alaska Native",
                            ifelse(race_ethn == "Asian/PI",
                                   "Asian/Pacific Islander",
                            race_ethn))) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "American Indian/Alaska Native",
                              "Asian/Pacific Islander",
                              "Black",
                              "Hispanic",
                              "White")))
ineq_plot_dat<-ineq_plot_dat  %>% 
  mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                            "American Indian/Alaska Native",
                            ifelse(race_ethn == "Asian/PI",
                                   "Asian/Pacific Islander",
                            race_ethn))) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "American Indian/Alaska Native",
                              "Asian/Pacific Islander",
                              "Black",
                              "Hispanic",
                              "White")))

malt_inv<-malt_inv  %>% 
  mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                            "American Indian/Alaska Native",
                            ifelse(race_ethn == "Asian/PI",
                                   "Asian/Pacific Islander",
                            race_ethn))) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "American Indian/Alaska Native",
                              "Asian/Pacific Islander",
                              "Black",
                              "Hispanic",
                              "White")))

fc_malt<-fc_malt  %>% 
  mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                            "American Indian/Alaska Native",
                            ifelse(race_ethn == "Asian/PI",
                                   "Asian/Pacific Islander",
                            race_ethn))) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "American Indian/Alaska Native",
                              "Asian/Pacific Islander",
                              "Black",
                              "Hispanic",
                              "White")))


ggplot(malt_inv,
       aes(x=x, y = y, group = group, fill = malt_inv)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Confirmed Maltreatment / Investigation") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +  
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()

ggsave("./vis/st_malt_inv.png", width = 7, height = 4)

ggplot(fc_malt %>% 
         mutate(fc_malt = ifelse(fc_malt>1, 1, fc_malt)),
       aes(x=x, y = y, group = group, fill = fc_malt)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Foster Care / Confirmed Maltreatment") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +  
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()

ggsave("./vis/st_fc_malt.png", width = 7, height = 4)

ggplot(plot_dat %>% 
         filter(varname=="Investigation"),
       aes(x=x, y = y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Investigation") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()

ggsave("./vis/st_race_investigation.png", width = 7, height = 4)

ggplot(ineq_plot_dat %>% 
         filter(varname=="Investigation"),
       aes(x=x, y = y, group = group, fill = risk_ratio)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Risk Ratio: Investigation") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed() 

ggsave("./vis/st_ineq_investigation.png", width =7, height = 4)


ggplot(plot_dat %>% 
         filter(varname=="Confirmed Maltreatment"),
       aes(x=x, y=y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Confirmed Maltreatment") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed() 

ggsave("./vis/st_race_malt.png", width = 7, height = 4)

ggplot(ineq_plot_dat %>% 
         filter(varname=="Confirmed Maltreatment"),
       aes(x=x, y=y, group = group, fill = risk_ratio)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Risk Ratio: Confirmed Maltreatment") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()

ggsave("./vis/st_ineq_race_malt.png", width = 7, height = 4)

ggplot(plot_dat %>% 
         filter(varname=="Foster Care"),
       aes(x=x, y=y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Foster Care") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()
  
ggsave("./vis/st_race_fc.png", width = 7, height = 4)

ggplot(ineq_plot_dat %>% 
         filter(varname=="Foster Care"),
       aes(x=x, y=y, group = group, fill = risk_ratio)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "Risk Ratio: Foster Care") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  coord_fixed()

ggsave("./vis/st_ineq_race_fc.png", width =7, height = 4)

#### scratch for paper numbers
plot_dat %>% 
  filter(varname=="Investigation",
         race_ethn == "Total") %>% 
  summarise(min = min(c_mn, na.rm=T),
            max = max(c_mn, na.rm=T))

plot_dat %>% filter(race_ethn=="Total",
                    varname == "Investigation",
                    c_mn<0.14) %>% distinct(staterr, c_mn)

### make appendix tables

### appx table 1
max<-plot_dat %>% 
  group_by(race_ethn, varname) %>% 
  mutate(max = max(c_mn, na.rm=T)) %>% 
  filter(c_mn == max) %>% 
  arrange(varname)

min<-plot_dat %>% 
  group_by(race_ethn, varname) %>% 
  mutate(min = min(c_mn, na.rm=T)) %>% 
  filter(c_mn == min) %>% 
  arrange(varname)

plot_dat %>% 
  filter(varname=="Investigation") %>% 
  group_by(race_ethn) %>% 
  summarise(l10 = sum(c_mn<0.1, na.rm=T),
            g50 = sum(c_mn>0.5, na.rm=T))

ineq_plot_dat %>% 
  filter(varname == "Investigation") %>% 
  select(staterr, race_ethn, risk_ratio) %>% 
  distinct() %>% 
  group_by(race_ethn) %>% 
  mutate(max = max(risk_ratio, na.rm=T)) %>% 
  filter(risk_ratio == max)

ineq_plot_dat %>% 
  filter(varname == "Investigation") %>% 
  select(staterr, race_ethn, risk_ratio) %>% 
  distinct() %>% 
  group_by(race_ethn) %>% 
  mutate(min = min(risk_ratio, na.rm=T)) %>% 
  filter(risk_ratio == min)

ineq_plot_dat %>% 
  filter(varname == "Investigation") %>% 
  select(staterr, race_ethn, risk_ratio) %>% 
  distinct() %>% 
  group_by(race_ethn) %>% 
  summarise(g1 = sum(risk_ratio>1, na.rm=T),
            g2 = sum(risk_ratio>2, na.rm=T))

### appx table 2

### abb to name
st<-data.frame(staterr = state.abb, state = state.name)

inv<-plot_dat %>% 
  filter(varname=="Investigation") %>% 
  select(staterr, race_ethn, c_mn) %>% 
  pivot_wider(names_from = race_ethn, values_from = c_mn) %>% 
  left_join(st) %>% 
  write_csv("inv_temp.csv")
