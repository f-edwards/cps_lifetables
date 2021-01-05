library(tidyverse)
library(maps)
library(usmap)

tables_comb<-read_csv("./vis/st_tables.csv") %>% 
  filter(varname!="Termination") %>% 
  mutate(varname = factor(varname, 
                          levels = c("Investigation",
                                     "Confirmed Maltreatment",
                                     "Foster Care"))) %>% 
  mutate(c_mn = ifelse(
    staterr == "WV" & varname %in% c("Investigation", "Confirmed Maltreatment"),
    NA,
    c_mn),
    c_upr = ifelse(
      staterr == "WV" & varname %in% c("Investigation", "Confirmed Maltreatment"),
      NA,
      c_upr)
  )

## remove states with non-reporting, OR, ND
non_reports<-tables_comb %>% 
  filter(varname=="Investigation" | varname=="Confirmed Maltreatment", 
         race_ethn=="Total") %>% 
  filter(c_mn==0) %>% 
  select(-race_ethn) 

plot_dat<-tables_comb %>% 
  filter(year>=2014) %>% 
  group_by(state, staterr, varname, race_ethn) %>% 
  summarise(c_mn = mean(c_mn)) %>% 
  filter(staterr!="DC")

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

state_dat<-tables_comb %>% 
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

ggplot(tables_comb %>% 
         filter(race_ethn!="Total"),
       aes(x = c_mn, fill = race_ethn, color = race_ethn)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~varname, scales = "free_y") + 
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()) +
  xlab("Risk of event by age 18") +
  theme_bw() +
  labs(y = "Density", fill = "", color = "") +
  theme(legend.position = "bottom") +
  ggsave("./vis/st_race_density.png", width = 7, height = 4) 

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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
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
  coord_fixed() +
  ggsave("./vis/st_ineq_race_fc.png", width =7, height = 4)

### Time series plots

ggplot(tables_comb %>% 
         filter(race_ethn=="Total", varname == "Investigation"), 
       aes(x = year - 2000, y = c_mn,
           ymin = c_lwr, ymax = c_upr)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.4) +
  theme_minimal() + 
  facet_wrap(~staterr) + 
  labs(x = "Year", y = "Lifetime risk") + 
  ggsave("./vis/st_time_inv.png", width = 7, height = 7)

ggplot(tables_comb %>% 
         filter(race_ethn=="Total", varname == "Confirmed Maltreatment"), 
       aes(x = year - 2000, y = c_mn,
           ymin = c_lwr, ymax = c_upr)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.4) +
  theme_minimal() + 
  facet_wrap(~staterr) + 
  labs(x = "Year", y = "Lifetime risk") + 
  ggsave("./vis/st_time_malt.png", width = 7, height = 7)

ggplot(tables_comb %>% 
         filter(race_ethn=="Total", varname == "Foster Care"), 
       aes(x = year - 2000, y = c_mn,
           ymin = c_lwr, ymax = c_upr)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.4) +
  theme_minimal() + 
  facet_wrap(~staterr) + 
  labs(x = "Year", y = "Lifetime risk") + 
  scale_x_continuous(breaks = c(2010, 2016)) + 
  ggsave("./vis/st_time_fc.png", width = 7, height = 7)

### proportional change since 08
plot_08<-tables_comb %>% 
  filter(race_ethn=="Total") %>% 
  filter(!(is.na(c_mn))) %>% 
  group_by(staterr, varname) %>% 
  filter(year == min(year)) %>% 
  select(staterr, varname, c_mn, se_tot) %>% 
  rename(c_mn08 = c_mn, 
         se_tot08=se_tot)

plot_18<-tables_comb %>% 
  filter(race_ethn=="Total") %>% 
  group_by(staterr, varname) %>% 
  filter(year == max(year)) %>% 
  select(staterr, varname, c_mn, se_tot)
  
change<-plot_18 %>% 
  left_join(plot_08) %>% 
  mutate(c_change = c_mn / c_mn08,
         se_change = sqrt((se_tot08/c_mn08)^2 + (se_tot/c_mn)^2)) %>% 
  arrange(varname, c_change) %>% 
  ungroup() %>% 
  mutate(staterr = factor(staterr, levels = staterr[103:153])) %>% 
  mutate(varname = factor(varname, levels = c("Foster Care",
                                              "Investigation", 
                                              "Confirmed Maltreatment")))

ggplot(change, 
       aes(x =staterr, y = c_change)) + 
  geom_point() + 
  coord_flip() + 
  theme_bw() +
  geom_hline(yintercept = 1, lty = 2) + 
  facet_wrap(~varname, ncol = 3) + 
  labs(x = "", y = "Proportional change") +
  ggsave("./vis/case_change.png", width = 7, height = 7)
