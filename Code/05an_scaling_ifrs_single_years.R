rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(readxl)
source("Code/00_functions.R")

##############
# reading data
##############

# IFRs by age from Verity et al.
ifrs_ch <- read_csv("Data/IFR_age_Verity_et_al.csv")
# equivalent Canadian thanatological ages compared to China
than_ch <- read_rds("Output/thanat_age_canada_china.rds")

# IFRs by age from Spain
ifrs_es <- read_csv("Data/IFR_sex_age_spain.csv") 
# equivalent Canadian thanatological ages compared to Spain
than_es <- read_rds("Output/thanat_age_canada_spain.rds")

########################
# scaling IFRs to Canada
########################

# From China
############

# interpolating IFRs in single-years age
ungr_ifrs_ch <- ungr_ifrs(ifrs_ch, 5)
  
# # visualizing the splines
# ifrs_ch_vis <- ungr_ifrs_ch %>% mutate(source = "ungr") %>% 
#   bind_rows(ifrs_ch %>% mutate(source = "verity",
#                                Age = Age + 5))
# 
# ifrs_ch_vis %>% 
#   ggplot()+
#   geom_point(aes(Age, IFR, col = source))+
#   scale_x_continuous(breaks = seq(0, 100, 10))+
#   scale_y_log10()

# attributing IFR from China to Canada
ifrs_ca_ch <- than_ch %>% 
  left_join(ungr_ifrs_ch %>% 
              rename(Age_ch = Age)) %>% 
  mutate(Source = "Verity") %>% 
  select(-Age_ch)

# From Spain
############

# interpolating IFRs in single-years age
ifrs_es_m <- ifrs_es %>% 
  filter(Sex == "m")
ungr_ifrs_es_m <- ungr_ifrs(ifrs_es_m, 2.5) %>% 
  mutate(Sex = "m")

ifrs_es_f <- ifrs_es %>% 
  filter(Sex == "f")
ungr_ifrs_es_f <- ungr_ifrs(ifrs_es_f, 2.5) %>% 
  mutate(Sex = "f")

ungr_ifrs_es <- bind_rows(ungr_ifrs_es_m, ungr_ifrs_es_f)

# # visualizing the splines
# ifrs_es_vis <- ungr_ifrs_es %>% mutate(source = "ungr") %>% 
#   bind_rows(bind_rows(ifrs_es_m, ifrs_es_f) %>% 
#               mutate(Age = Age + 2.5,
#                      source = "spain"))
# 
# ifrs_es_vis %>% 
#   ggplot()+
#   geom_point(aes(Age, IFR, col = source))+
#   scale_x_continuous(breaks = seq(0, 100, 10))+
#   scale_y_log10()

# attributing IFR from Spain to Canada
ifrs_ca_es <- than_es %>% 
  left_join(ungr_ifrs_es %>% 
              rename(Age_es = Age)) %>% 
  mutate(Source = "Spain") %>% 
  select(-Age_es)

# binding all IFRs for Canada
ifrs_ca <- bind_rows(ifrs_ca_ch, ifrs_ca_es)

ifrs_ca_adj <- NULL
regs <- unique(ifrs_ca$Region)
for(r in regs){
  temp1 <- ifrs_ca %>% 
    filter(Region == r)
  sxs <- unique(temp1$Sex)
  for(s in sxs){
    temp2 <- temp1 %>% 
      filter(Sex == s)
    ifrs_ca_adj <- ifrs_ca_adj %>% 
    bind_rows(ungr_ifrs(temp2, 0.5) %>% 
                mutate(Region = r,
                       Sex = s))  
  }
}

ifrs_ca_adj2 <- ifrs_ca_adj %>% 
  mutate(Source = case_when(Sex == "b" ~ "Verity et al.", 
                            Sex == "f" ~ "f_Spain seroprev.",
                            Sex == "m" ~ "m_Spain seroprev."))

ifrs_ca_adj2 %>% 
  filter(Source == "Verity et al.") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()

ifrs_ca_adj2 %>% 
  filter(Source == "m_Spain seroprev.") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()

ifrs_ca_adj2 %>% 
  filter(Source == "f_Spain seroprev.") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Region))+
  scale_y_log10()

ifrs_ca_adj2 %>% 
  filter(Region == "All") %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  scale_y_log10()


# ggsave("Figures/age-spe_ifr_canada_china.png")
# 
# ifrs_ca_adj %>% 
#   bind_rows(ifrs_ungr %>% 
#               rename(Age = Age_ch) %>% 
#               mutate(Region = "China")) %>% 
#   filter(Age >= 60) %>% 
#   ggplot()+
#   geom_line(aes(Age, IFR, col = Region))+
#   scale_y_log10()+
#   scale_x_continuous(breaks = seq(0, 90, 10))+
#   labs(title = "Age-specific IFRs by region ages >60")+
#   theme_bw()
# 
# ggsave("Figures/age-spe_ifr_60plus.png")
# 
# ifr_10age_can <- ifrs_ca_adj %>% 
#   filter(Age %in% seq(5, 85, 10)) %>% 
#   mutate(Age = Age -5)

write_rds(ifrs_ca_adj2, "Output/ifr_age_sex_canada.rds")
