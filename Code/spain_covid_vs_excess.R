library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)

osf_retrieve_file("7tnfh") %>%
  osf_download(path = "Data/",
               conflicts = "overwrite") 

db_covid <-  read_csv("Data/Output_5.zip",
                skip = 3,
                col_types = "ccccciiddd")

db_covid2 <- db_covid %>% 
  filter(Country == "Spain", 
         Region == "All",
         Date == "21.05.2020",
         Sex != "b") %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Sex, Age) %>%
  summarise(Deaths = sum(Deaths)) %>% 
  group_by(Sex) %>% 
  mutate(Deaths_tot = sum(Deaths),
         age_dist = Deaths / Deaths_tot) %>% 
  ungroup() %>% 
  select(Sex, Age, age_dist) %>% 
  mutate(Source = "COVID")

unique(db_covid2$Age)

db_excess <- read_csv("Output/spain_excess_until_seroprev.csv")

db_excess2 <- db_excess %>% 
  filter(Age != "All") %>% 
  group_by(Sex) %>% 
  mutate(Deaths_tot = sum(Excess),
         age_dist = Excess / Deaths_tot) %>% 
  ungroup() %>% 
  select(Sex, Age, age_dist) %>% 
  mutate(Source = "Excess",
         Age = as.integer(Age))

unique(db_excess2$Age)

  
db_age_dist <- bind_rows(db_covid2,
                         db_excess2)

db_age_dist %>% 
  ggplot()+
  geom_line(aes(Age, age_dist, col = Source))+
  facet_grid(~ Sex)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

ggsave("Figures/spain_seroprev/age_distrib_excess_covid.png")

db_covid3 <- db_covid %>% 
  filter(Country == "Spain", 
         Region == "All",
         Date == "21.05.2020",
         Sex != "b") %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Sex, Age) %>%
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "COVerAGE-DB")
  

db_excess3 <- db_excess %>% 
  filter(Age != "All") %>% 
  select(Sex, Age, Deaths) %>% 
  mutate(Source = "Excess",
         Age = as.integer(Age))

db_deaths_age <- bind_rows(db_covid3, db_excess3)

db_deaths_age %>% 
  ggplot()+
  geom_line(aes(Age, Deaths, col = Source))+
  facet_grid(~ Sex)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

ggsave("Figures/spain_seroprev/age_deaths_excess_covid.png")
