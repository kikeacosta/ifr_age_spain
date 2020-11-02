# Description:
# Summarize EXCESS deaths since 24th February, 2020, in all countries by sex and age 
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
rm(list=ls())
pkgs <- c("tidyverse",
          "lubridate",
          "haven",
          "readxl")

lapply(pkgs, require, character.only = T)

# reading seroprevalence data
db_ser <- read_csv("Data/spain_seroprev_round1.csv")
# reading excess mortality data until the date of seroprevalence study
db_exc <- read_csv("Output/spain_excess_until_seroprev.csv")


# Seroprevalence data in tidy format by sex
db_ser2 <- db_ser %>% 
  gather(-Age, key = "Measure", value = "Val") %>% 
  separate(Measure, c("Measure", "Sex"), sep = "_") %>% 
  spread(Measure, Val)

# Reestimation of infection rates and confidence intervals grouping 
# ages "<1" and "1-4" for the age group 0-4
db_ser_sex <- db_ser2 %>% 
  filter(Sex != "t") %>% 
  mutate(inf = round(smpl * pstv / 100),
         inf_l = round(smpl * lo / 100),
         inf_u = round(smpl * up / 100), 
         Age = ifelse(Age == "1", 0, as.integer(Age)),
         Sex = ifelse(Sex == "t", "b", Sex)) %>% 
  group_by(Sex, Age) %>% 
  summarise(smpl = sum(smpl),
            inf = sum(inf),
            inf_l = sum(inf_l),
            inf_u = sum(inf_u)) %>% 
  ungroup()

# reestimating for both sexes
db_ser_tot <- db_ser_sex %>% 
  group_by(Age) %>% 
  summarise(smpl = sum(smpl),
            inf = sum(inf),
            inf_l = sum(inf_l),
            inf_u = sum(inf_u)) %>% 
  ungroup() %>% 
  mutate(Sex = "b") 

# binding by sex and all sex
db_ser3 <- bind_rows(db_ser_sex, db_ser_tot) %>%   
  mutate(IR = inf / smpl,
         IR_l = inf_l / smpl,
         IR_u = inf_u / smpl,
         Age = as.integer(Age)) %>% 
  select(Sex, Age, IR, IR_l, IR_u) %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

# preparing mortality data to merge
db_deaths_sex <- db_exc %>% 
  select(Sex, Age, Excess, Exposure) %>% 
  rename(Deaths = Excess) %>% 
  filter(Age != "All") %>% 
  mutate(Age = as.integer(Age))

# excess mortality for both sexes
db_deaths_tot <- db_deaths_sex %>% 
  group_by(Age) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

db_deaths <- bind_rows(db_deaths_sex, db_deaths_tot)

db_ifr_age <- db_deaths %>% 
  left_join(db_ser3) %>% 
  mutate(Infected = Exposure * IR,
         Infected_l = Exposure * IR_l,
         Infected_u = Exposure * IR_u,
         IFR = Deaths / Infected,
         IFR_l = Deaths / Infected_u,
         IFR_u = Deaths / Infected_l,
         Age = as.character(Age)) %>% 
  select(Sex, Age, Deaths, Exposure, Infected, Infected_l, Infected_u, IFR, IFR_l, IFR_u)

# saving IFRs
db_ifr_age2 <- db_ifr_age %>% 
  select(Sex, Age, IFR, IFR_l, IFR_u)

write_csv(db_ifr_age2,  path = "Output/spain_sex_age_ifr.csv")

# Overall IFR in Spain
db_ifr_age %>%
  group_by(Sex) %>% 
  summarise(Infected = sum(Infected),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Overall_IFR = Deaths / Infected)
  
# plotting sex- and age-specific IFR in Spain by sex
db_ifr_age %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Sex))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/ifr_age_sex.png")

# plotting sex- and age-specific IFR in Spain with confidence intervals
db_ifr_age %>% 
  filter(Sex == "b") %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, IFR))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u), alpha = 0.5)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()
ggsave("Figures/ifr_age_confidence_intervals.png")

# comparing with Verity values
verity <- read_delim("Data/IFR-Verity.txt", delim = "\t", col_names = F) %>% 
  select(1, 2) %>% 
  rename(Age = 1,
         IFR = 2) %>% 
  mutate(Age = Age + 5,
         Source = "Verity et al.",
         Sex = "b")


db_ifrs <- db_ifr_age %>%
  mutate(Source = "Spain",
         Age = as.integer(Age) + 2.5) %>% 
  select(Age, Sex, IFR, Source) %>% 
  bind_rows(verity)

db_ifrs %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Sex, linetype = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/ifr_age_sex_spain_verity.png")

db_ifrs %>% 
  filter(Sex == "b") %>% 
  spread(Source, IFR) %>% 
  drop_na() %>% 
  mutate(f = `Spain females` / `Verity et al.`,
         m = `Spain males` / `Verity et al.`) %>% 
  select(Age, f, m) %>% 
  gather(-Age, key = "Sex", value = "Ratio") %>% 
  ggplot()+
  geom_point(aes(Age, Ratio, col = Sex))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(10, 90, 10))+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

ggsave("Figures/spain_seroprev/ifr_ratios.png")


