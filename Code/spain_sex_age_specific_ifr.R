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

db_exc <- read_csv("Output/spain_excess_until_seroprev.csv")

db_ser <- read_csv("Data/spain_seroprev_round1.csv")

# db_e <- read_rds("Data/OffsetsHMD.rds")

db_deaths <- db_exc %>% 
  select(Sex, Age, Excess, Exposure) %>% 
  rename(Deaths = Excess) %>% 
  filter(Age != "All") %>% 
  mutate(Age = as.integer(Age))

# Reestimation of infection rates grouping age groups <1 and 1-4
db_ser2 <- db_ser %>% 
  gather(-Age, key = "Measure", value = "Val") %>% 
  separate(Measure, c("Measure", "Sex"), sep = "_") %>% 
  spread(Measure, Val) %>% 
  select(Age, Sex, smpl, pstv) %>% 
  mutate(inf = round(smpl * pstv / 100),
         Age = ifelse(Age == "1", "0", Age),
         Sex = ifelse(Sex == "t", "b", Sex)) %>% 
  group_by(Sex, Age) %>% 
  summarise(smpl = sum(smpl),
            inf = sum(inf)) %>% 
  ungroup() %>% 
  mutate(IR = inf / smpl,
         Age = as.integer(Age)) %>% 
  select(Sex, Age, IR) %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

db_ifr_age <- db_deaths %>% 
  left_join(db_ser2) %>% 
  mutate(Infected = Exposure * IR,
         IFR = Deaths / Infected,
         Age = as.character(Age)) %>% 
  select(Sex, Age, Deaths, Exposure, IR, Infected, IFR)

# saving IFRs
db_ifr_age2 <- db_ifr_age %>% 
  select(Sex, Age, IFR)

write_csv(db_ifr_age2,  path = "Output/spain_sex_age_ifr.csv")

# Overall IFR in Spain
db_ifr_age %>%
  group_by(Sex) %>% 
  summarise(Infected = sum(Infected),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Overall_IFR = Deaths / Infected)
  
# plotting sex- and age-specific IFR in Spain
db_ifr_age %>% 
  mutate(Age = as.integer(Age)) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Sex))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

ggsave("Figures/spain_seroprev/ifr_age_sex.png")

# comparing with Verity values
verity <- read_delim("Data/IFR-Verity.txt", delim = "\t", col_names = F) %>% 
  select(1, 2) %>% 
  rename(Age = 1,
         IFR = 2) %>% 
  mutate(Age = Age + 5,
         Source = "Verity et al.")


db_ifrs <- db_ifr_age %>% 
  mutate(Source = ifelse(Sex == "m", "Spain males", "Spain females"),
         Age = as.integer(Age)) %>% 
  select(Age, IFR, Source) %>% 
  bind_rows(verity)

db_ifrs %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/spain_seroprev/ifr_age_sex_spain_verity.png")

db_ifrs %>% 
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


