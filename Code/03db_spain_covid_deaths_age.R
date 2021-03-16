# Description:
# Summarize EXCESS deaths since 24th February, 2020, in all countries by sex and age 
source("Code/00_functions.R")

delays <- read_rds("Output/delays_onset_death_by_age.rds")
cut_date <- "2020-11-29"  
# # Loading Confirmed deaths data in 5-year age groups directly from 
# # COVerAGE-DB in OSF
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# osf_retrieve_file("7tnfh") %>%
#   osf_download(conflicts = "overwrite",
#                path = "Data") 
# 
# # This reads it in
# db_conf <-  read_csv("Data/Output_5.zip",
#                  skip = 3,
#                  col_types = cols(.default = "c")) %>% 
#   mutate(Date = dmy(Date),
#          Deaths = as.double(Deaths),
#          Age = as.integer(Age)) %>% 
#   filter(!str_detect(Code, "ECDC"),
#          Country == "Spain",
#          Region == "All") %>% 
#   select(-Tests)
# 
# write_rds(db_conf, "Output/spain_confirmed_c19_deaths_coverage-db.rds")

# loading stored deaths in Spain from a filtered sample of COVerAGE-DB
# (Version 24-Feb-2021)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_conf <- read_rds("Output/spain_confirmed_c19_deaths_coverage-db.rds")

db_conf2 <- db_conf %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age),
         Sex = recode(Sex,
                      "b" = "t")) %>% 
  group_by(Date, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  left_join(delays) %>% 
  mutate(dly_days = round(dly_days),
         last_date = ymd(cut_date) + dly_days) %>% 
  filter(Date == last_date) %>% 
  arrange(Sex, Age) %>% 
  rename(Confirmed = Deaths) %>% 
  select(Sex, Age, Confirmed)

# Loading excess deaths by age and sex
db_excess <- read_rds("Output/spain_excess_until_seroprev.rds")

# Merging confirmed and excess deaths
# assigning confirmed deaths in ages < 65 and excess_epi to ages >= 65
db_deaths <- db_excess %>% 
  select(-last_week) %>% 
  left_join(db_conf2)

db_deaths_combined <-  
  db_deaths %>%
  mutate(Deaths = ifelse(Age <= 60, Confirmed, Excess_epi)) %>% 
  select(-Excess_pos, -Excess_all) %>% 
  rename(Excess = Excess_epi)

write_rds(db_deaths_combined, "Output/spain_deaths_confirmed_and_excess.rds")

db_deaths_source <- db_deaths %>% 
  select(-Excess_all) %>% 
  gather(Excess_epi, Excess_pos, Confirmed, key = Source, value = Value) %>% 
  mutate(Source = factor(Source, levels = c("Confirmed", "Excess_epi", "Excess_pos")),
         Mx = Value / Exposure)
  
db_deaths_source %>% 
  ggplot()+
  geom_line(aes(Age, Mx, col = Source, size = Source, alpha = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  scale_size_manual(values = c(1, 1, 1))+
  scale_alpha_manual(values = c(1, 1, 1))+
  facet_grid(~ Sex)+
  theme_bw()

ggsave("Figures/covid_deaths_by_source_age_sex_v1.png")

db_deaths_combined %>% 
  gather(Excess, Confirmed, Deaths, key = Source, value = Value) %>% 
  mutate(Source = factor(Source, levels = c("Confirmed", "Excess", "Deaths")),
         Mx = Value / Exposure) %>% 
  ggplot()+
  geom_line(aes(Age, Mx, col = Source, size = Source, alpha = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 90, 10))+
  scale_size_manual(values = c(1, 1, 3))+
  scale_alpha_manual(values = c(1, 1, 0.5))+
  facet_grid(~ Sex)+
  theme_bw()

ggsave("Figures/covid_deaths_by_source_age_sex_v2.png")

