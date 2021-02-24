# Description:
# Summarize EXCESS deaths since 24th February, 2020, in all countries by sex and age 
source("Code/00_functions.R")


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
# assigning max deaths to each age and sex combination
db_deaths <- db_excess %>% 
  select(-last_week) %>% 
  left_join(db_conf2) %>% 
  mutate(Deaths = ifelse(Excess > Confirmed, Excess, Confirmed))

write_rds(db_deaths, "Output/spain_deaths_confirmed_and_excess.rds")
