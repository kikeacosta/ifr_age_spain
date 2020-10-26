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

db_excess <- read_csv("Output/spain_baseline_mortality.csv")

# Date in which excess mortality is accounted for
date1 <- "2020-02-24"

# Date in which seroprevalence information was collected, 
# between April XX and May XX. Week 18 is the closest middle point 
cut_week <- 18
  
  
# Delay time between onset and death for each age
# Information obtained from Europe CDC 
# https://covid19-surveillance-report.ecdc.europa.eu/
# Average time delay  between onset and death not available by age in Spain,
# So we use EU/EEA and UK averages

# placing delays at the middle of the age interval
ages <- seq(5, 85, 10)
days <- c(8, 14, 21, 23, 20, 21, 20, 16, 15)

# interpolating to single years of age
md1 <- smooth.spline(x = ages, y = days)

# predicting delay time values by single-year of age  and converting them to integer weeks
# deplacing age -2 to match 5 year age groups
delays <- tibble(Age = seq(-2.5, 97.5, 0.5), dly_days = predict(md1, x = seq(0, 100, 0.5))$y) %>% 
  mutate(dly_weeks = round(dly_days / 7))

# # Observing fit results
# delays %>% 
#   mutate(Age = Age + 2.5,
#          source = "spline") %>%
#   bind_rows(tibble(Age = ages, dly_days = days, source = "ECDC")) %>% 
#   ggplot()+
#   geom_point(aes(Age, dly_days, col = source))+
#   scale_x_continuous(breaks = seq(0, 100, 10))
# 
# delays %>%
#   ggplot()+
#   geom_point(aes(Age, dly_weeks))+
#   scale_x_continuous(breaks = seq(0, 100, 10))

# excess mortality by age since the beginning of the pandemic until the 
# collection of seroprevalence data (Week 18 + delay)
db_excess_age <- db_excess %>% 
  left_join(delays) %>% 
  mutate(last_week = cut_week + dly_weeks) %>%
  filter(date >= date1,
         Week <= last_week, 
         Excess >= 0) %>% 
  mutate(Excess_lp = ifelse(Deaths - up > 0, Deaths - up, 0),
         Excess_up = Deaths - lp,
         Age = as.character(Age)) %>% 
  group_by(Sex, Age) %>% 
  summarise(Exposure = max(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Excess = sum(Excess),
            Excess_lp = sum(Excess_lp),
            Excess_up = sum(Excess_up),
            last_week = max(last_week)) %>% 
  ungroup() %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

# for all ages
db_excess_all <- db_excess_age %>% 
  group_by(Sex) %>% 
  summarise(Exposure = max(Exposure),
            Deaths = sum(Deaths),
            Baseline = sum(Baseline),
            Excess = sum(Excess),
            Excess_lp = sum(Excess_lp),
            Excess_up = sum(Excess_up),
            last_week = max(last_week)) %>% 
  mutate(Age = "All") %>% 
  ungroup()

db_excess_4 <- bind_rows(db_excess_age, db_excess_all) %>% 
  mutate(last_date = as.Date(paste(2020, last_week, 1, sep="-"), "%Y-%U-%u")) %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

# saving excess mortality 
write_csv(db_excess_4,  path = "Output/spain_excess_until_seroprev.csv")

# # Plotting excess mortality
# db_excess_age %>% 
#   select(Sex, Age, Deaths, Baseline) %>% 
#   gather(Deaths, Baseline, key = "Mortality", value = "Value") %>% 
#   ggplot()+
#   geom_point(aes(Age, Value, col = Mortality))
# 
# db_excess_age %>% 
#   select(Sex, Age, Excess) %>% 
#   mutate(Age = as.integer(Age)) %>% 
#   ggplot()+
#   geom_line(aes(Age, Excess, col = Sex))+
#   scale_y_log10()

