# Description:
# Summarize EXCESS deaths since 24th February, 2020, in all countries by sex and age 
source("Code/00_functions.R")

# loading excess deaths for Spain
db_excess <- read_rds("Output/spain_baseline_mortality.rds") %>% 
  as_tibble()

# Date in which excess mortality is accounted for
date1 <- "2020-03-01"


# Date in which seroprevalence information was collected, 
# Week 48 (ending in November 29) 
cut_week <- 48
cut_date <- "2020-11-29"  
  
# Delay time between onset and death for each age
# Information obtained from Europe CDC 
# https://covid19-surveillance-report.ecdc.europa.eu/
# Average time delay  between onset and death not available by age in Spain,
# So we use EU/EEA and UK averages

# placing delays at the middle of the age interval (in days)
ages <- seq(5, 85, 10)
days <- c(8, 14, 21, 23, 20, 21, 20, 16, 15)

# interpolating to single years of age
md1 <- smooth.spline(x = ages, y = days)

# predicting delay time values by single-year of age  and converting them to integer weeks
# deplacing age -2 to match 5 year age groups
delays <- 
  tibble(Age = seq(-2.5, 97.5, 0.5), 
         dly_days = predict(md1, x = seq(0, 100, 0.5))$y) %>% 
  # to weeks
  mutate(dly_weeks = dly_days / 7)

# Observing ECDC and fitted results
delays %>%
  mutate(Age = Age + 2.5,
         source = "spline") %>%
  bind_rows(tibble(Age = ages, dly_days = days, source = "ECDC")) %>%
  ggplot()+
  geom_point(aes(Age, dly_days, col = source))+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

# ggsave("Figures/spain_seroprev/lag_days_age_dist.png")

# excess mortality by age since the beginning of the pandemic until the 
# collection of seroprevalence data (Week 18 + delay)
db_excess_age <- db_excess %>% 
  filter(Year == 2020) %>% 
  left_join(delays) %>% 
  mutate(last_week = cut_week + dly_weeks,
         weight_week = case_when(Week < last_week ~ 1,
                                 Week > last_week & Week == ceiling(last_week) ~ last_week - floor(last_week),
                                 Week > ceiling(last_week) ~ 0),
         epi_week = ifelse(Deaths > up, 1, 0),
         Excess = ifelse(epi_week == 1, Deaths - Baseline, 0)) %>%
  filter(Date >= ymd(date1),
         Week <= ceiling(last_week)) %>% 
  group_by(Sex, Age) %>% 
  summarise(Exposure = sum(Exposure),
            Excess = sum(Excess * weight_week),
            last_week = max(last_week)) %>% 
  ungroup() %>% 
  arrange(Sex, Age)

# for all ages
db_excess_all <- db_excess_age %>% 
  group_by(Sex) %>% 
  summarise(Exposure = sum(Exposure),
            Excess = sum(Excess),
            last_week = max(last_week)) %>% 
  mutate(Age = "All") %>% 
  ungroup()

db_excess_4 <- bind_rows(db_excess_age, db_excess_all) %>% 
  arrange(Sex, Age)

# saving excess mortality 
write_rds(db_excess_age,  path = "Output/spain_excess_until_seroprev.rds")

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

