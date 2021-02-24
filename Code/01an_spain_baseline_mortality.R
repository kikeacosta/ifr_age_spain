###########################################################
# Estimating excess mortality in Spain by 5-year age groups
###########################################################
source("Code/00_functions.R")
pkgs <- c("stats", 
          "splines",
          "MASS",
          "gnm",
          'doParallel', 
          'foreach')

lapply(pkgs, require, character.only = T)
select <- dplyr::select

registerDoParallel(cores = 6)

# if (!dir.exists(here("Data","single_est"))){
#   dir.create(here("Data","single_est"))
# }

###################################################################################
# reading data of weekly mortality in 5-years age groups and exposures from the HMD
###################################################################################

# Downloading Spaniard weekly mortality data from STMF (version 2021-02-20) 
# from OSF
osf_retrieve_file("ejztk") %>%
  osf_download(conflicts = "overwrite",
               path = "Data")
# loading weekly deaths
db_d <- read_csv("Data/ESPstmf.csv")

# reading offsets from HMD (version 2021-02-20)
db_p <- read_table("Data/HMD_population_spain_x1.txt",
                   skip = 1)

# grouping exposures in 5-year age groups and filtering Spain
db_p2 <- db_p %>%
  filter(Year >= 2000) %>% 
  mutate(Age = ifelse(Age == "110+", 110, as.integer(Age))) %>% 
  gather(-Year, -Age, key = "Sex", value = "Pop") %>% 
  mutate(Sex = recode(Sex,
                      "Female" = "f",
                      "Male" = "m",
                      "Total" = "t"),
         Age = floor(Age / 5) * 5,
         Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Year, Sex, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Week = 26)

# weekly interpolation of exposures and extrapolation until 2021
# dataframe with weeks by year between 2000 and 2020
db_w <- expand_grid(Year = 2000:2021, Week = 1:52) %>% 
  bind_rows(tibble(Year = c(2004, 2009, 2015, 2020), Week = 53)) %>% 
  arrange(Year, Week) %>% 
  mutate(t = 1:n())

ages <- unique(db_p2$Age)
inters_pop <- NULL

for(s in c("m", "f", "t")){
  pop_temp1 <- db_p2 %>% 
    filter(Sex == s)
  for(a in ages){
    db_w_temp <- db_w %>% 
      mutate(Sex = s,
             Age = a) %>% 
      left_join(pop_temp1)
    
    db_w_temp2 <- db_w_temp %>% 
      left_join(interpop(db_w_temp)) %>% 
      mutate(Age = a,
             Sex = s)

    inters_pop <- inters_pop %>% 
      bind_rows(db_w_temp2)
    
  }
}
# visual inspection
# ~~~~~~~~~~~~~~~~~

# a <- 0
# s <- "t"
# inters_pop %>% 
#   filter(Age == a,
#          Sex == s) %>% 
#   ggplot()+
#   geom_line(aes(t, Pop2), col = "black")+
#   geom_point(aes(t, Pop), col = "red")
# 
inters_pop2 <-
  inters_pop %>%
  select(-Pop) %>%
  rename(Pop = Pop2)

write_rds(inters_pop2, "Output/weekly_population_estimates_spain.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# adjusting STMF mortality data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# distributing deaths at unknown weeks and ages
db_d2 <- db_d %>% 
  select(-Access, -Type) %>% 
  mutate(Age = ifelse(Age == "Unknown", "UNK", Age),
         Week = ifelse(is.na(Week), "UNK", Week),
         Sex = recode(Sex,
                      "b" = "t")) %>% 
  group_by(Year, Week, Sex) %>% 
  # scale TOT
  do(dist_tot(chunk = .data)) %>% 
  # redistribute UNK Age
  do(dist_unk(chunk = .data)) %>% 
  ungroup() 

# redistribute UNK Week
db_d3 <- db_d2 %>% 
  group_by(Year, Sex, Age) %>% 
  do(dist_unk_week(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(Age),
         Week = as.integer(Week)) %>% 
  arrange(Year, Week, Sex, Age)

# scaling sexes to add total deaths
db_d4 <- db_d3 %>% 
  group_by(PopCode, Year, Week) %>% 
  # scaling sexes to add total deaths
  do(scale_sexes(chunk = .data)) %>% 
  ungroup() %>% 
  select(-PopCode, -Area, -AgeInterval) %>% 
  mutate(Age = ifelse(Age == "90+", 90, as.integer(Age)))
  
# combining deaths and exposures
db_d5 <- db_d4 %>% 
  filter(Year >= 2010) %>% 
  left_join(inters_pop2)

# minimum year to include
# ym <- 2010
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjusting data for baseline estimation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# definition of flu seasons and heat waves
flu_season <- c(seq(1, 14, 1), seq(46, 54, 1))
heat_waves <- seq(27, 35, 1)

# Formating data for baseline estimation
db_de <- db_d5 %>% 
  # estimating exposures in person-weeks and rounding deaths to min 1
  mutate(Exposure = Pop / 52.25,
         Deaths) %>% 
  select(Year, Week, Sex, Age, Deaths, Exposure) %>% 
  arrange(Sex, Age, Year, Week) %>% 
  group_by(Age, Sex) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  # adding sinusoidal terms for seasonality
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding winter and summer weeks, as well as 2009 and COVID-19 pandemics
         include = case_when(!(Week %in% heat_waves | Week %in% flu_season) & 
                               Year < 2020 ~ 1,
                             TRUE ~ 0),
         include = factor(include)) %>% 
  drop_na()


###########################################
# estimating baseline for each sex, and age
###########################################
sxs <- unique(db_de$Sex)
ags <- unique(db_de$Age)
db_blns <- NULL
for (s in sxs) {
  for (a in ags) {
    
    temp <- db_de %>% 
      filter(Sex == s,
             Age == a) %>% 
      select(Year, Week, t, Deaths, Exposure, sn52, cs52, include)
    
    cat(paste(s, a, "\n", sep = "_"))
    
    temp2 <- fit_baseline(temp, a) %>% 
      mutate(Sex = s,
             Age = a,
             Date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7")),
             mx_b = 100000 * Baseline / Exposure,
             mx_b_u = 100000 * up / Exposure,
             mx_b_l = 100000 * lp / Exposure,
             mx_d = 100000 * Deaths / Exposure) 
    
    db_blns <- db_blns %>% 
      bind_rows(temp2)
    
    ## plots of estimates
    ## ~~~~~~~~~~~~~~~~~~
    temp2 %>%
      ggplot()+
      geom_vline(xintercept = ymd("2020-03-01"), col = "#b80c09", alpha = 0.2)+
      geom_line(aes(Date, mx_d), size = 0.4)+
      geom_ribbon(aes(Date, ymin = mx_b_l, ymax = mx_b_u), fill = "#01BAEF", alpha = 0.25)+
      geom_line(aes(Date, mx_b), col = "#01BAEF", alpha = 0.9, size = 0.6)+
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
      labs(title=paste0(s, "_", a))+
      theme_bw()+
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))+
      ggsave(paste0("Figures/baselines/", s, "_", a, ".png"), dpi = 300, width = 6, height = 4)

  }
}

detach(package:MASS)
write_rds(db_blns, "Output/spain_baseline_mortality.rds")
