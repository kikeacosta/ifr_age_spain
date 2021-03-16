# Description:
source("Code/00_functions.R")

# Global seroprevalence data from 4 rounds of the national representative study. 
# Values obtained from Table 9 in
# https://www.mscbs.gob.es/gabinetePrensa/notaPrensa/pdf/15.12151220163348113.pdf
# Date of analysis 29 Nov 2020
db_ser <- read_xlsx("Data/spain_global_prevalence_rounds_1-4.xlsx")

# Seroprevalence data in tidy format by sex
db_ser2 <- 
  db_ser %>% 
  rename(Age = Edad,
         Sex = Sexo,
         Central = central,
         Lower = inferior,
         Upper = superior) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = recode(Age,
                      "0-" = "0",
                      "5-" = "5"),
         Sex = recode(Sex,
                      "Total" = "t",
                      "Hombres" = "m",
                      "Mujeres" = "f")) %>% 
  filter(Age != "To") %>% 
  select(-cantidad) %>% 
  gather(-Age, - Sex, key = "Estimate", value = "IR") %>% 
  mutate(IR = IR / 100,
         Age = as.integer(Age)) %>% 
  replace_na(list(Age = 90))

# reading excess mortality data until the date of seroprevalence study
db_deaths <- read_rds("Output/spain_deaths_confirmed_and_excess.rds")

db_deaths2 <- db_deaths %>% 
  select(Sex, Age, Deaths)

# exposures
db_pop <- read_rds("Output/weekly_population_estimates_spain.rds")

db_pop2 <- db_pop %>% 
  filter(Year == 2020,
         Week == 49) %>% 
  select(-t, -Year, -Week)
  

db_ifr_age <- db_ser2 %>% 
  left_join(db_pop2) %>% 
  mutate(Infections = IR * Pop) %>% 
  left_join(db_deaths2) %>% 
  mutate(IFR = Deaths / Infections) %>% 
  select(Sex, Age, Estimate, IFR)

# plotting sex- and age-specific IFR in Spain by sex
db_ifr_age %>% 
  filter(Estimate == "Central") %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Sex))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/ifr_age_sex.png")

write_csv(db_ifr_age,  path = "Output/spain_sex_age_ifr.csv")


db_ifr_age %>%
  spread(Estimate, IFR) %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, Central, col = Sex))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper, fill = Sex), alpha = 0.3)+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  scale_fill_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/ifr_age_sex_pred_ints.png")


# Overall IFR in Spain
# ~~~~~~~~~~~~~~~~~~~~
overall_ifr_spain <- db_ser2 %>% 
  left_join(db_pop2) %>% 
  mutate(Infections = IR * Pop) %>% 
  left_join(db_deaths2) %>% 
  mutate(IFR = Deaths / Infections) %>%
  group_by(Sex, Estimate) %>% 
  summarise(Infections = sum(Infections),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Overall_IFR = Deaths / Infections) %>% 
  select(Sex, Estimate, Overall_IFR) %>% 
  spread(Estimate, Overall_IFR)
  
overall_ifr_spain

overall_ifr_spain %>% 
  ggplot()+
  geom_point(aes(Sex, Central), size = 3)+
  geom_errorbar(aes(Sex, ymin=Upper, ymax=Lower), width=.5)+
  scale_y_continuous(limits = c(0, 0.03))+
  # coord_flip()+
  theme_bw()
    
ggsave("Figures/Overall_ifr_spain_by_sex.png", width = 5, height = 3)

# plotting sex- and age-specific IFR in Spain with confidence intervals
db_ifr_age %>% 
  spread(Estimate, IFR) %>% 
  filter(Sex == "t") %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, Central))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper), alpha = 0.5)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()
ggsave("Figures/ifr_age_confidence_intervals.png")


# comparing with Verity values
##############################

# reading Verity et al. IFR estimates
verity <- read_delim("Data/IFR-Verity.txt", delim = "\t", col_names = F) %>% 
  rename(Age = 1,
         IFR = 2,
         IFR_l = 3,
         IFR_u = 4) %>% 
  gather(-Age, key = "Estimate", value = "IFR") %>% 
  mutate(Estimate = recode(Estimate,
                           "IFR" = "Central", 
                           "IFR_l" = "Lower",
                           "IFR_u" = "Upper"),
         Source = "Verity et al.",
         Sex = "t",
         Age_int = 10)

# appending Verity et al. and Spain estimates
db_ifrs <- db_ifr_age %>%
  mutate(Source = "Spain",
         Age = as.integer(Age),
         Age_int = 5) %>% 
  bind_rows(verity)

# plotting Spain by sex and Verity estimates
db_ifrs %>% 
  filter(Estimate == "Central") %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Sex, linetype = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_bw()

ggsave("Figures/ifr_age_sex_spain_verity.png")

# plotting Spain and Verity estimates with conficence intervals
db_ifrs %>% 
  filter(Sex == "t") %>% 
  spread(Estimate, IFR) %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, Central, col = Source))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#048BA8", "#F18F01"))+
  scale_fill_manual(values = c("#048BA8", "#F18F01"))+
  theme_bw()

ggsave("Figures/ifr_age_conf_int_spain_verity.png")


#########################################################
# Estimating age-specific IFRs, based on other estimates
#########################################################

db_odr <- read_csv("Data/IFR-ODriscoll.csv")
db_ic <- read_csv("Data/IFR_imperial_college.csv")

db_odr2 <- db_odr %>% 
  gather(-Age, key = "group", value = "IFR") %>% 
  separate(group, c("Sex", "trash", "Estimate"), sep = "_") %>% 
  replace_na(list(Estimate = "Central")) %>% 
  mutate(Estimate = recode(Estimate,
                           "u" = "Upper",
                           "l" = "Lower"),
         Source = "ODriscol et al.",
         IFR = IFR / 100,
         Age_int = 5,
         Sex = recode(Sex,
                      "b" = "t")) %>% 
  select(-trash)

db_ic2 <- db_ic %>% 
  select(Age, s_IFR, s_IFR_l, s_IFR_u) %>% 
  rename(Central = s_IFR, 
         Lower = s_IFR_l, 
         Upper = s_IFR_u) %>% 
  gather(-Age, key = "Estimate", value = "IFR") %>% 
  mutate(Source = "Imperial College",
         IFR = IFR / 100,
         Age_int = 5,
         Sex = "t")

db_ifrs2 <- db_ifrs %>% 
  bind_rows(db_odr2, db_ic2)

db_ifrs2 %>% 
  filter(Estimate == "Central",
         Sex == "t") %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source, size = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_size_manual(values = c(0.8, 0.8, 1.5, 0.8))+
  theme_bw()

ggsave("Figures/ifr_age_several_sources.png")

# with confidence intervals
db_ifrs2 %>% 
  filter(Sex == "t") %>% 
  spread(Estimate, IFR) %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, Central, col = Source))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  theme_bw()

write_csv(db_ifrs2, "Output/sex_age_ifr_several_sources.csv")

# Comparing with previous estimates
unique(db_ifr_age$Estimate)
unique(db_ifr_age$Sex)

prev_ifrs <- read_csv("Output/deprecated/spain_sex_age_ifr.csv")

prev_ifrs2 <- 
  prev_ifrs %>% 
  rename(Central = IFR,
         Lower = IFR_l,
         Upper = IFR_u) %>% 
  mutate(Sex = recode(Sex,
                      "b" = "t"),
         Source = "Previous Spain") %>% 
  bind_rows(db_ifr_age %>%
              spread(Estimate, IFR) %>% 
              mutate(Source = "Current Spain"))
  
prev_ifrs2 %>%
  ggplot()+
  geom_line(aes(Age, Central, col = Source))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  facet_grid(~ Sex)+
  theme_bw()

ggsave("Figures/ifr_age_spain_previous_vs_current.png")
