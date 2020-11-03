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
db_ser_f <- read_csv("Data/spain_seroprev_round_final.csv")
# reading excess mortality data until the date of seroprevalence study
db_exc <- read_csv("Output/spain_excess_until_seroprev.csv")

# Seroprevalence data in tidy format by sex
db_ser2 <- db_ser %>% 
  gather(-Age, key = "Measure", value = "Val") %>% 
  separate(Measure, c("Measure", "Sex"), sep = "_") %>% 
  spread(Measure, Val)


# Rapid test by age and sex
###########################
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

###################################
# preparing mortality data to merge
###################################
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

#############################################################
# Estimating sex- and age-specific IFRs, based on Rapid tests
#############################################################
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
##############################

# reading Verity et al. IFR estimates
verity <- read_delim("Data/IFR-Verity.txt", delim = "\t", col_names = F) %>% 
  rename(Age = 1,
         IFR = 2,
         IFR_l = 3,
         IFR_u = 4) %>% 
  mutate(Age_int = 10,
         Source = "Verity et al.",
         Sex = "b")

# appending Verity et al. and Spain estimates
db_ifrs <- db_ifr_age %>%
  mutate(Source = "Spain",
         Age = as.integer(Age),
         Age_int = 5) %>% 
  select(Age, Age_int, Sex, IFR, IFR_l, IFR_u, Source) %>% 
  bind_rows(verity)

# plotting Spain by sex and Verity estimates
db_ifrs %>% 
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
  filter(Sex == "b") %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#048BA8", "#F18F01"))+
  scale_fill_manual(values = c("#048BA8", "#F18F01"))+
  theme_bw()

ggsave("Figures/ifr_age_conf_int_spain_verity.png")



# db_ifrs %>% 
#   mutate(Age = Age + 0.5 * Age_int) %>% 
#   filter(Sex == "b") %>% 
#   spread(Source, IFR) %>% 
#   drop_na() %>% 
#   mutate(f = `Spain females` / `Verity et al.`,
#          m = `Spain males` / `Verity et al.`) %>% 
#   select(Age, f, m) %>% 
#   gather(-Age, key = "Sex", value = "Ratio") %>% 
#   ggplot()+
#   geom_point(aes(Age, Ratio, col = Sex))+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   scale_y_log10()+
#   scale_x_continuous(breaks = seq(10, 90, 10))+
#   scale_color_manual(values = c("red", "black"))+
#   theme_bw()
# 
# ggsave("Figures/spain_seroprev/ifr_ratios.png")

################################
# Max specificity results by age
################################

db_ser_f2 <- db_ser_f %>% 
  mutate(infs_i = round(Sample_imm * Immunoassay / 100),
         infs_i_lb = round(Sample_imm * Immuno_lb / 100),
         infs_i_ub = round(Sample_imm * Immuno_ub / 100),
         infs_b = round(Sample_imm * Both / 100),
         infs_b_lb = round(Sample_imm * Both_lb / 100),
         infs_b_ub = round(Sample_imm * Both_ub / 100),
         infs_e = round(Sample_imm * Either / 100),
         infs_e_lb = round(Sample_imm * Either_lb / 100),
         infs_e_ub = round(Sample_imm * Either_ub / 100),
         Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Age) %>% 
  summarise(Sample_imm = sum(Sample_imm),
            infs_i = sum(infs_i),
            infs_i_lb = sum(infs_i_lb),
            infs_i_ub = sum(infs_i_ub),
            infs_b = sum(infs_b),
            infs_b_lb = sum(infs_b_lb),
            infs_b_ub = sum(infs_b_ub),
            infs_e = sum(infs_e),
            infs_e_lb = sum(infs_e_lb),
            infs_e_ub = sum(infs_e_ub)) %>% 
  ungroup() %>% 
  mutate(Imm = infs_i / Sample_imm,
         Imm_lb = infs_i_lb / Sample_imm,
         Imm_ub = infs_i_ub / Sample_imm,
         Both = infs_b / Sample_imm,
         Both_lb = infs_b_lb / Sample_imm,
         Both_ub = infs_b_ub / Sample_imm,
         Either = infs_e / Sample_imm,
         Either_lb = infs_e_lb / Sample_imm,
         Either_ub = infs_e_ub / Sample_imm)

db_ser_imm <- db_ser_f2 %>% 
  select(Age, Imm, Imm_lb, Imm_ub) %>% 
  rename(IR = Imm,
         IR_l = Imm_lb,
         IR_u = Imm_ub) %>% 
  mutate(Source = "Immunoassay")

db_ser_both <- db_ser_f2 %>% 
  select(Age, Both, Both_lb, Both_ub) %>% 
  rename(IR = Both,
         IR_l = Both_lb,
         IR_u = Both_ub) %>% 
  mutate(Source = "Both")

db_ser_either <- db_ser_f2 %>% 
  select(Age, Either, Either_lb, Either_ub) %>% 
  rename(IR = Either,
         IR_l = Either_lb,
         IR_u = Either_ub) %>% 
  mutate(Source = "Either")

db_ser4 <- db_ser3 %>% 
  filter(Sex == "b") %>% 
  select(Age, IR, IR_l, IR_u) %>% 
  mutate(Source = "Rapid")

db_ser_f3 <- bind_rows(db_ser4,
                       db_ser_imm,
                       db_ser_both, 
                       db_ser_either)

db_ser_f3 %>% 
  ggplot()+
  geom_line(aes(Age, IR, col = Source))+
  geom_ribbon(aes(Age, ymin = IR_l, ymax = IR_u, fill = Source), alpha = 0.5)



#########################################################
# Estimating age-specific IFRs, based on other estimates
#########################################################

db_ifr_age_oth <- db_deaths %>% 
  filter(Sex == "b") %>% 
  left_join(db_ser_f3) %>% 
  mutate(Infected = Exposure * IR,
         Infected_l = Exposure * IR_l,
         Infected_u = Exposure * IR_u,
         IFR = Deaths / Infected,
         IFR_l = Deaths / Infected_u,
         IFR_u = Deaths / Infected_l,
         Age = as.character(Age)) %>% 
  select(Age, Deaths, Exposure, Infected, Infected_l, Infected_u, IFR, IFR_l, IFR_u, Source)

db_ifr_age_oth %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  # geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue", "green"))+
  scale_fill_manual(values = c("red", "black", "blue", "green"))+
  theme_bw()
ggsave("Figures/ifr_age_several_spain.png")



# either and both
db_ifr_age_oth %>% 
  filter(Source %in% c("Either", "Both")) %>% 
  mutate(Age = as.integer(Age) + 2.5) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("red", "black", "blue", "green"))+
  scale_fill_manual(values = c("red", "black", "blue", "green"))+
  theme_bw()

ggsave("Figures/ifr_age_both_either_spain.png")

# and verity?
#############

db_ifr_age_oth2 <- verity %>% 
  select(-Sex) %>%
  mutate(Age_int = 10) %>% 
  bind_rows(db_ifr_age_oth %>% 
              select(Age, IFR, IFR_l, IFR_u, Source) %>% 
              mutate(Age = as.integer(Age),
                     Age_int = 5))

db_ifr_age_oth2 %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  theme_bw()


# mix of specificity and sensitivity

db_ifr_age_oth3 <- db_ifr_age_oth2 %>% 
  filter(Source == "Both" | Source == "Either") %>% 
  group_by(Age) %>% 
  summarise(IFR = mean(IFR),
            IFR_l = min(IFR_l),
            IFR_u = max(IFR_u)) %>% 
  ungroup() %>% 
  mutate(Source = "Spain_max",
         Age_int = 5) %>% 
  bind_rows(verity %>% 
              select(-Sex))

db_ifr_age_oth3 %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  theme_bw()

ggsave("Figures/ifr_age_max_spain.png")


db_odr <- read_csv("Data/IFR-ODriscoll.csv")
db_ic <- read_csv("Data/IFR_imperial_college.csv")

db_odr2 <- db_odr %>% 
  select(Age, b_IFR, b_IFR_l, b_IFR_u) %>% 
  rename(IFR = b_IFR, 
         IFR_l = b_IFR_l, 
         IFR_u = b_IFR_u) %>% 
  mutate(Source = "ODriscol et al.",
         IFR = IFR / 100,
         IFR_l = IFR_l / 100,
         IFR_u = IFR_u / 100,
         Age_int = 5)

db_ic2 <- db_ic %>% 
  select(Age, s_IFR, s_IFR_l, s_IFR_u) %>% 
  rename(IFR = s_IFR, 
         IFR_l = s_IFR_l, 
         IFR_u = s_IFR_u) %>% 
  mutate(Source = "Imperial College",
         IFR = IFR / 100,
         IFR_l = IFR_l / 100,
         IFR_u = IFR_u / 100,
         Age_int = 5)


db_ifr_age_oth4 <- db_ifr_age_oth3 %>% 
  bind_rows(db_odr2, db_ic2)

db_ifr_age_oth4 %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  theme_bw()

ggsave("Figures/ifr_age_several_sources.png")

# with confidence intervals
db_ifr_age_oth4 %>% 
  mutate(Age = Age + 0.5 * Age_int) %>% 
  ggplot()+
  geom_line(aes(Age, IFR, col = Source))+
  geom_ribbon(aes(Age, ymin = IFR_l, ymax = IFR_u, fill = Source), alpha = 0.3)+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))+
  theme_bw()
