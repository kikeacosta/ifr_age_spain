# Description:
source("Code/00_functions.R")

# interpolating Spaniard IFRs in single-years age

# IFRs by age from Spain
ifrs <- read_csv("Output/spain_sex_age_ifr.csv") 

# sr <- "Spain"
# s <- "t"
# e <- "Central"

# ax considered as half of the age interval, which is 5 for Spain IFRs
ax <- 2.5


# srs <- unique(ifrs$Source)
db_ungr_ifrs <- NULL
sx <- unique(ifrs$Sex)
es <- unique(ifrs$Estimate)
for(s in sx){
  for(e in es){
    temp1 <- ifrs %>% 
      filter(Sex == s,
             Estimate == e) 
    temp2 <- ungr_ifrs(temp1, ax) %>% 
      mutate(Sex = s,
             Estimate = e)
    db_ungr_ifrs <- db_ungr_ifrs %>% 
      bind_rows(temp2)
  }
}


db_ungr_ifrs2 <- db_ungr_ifrs %>% 
  mutate(Type = "Ungrouped")

ifrs2 <- ifrs %>% 
  mutate(Type = "Grouped",
         Age = Age + 2.5) %>% 
  bind_rows(db_ungr_ifrs2)

s <- "t"
e <- "Lower"
ifrs2 %>% 
  filter(Sex == s,
         Estimate == e) %>% 
  ggplot()+
  geom_point(aes(Age, IFR, col = Type))+
  scale_y_log10()

ifrs2 %>% 
  filter(Type == "Ungrouped") %>% 
  spread(Estimate, IFR) %>% 
  ggplot()+
  geom_line(aes(Age, Central, col = Sex))+
  geom_ribbon(aes(Age, ymin = Lower, ymax = Upper, fill = Sex), alpha = 0.3)+
  scale_x_continuous(breaks = seq(0, 100, 10))+
  scale_y_log10()+
  scale_color_manual(values = c("red", "black", "blue"))+
  scale_fill_manual(values = c("red", "black", "blue"))+
  theme_bw()+
  labs(y = "IFR")

ggsave("Figures/ifr_single_age_sex_confidence_intervals.png")


write_csv(db_ungr_ifrs2,  "Output/spain_sex_age_ifr_single_years.csv")
