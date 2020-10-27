###########################################################
# Estimating excess mortality in Spain by 5-year age groups
###########################################################
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
rm(list=ls())
pkgs <- c("tidyverse",
          "lubridate",
          "haven",
          "readxl", 
          "stats", 
          "splines",
          "MASS",
          "gnm",
          'doParallel', 
          'foreach')

lapply(pkgs, require, character.only = T)
select <- dplyr::select

registerDoParallel(cores = 4)

# if (!dir.exists(here("Data","single_est"))){
#   dir.create(here("Data","single_est"))
# }

###################################################################################
# reading data of weekly mortality in 5-years age groups and exposures from the HMD
###################################################################################

# reading Spaniard weekly mortality data from STMF
db_d <- read_csv("Data/ESPstmf.csv")
# reading offsets from HMD
db_e <- read_rds("Data/OffsetsHMD.rds")

# grouping exposures in 5-year age groups and filtering Spain
unique(db_e$Country)
cts <- ("Spain")

db_e2 <- db_e %>%
  mutate(Age = floor(Age / 5) * 5,
         Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(e = sum(Population)) %>% 
  ungroup() %>% 
  filter(Country %in% cts)

# adjusting format mortality data
db_d2 <- db_d %>% 
  filter(Year >= 2014,
         Age != "TOT" & Age != "UNK",
         Sex != "b") %>% 
  mutate(Age = as.integer(Age)) %>% 
  select(PopCode, Year, Week, Sex, Age, Deaths)

# combining deaths and exposures
# increasing deaths in 1 to guarantee no 0s
db <- db_d2 %>% 
  left_join(db_e2) %>% 
  mutate(Deaths = Deaths + 1)


# minimum year to include
ym <- 2014
skip_to_next <- F

cts <- unique(db$Country)
sxs <- unique(db$Sex)
ags <- unique(db$Age)

##############################
### function for bootstrapping 
##############################

boot_pi <- function(model, odata, pdata, n, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}

#############################################################
### function for fitting model for each country, sex, and age
#############################################################

# ct <- "Spain"
# sx <- "b"
# ag <- 80
# ymin <- 2014


fit_baseline <- function(ct = c, sx = s, ag = a, ymin = ym) {
  
  cat(paste(ct, sx, ag, "\n", sep = "_"))
  
  skip_to_next <- F
  
  db2 <- db %>% 
    filter(Country == ct,
           Sex == sx,
           Age == ag,
           Year >= ymin,
           Week <= 52) %>% 
    arrange(Year, Week) %>% 
    mutate(Deaths = as.integer(Deaths),
           # adding time trend and seasonal components
           t = row_number(),
           sn52 = sin((2*pi*t)/(52)),
           cs52 = cos((2*pi*t)/(52)),
           # excluding winter (wks 46-14), summer (wks 27-35), 2009 and COVID-19 pandemics
           include = ifelse(((Week >= 15 & Week <= 26) |
                               (Week >= 36 & Week <= 45)) &
                              (Year != 2020 & Year != 2009),
                            1, 0))
  
  # data to include in the model 
  db_bline <- db2 %>% 
    filter(include == 1)
  
  ##########################
  # model fitting evaluation
  ##########################
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(e)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(e)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(e)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(e)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(e)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(e)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(e)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(e)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(e)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(e)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(e)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(e)), 
                  family = poisson, data = db_bline)
    }
  }
  
  ################################################
  # predicting values and 95% confidence intervals
  ################################################
  
  # bootstrapping
  tryCatch({
    db3 <- cbind(db2, 
                 boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
  db4 <- db3 %>% 
    mutate(date = as.Date(paste(Year, Week, 1, sep="-"), "%Y-%U-%u"),
           m_pred = pred / e,
           excess = Deaths - pred,
           m_excess = excess / e,
           exc_reg_pi = ifelse(Deaths > up, 1, 0)) %>% 
    dplyr::select(Country, date, everything())
  
  # write_csv(db4, path = paste0("Data_output/single_ests/", ct, "_", sx, "_", ag, "_baseline.csv"))
  
  # db4 %>%
  #   ggplot()+
  #   geom_line(aes(date, Deaths))+
  #   geom_ribbon(aes(date, ymin = lp, ymax = up), fill = "#2ca25f", alpha = 0.3)+
  #   geom_line(aes(date, pred), col = "#2ca25f", alpha = 0.9)+
  #   labs(title=paste0(ct, "_", sx, "_", ag))+
  #   theme_bw()+
  #   theme(
  #     panel.grid.minor = element_blank(),
  #     plot.title = element_text(size=13),
  #     axis.text.x = element_text(size=10),
  #     axis.text.y = element_text(size=10),
  #     axis.title.x = element_text(size=11),
  #     axis.title.y = element_text(size=11))+
  #   ggsave(paste0("Figures/", ct, "_", sx, "_", ag, ".png"), dpi = 300, width = 6, height = 4)
  
  return(db4)
}

####################################################
# estimating baseline for each country, sex, and age
####################################################
db_all <- NULL
for (c in cts) {
  dbc <- db %>%
    filter(Country == c)
  sxs <- unique(dbc$Sex)
  for (s in sxs) {
    for (a in ags) {
      # cat(paste(c, s, a, "\n", sep = "_"))
      db_temp <- fit_baseline(ct = c, sx = s, ag = a, ymin = ym)
      db_all <- bind_rows(db_all, db_temp)
    }
  }
}

db_all2 <- db_all %>% 
  select(Country, date, Year, Week, Sex, Age, Deaths, pred, e, excess, lp, up) %>% 
  rename(Baseline = pred, 
         Exposure = e, 
         Excess = excess)

detach(package:MASS)

write_csv(db_all2, path = "Output/spain_baseline_mortality.csv")
