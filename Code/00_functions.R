Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
pkgs <- c("tidyverse",
          "lubridate",
          "haven",
          "readxl",
          "osfr")

lapply(pkgs, require, character.only = T)


# Weekly population mortality interpolation 
interpop <- function(db){
  ys <- db %>% drop_na() %>% pull(t)
  ps <- db %>% drop_na() %>% pull(Pop)
  # smoothing using cubic splines
  ws <- db %>% pull(t)
  md2 <- smooth.spline(x = ys, y = ps)
  inter_pop <- tibble(t = ws,
                      Pop2 = predict(md2, ws)$y)
  return(inter_pop)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjustments to STMF inputs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data prep function preamble
dist_unk <- function(chunk){
  if ("UNK" %in% chunk$Age){
    UNK   <- chunk %>% filter(Age == "UNK") %>% pull(Deaths)
    chunk <- chunk %>% filter(Age != "UNK") %>% 
      mutate(Deaths = Deaths + UNK * Deaths / sum(Deaths) )
  }
  chunk
}

dist_tot <- function(chunk){
  if ("TOT" %in% chunk$Age){
    TOT   <- chunk %>% filter(Age == "TOT") %>% pull(Deaths)
    chunk <- chunk %>% filter(! Age %in% c("TOT","UNK")) %>% 
      mutate(Deaths = TOT * Deaths / sum(Deaths) )
  }
  chunk
}

# within Country, Year, Sex, Age
dist_unk_week <- function(chunk){
  if ("UNK" %in% chunk$Week){
    UNK   <- chunk %>% filter(Week == "UNK") %>% pull(Deaths)
    chunk <- chunk %>% filter(Week != "UNK") %>% 
      mutate(Deaths = Deaths + UNK * Deaths / sum(Deaths) )
  }
  chunk
}

scale_sexes <- function(chunk){
  if (setequal(c("m","f","b"),unique(chunk$Sex))){
    cnames <- colnames(chunk)
    chunk <-
      chunk %>% 
      pivot_wider(names_from = Sex,
                  values_from = Deaths) %>% 
      mutate(m = m / (m + f) * t,
             f = f / (m + f) * t,
             m = ifelse(is.nan(m),0,m),
             f = ifelse(is.nan(f),0,f)) %>% 
      pivot_longer(m:b, names_to = "Sex", values_to = "Deaths") %>% 
      select(all_of(cnames))
  }
  chunk
}


# db <- temp2
# ax <- aint / 2

# ungrouping IFRs
ungr_ifrs <- function(db, ax){
  ages <- db %>% pull(Age) + ax
  log_ifrs <- log(db %>% pull(IFR) + 1e-20)
  # smoothing in single-years of age
  new_x <- seq(0, 100)
  md2 <- smooth.spline(x = ages, y = log_ifrs)
  ifrs_ungr <- tibble(Age = new_x,
                      IFR = exp(predict(md2, new_x)$y) - 1e-20)
  return(ifrs_ungr)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### function for bootstrapping 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  return(data.frame(Baseline = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### function for fitting model for each sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ct <- "Spain"
# sx <- "b"
# ag <- 80
# ymin <- 2014


fit_baseline <- function(db2, a) {
  
  skip_to_next <- F
  
  # data to include in the model 
  db_bline <- db2 %>% 
    filter(include == 1)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  # model fitting evaluation
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6 | a >= 50) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # predicting values and 95% confidence intervals
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # bootstrapping
  tryCatch({
    db3 <- cbind(db2, 
                 boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
  return(db3)
}