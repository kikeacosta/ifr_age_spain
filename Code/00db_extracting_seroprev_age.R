rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

library(tidyverse)
library(lubridate)
library(pdftools)

# setwd("U:/nextcloud/Projects/COVID_19/COVerAge-DB/espana/Seroprevalencia/")

extract_cases1 <- function(rw){
  v1 <- str_split(pg1[rw], "\\s{1,}")[[1]][j] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v2 <- str_split(pg1[rw], "\\s{1,}")[[1]][j + 1] %>% 
    str_replace_all(c(" " = ""))
  v3 <- str_split(pg1[rw], "\\s{1,}")[[1]][j + 2] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v4 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 3] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v5 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 4] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v6 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 5] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v7 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 6] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v8 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 7] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v9 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 8] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v10 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 9] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v11 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 10] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v12 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 11] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v13 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 12] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v14 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 13] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v15 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 14] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v16 <- str_split(pg1[rw ], "\\s{1,}")[[1]][j + 15] %>% 
    str_replace_all(c(" " = "", "," = "."))
  
  row_i <- tibble(Age = v1, 
                  smpl_t = v2, 
                  pstv_t = v3, 
                  lo_t = v4,
                  up_t = v6,
                  smpl_m = v7, 
                  pstv_m = v8, 
                  lo_m = v9,
                  up_m = v11,
                  smpl_f = v12, 
                  pstv_f = v13, 
                  lo_f = v14,
                  up_f = v16)
  return(row_i)
}


extract_cases2 <- function(rw){
  v1 <- str_split(pg2[rw], "\\s{1,}")[[1]][j] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v2 <- str_split(pg2[rw], "\\s{1,}")[[1]][j + 1] %>% 
    str_replace_all(c(" " = ""))
  v3 <- str_split(pg2[rw], "\\s{1,}")[[1]][j + 2] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v4 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 3] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v5 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 4] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v6 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 5] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v7 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 6] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v8 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 7] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v9 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 8] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v10 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 9] %>% 
    str_replace_all(c(" " = "", "," = "."))
  v11 <- str_split(pg2[rw ], "\\s{1,}")[[1]][j + 10] %>% 
    str_replace_all(c(" " = "", "," = "."))

  row_i <- tibble(Age = v1, 
                  smpl1 = v2, 
                  pstv1 = v3, 
                  lo1 = v4,
                  up1 = v6,
                  smpl2 = v7, 
                  pstv2 = v8, 
                  lo2 = v9,
                  up2 = v11)
  return(row_i)
}

ronda1 <- "Data/ESTUDIO_ENE-COVID19_PRIMERA_RONDA_INFORME_PRELIMINAR.pdf"
ronda2 <- "Data/ESTUDIO_ENE-COVID19_SEGUNDA_RONDA_INFORME_PRELIMINAR.pdf"

txt1 <- pdf_text(ronda1)
txt2 <- pdf_text(ronda2)
pg1 <- capture.output(cat(txt1[grep("Prevalencia de anticuerpos IgG anti SARS-Cov2", txt1)]))
pg2 <- capture.output(cat(txt2[grep("Prevalencia de anticuerpos IgG anti SARS-Cov2", txt2)]))

# table round 1
tb1 <- NULL
j <- 2
for(k in 7:26){
  rw_k <- extract_cases1(k)
  tb1 <- tb1 %>% bind_rows(rw_k)
}

tb1

# table round 2
tb2 <- NULL
j <- 2
for(k in 7:26){
  rw_k <- extract_cases2(k)
  tb2 <- tb2 %>% bind_rows(rw_k)
}

tb2

tb1_2 <- tb1 %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Age = case_when(Age == "<1" ~ "0",
                         Age == "=90" ~ "90",
                         TRUE ~ Age)) %>% 
  select(-trash) 

tb2_2 <- tb2 %>% 
  separate(Age, c("Age", "trash"), sep = "-") %>% 
  mutate(Age = case_when(Age == "<1" ~ "0",
                         Age == "=90" ~ "90",
                         TRUE ~ Age)) %>% 
  select(-trash) 

getwd()
write_csv(tb1_2, "Data_output/spain_seroprev_round1.csv")
write_csv(tb2_2, "Data_output/spain_seroprev_round2.csv")

