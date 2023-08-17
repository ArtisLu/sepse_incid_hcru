# -------------------------------------------------------------------------
# Pievieno visām hospitalizācijām blakusslimības
# -------------------------------------------------------------------------

library(tidyverse)
library(comorbidity)

rm(list = ls())
gc()

# Dati --------------------------------------------------------------------
# kohorta
load(file = "data/proc/hosp_cohort_alive.RData")

# blakusslimībām
stac <- readRDS(file = "data/proc/clean_stac.rds")
load(file = "data/proc/clean_ambul_stac.RData")
load(file = "data/proc/clean_komp_med_stac.RData")

# Apstrāde ----------------------------------------------------------------

# atlasa kolonnas
stac_bs <- stac %>% 
  select(pid, date = date1, diag1, diag2)

ambul_bs <- ambul %>% 
  select(
    pid,
    date = sakuma_datums,
    diag1 = pamatdiagnoze_kods,
    diag2 = papild_diagnoze_kods
  )

komp_med_bs <- komp_med %>% 
  select(
    pid, 
    date = datums, 
    diag1 = pamatdiagnoze_kods
  )

rm(ambul)
rm(stac)
rm(komp_med)
gc()

# pārveido datumus
stac_bs <- stac_bs %>% 
  mutate(across(contains("date"), ~lubridate::as_date(.))) %>% 
  mutate(source = "stac")

ambul_bs <- ambul_bs %>% 
  mutate(across(contains("date"), ~lubridate::as_date(.))) %>% 
  mutate(source = "ambul")

komp_med_bs <- komp_med_bs %>% 
  mutate(across(contains("date"), ~lubridate::as_date(.))) %>% 
  mutate(source = "komp_med")

bs <- bind_rows(stac_bs, ambul_bs, komp_med_bs) %>% 
  select(source, everything())

# atmet diagnožu secību
bs <- bs %>% 
  pivot_longer(diag1:diag2, names_to = "diag_type", values_to = "diag") %>% 
  select(-diag_type) %>% 
  drop_na(diag) %>% 
  mutate(diag = strsplit(diag, ";")) %>% 
  unnest(diag)

bs <- bs %>% 
  mutate(diag = gsub("\\.", "", diag))

rm(ambul_bs)
rm(stac_bs)
rm(komp_med_bs)
gc()

# sakārto kohortu augošā secībā
cohort <- cohort %>% 
  arrange(pid, date1)

# paņem 100 pid no katras kohortas
n <- 1000
n_sepsis <- cohort %>% 
  filter(cohort == "sepsis") %>% 
  distinct(pid) %>% 
  nrow()
n_control <- cohort %>% 
  filter(cohort == "control") %>% 
  distinct(pid) %>% 
  nrow()

set.seed(250822)
sample_sepsis_pids <- unique(cohort[cohort$cohort == "sepsis", ]$pid)[sample(1:n_sepsis, n)]
sample_control_pids <- unique(cohort[cohort$cohort == "control", ]$pid)[sample(1:n_control, n)]

sample_sepsis <- cohort %>% 
  filter(pid %in% sample_sepsis_pids)

sample_control <- cohort %>% 
  filter(pid %in% sample_control_pids)

sample <- bind_rows(sample_sepsis, sample_control)

pids <- unique(sample$pid)
pids

cohort_comorb <- list()

n <- length(pids) # sepsis + control

t1 <- Sys.time()
for (i in 1:n){ # i <- 1
  # atlasa visas hospitalizācijas
  tmp <- sample %>%
    filter(pid == pids[i])
  # atlasa visus diagnožu ierakstus
  tmp_diag <- bs %>% filter(pid == pids[i])
  
  tmp_comorb <- list()
  for (j in 1:nrow(tmp)){ # j <- 1
    tmp_comorb[[j]] <- comorbidity(
      x = tmp_diag %>% filter(date <= tmp$date1[j]),
      id = "pid",
      code = "diag",
      map = "charlson_icd10_quan",
      assign0 = TRUE
    )
  }
  
  # aprēķina indeksus
  charlson <- map_int(tmp_comorb, ~as.integer(score(., weights = "charlson", assign0 = TRUE)))
  charlson_quan <- map_int(tmp_comorb, ~as.integer(score(., weights = "quan", assign0 = TRUE)))
  
  # izvade
  out <- bind_cols(tmp, bind_rows(tmp_comorb)[, -1])
  out$charlson <- charlson
  out$charlson_quan <- charlson_quan
  cohort_comorb[[i]] <- out
  cat("\r")
  cat(round(i/n*100, 2), "% done!")
}
t2 <- Sys.time()

t2 - t1

cohort_comorb <- bind_rows(cohort_comorb)

save(cohort_comorb, file = paste0("data/proc/hosp_cohort_all_w_comorb_sample_n=", n, ".RData"))
