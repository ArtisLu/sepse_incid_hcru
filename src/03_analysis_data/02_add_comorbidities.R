# -------------------------------------------------------------------------
# Pievieno visām hospitalizācijām blakusslimības
# -------------------------------------------------------------------------

library(tidyverse)
library(comorbidity)

rm(list = ls())
gc()

# Dati --------------------------------------------------------------------
# visu hospitalizāciju fails
load(file = "data/03_intermediate/stac_sepsis_ind.RData")
stac_merged <- stac

# blakusslimībām
load(file = "data/02_cleaned/clean_stac.RData")
load(file = "data/02_cleaned/clean_ambul_stac.RData")
load(file = "data/02_cleaned/clean_komp_med_stac.RData")

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

# sakārto kohortu augošā secībā
cohort <- cohort %>% 
  arrange(pid, date1)

pids <- unique(cohort$pid)

cohort_comorb <- list()

n <- length(pids)

t1 <- Sys.time()
for (i in 1:n){ # i <- 9479
  # atlasa visas hospitalizācijas
  tmp <- cohort %>%
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

cohort_comorb %>% View()
save(cohort_comorb, file = "data/proc/hosp_cohort_alive_w_comorb.RData")
