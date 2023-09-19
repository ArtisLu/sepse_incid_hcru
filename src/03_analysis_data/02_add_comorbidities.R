# -------------------------------------------------------------------------
# Pievieno visām hospitalizācijām blakusslimības
# -------------------------------------------------------------------------

library(tidyverse)
library(comorbidity)

rm(list = ls())
gc()


# -------------------------------------------------------------------------
# Dati
# -------------------------------------------------------------------------

# visu hospitalizāciju fails
load(file = "data/03_intermediate/stac_sepsis_ind.RData")
stac_merged <- stac

# blakusslimībām
load(file = "data/02_cleaned/clean_stac.RData")
load(file = "data/02_cleaned/clean_ambul_stac.RData")
load(file = "data/02_cleaned/clean_komp_med_stac.RData")


# -------------------------------------------------------------------------
# Apstrāde 
# -------------------------------------------------------------------------

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

# pievieno avotu
stac_bs <- stac_bs %>% 
  mutate(source = "stac")

ambul_bs <- ambul_bs %>% 
  mutate(source = "ambul")

komp_med_bs <- komp_med_bs %>% 
  mutate(source = "komp_med")

# pārveido datumus
unique_datetimes <- bind_rows(
  ambul_bs %>% select(date),
  komp_med_bs %>% select(date)
) %>% 
  distinct() %>% 
  rename(date_time = date) %>% 
  mutate(date = lubridate::as_date(date_time))

# pievieno datumus
ambul_bs <- ambul_bs %>% 
  left_join(unique_datetimes, by = c("date" = "date_time")) %>% 
  mutate(date = date.y) %>% 
  select(-date.y)

komp_med_bs <- komp_med_bs %>% 
  left_join(unique_datetimes, by = c("date" = "date_time")) %>% 
  mutate(date = date.y) %>% 
  select(-date.y)

bs <- bind_rows(stac_bs, ambul_bs, komp_med_bs) %>% 
  select(source, everything())

# atmet diagnožu secību
bs <- bs %>% 
  pivot_longer(diag1:diag2, names_to = "diag_type", values_to = "diag") %>% 
  select(-diag_type) %>% 
  drop_na(diag) %>% 
  mutate(diag = strsplit(diag, ";")) %>% 
  unnest(diag)

# atstāj unikālos ierakstus pēc pid, datuma un diagnozes
bs_unique <- bs %>%
  distinct(pid, date, diag)

# izņem punktu no ICD-10 koda
bs_unique <- bs %>% 
  mutate(diag = gsub("\\.", "", diag))

# sakārto kohortu augošā secībā
stac_merged <- stac_merged %>% 
  arrange(pid, date1)


# -------------------------------------------------------------------------
# Blakusslimības sepses hospitalizācijām
# -------------------------------------------------------------------------
stac_sepsis <- stac_merged %>% 
  filter(implicit | explicit)

pids <- unique(stac_sepsis$pid)

sepsis_comorb <- list()

n <- length(pids)

t1 <- Sys.time()
for (i in 1:n){ # i <- 9479
  # atlasa visas hospitalizācijas
  tmp <- stac_sepsis %>%
    filter(pid == pids[i])
  # atlasa visus diagnožu ierakstus
  tmp_diag <- bs_unique %>% filter(pid == pids[i])
  
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
  sepsis_comorb[[i]] <- out
  cat("\r")
  cat(round(i/n*100, 2), "% done!")
}
t2 <- Sys.time()

t2 - t1

sepsis_comorb <- bind_rows(sepsis_comorb)
sepsis_hosp <- sepsis_comorb

save(sepsis_hosp, file = "data/03_intermediate/sepsis_hosp_w_comorb.RData")

# -------------------------------------------------------------------------
# Blakusslimības pārējām hospitalizācijām
# -------------------------------------------------------------------------
stac_other <- stac_merged %>% 
  filter(!(implicit | explicit))

# pārbaude
# nrow(stac_other) + nrow(stac_sepsis) == nrow(stac_merged)
# [1] TRUE

pids <- unique(stac_other$pid)

other_comorb <- list()

n <- length(pids)

t1 <- Sys.time()
for (i in 1:n){ # i <- 9479
  # atlasa visas hospitalizācijas
  tmp <- stac_other %>%
    filter(pid == pids[i])
  # atlasa visus diagnožu ierakstus
  tmp_diag <- bs_unique %>% filter(pid == pids[i])
  
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
  other_comorb[[i]] <- out
  cat("\r")
  cat(round(i/n*100, 2), "% done!")
}
t2 <- Sys.time()

t2 - t1

other_comorb <- bind_rows(other_comorb)
other_hosp <- other_comorb

save(other_hosp, file = "data/03_intermediate/other_hosp_w_comorb.RData")
