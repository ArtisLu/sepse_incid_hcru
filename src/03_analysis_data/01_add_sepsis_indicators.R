# -------------------------------------------------------------------------
# Pievieno hospitalizāciju datiem sepses indikatoru
# -------------------------------------------------------------------------

library(tidyverse)
library(readxl)

rm(list = ls())


# -------------------------------------------------------------------------
# Dati
# -------------------------------------------------------------------------

# visa datubāze
load("data/proc/clean_stac_merged.RData")

# -------------------------------------------------------------------------
# Palīgfaili
# -------------------------------------------------------------------------

# sepses kodi
codes <- list()
# excel_sheets("taskfiles/derived/sepses_kodi_manual.xlsx")
# [1] "READ"           "tiesie"         "netiesie"       "org_darb_trauc"
codes$tiesie <- read_xlsx("taskfiles/derived/sepses_kodi_manual.xlsx", sheet = "tiesie")

codes$netiesie <- read_xlsx("taskfiles/derived/sepses_kodi_manual.xlsx", sheet = "netiesie")

codes$org_trauc <- read_xlsx("taskfiles/derived/sepses_kodi_manual.xlsx", sheet = "org_darb_trauc")

# pievieno tipu
codes$tiesie <- codes$tiesie %>% 
  mutate(tips = "tiesie")

codes$netiesie <- codes$netiesie %>% 
  mutate(tips = "netiesie")

codes$org_trauc <- codes$org_trauc %>% 
  mutate(tips = "org_trauc")

# sagatavo regex
sepsis_codes <- codes %>%
  map(~{select(., kods_atlasei, tips)}) %>% 
  bind_rows() %>% 
  mutate(kods = gsub("\\.", "", kods_atlasei)) %>% 
  group_by(tips) %>% 
  summarise(regex = paste(kods, collapse = "|")) %>% 
  mutate(regex = gsub("\\.", "", regex))


# -------------------------------------------------------------------------
# Atzīmē hospitalizācijas ar atlasītajiem kodiem
# -------------------------------------------------------------------------

# izņem "." no ICD koda
stac <- stac %>% 
  mutate(
    across(contains("diag"), ~gsub("\\.", "", .)) 
  )

# atrod hospitalizācijas ar kodiem
stac <- stac %>% 
  mutate(
    tiesie_d1 = grepl(pattern = sepsis_codes %>% filter(tips == "tiesie") %>% pull(regex), x = diag1),
    tiesie_d2 = grepl(pattern = sepsis_codes %>% filter(tips == "tiesie") %>% pull(regex), x = diag2),
    netiesie_d1 = grepl(pattern = sepsis_codes %>% filter(tips == "netiesie") %>% pull(regex), x = diag1),
    netiesie_d2 = grepl(pattern = sepsis_codes %>% filter(tips == "netiesie") %>% pull(regex), x = diag2),
    org_trauc_d1 = grepl(pattern = sepsis_codes %>% filter(tips == "org_trauc") %>% pull(regex), x = diag1),
    org_trauc_d2 = grepl(pattern = sepsis_codes %>% filter(tips == "org_trauc") %>% pull(regex), x = diag2)
  )
# ~ 20 sek


# klasificē hospitalizācijas
stac <- stac %>% 
  mutate(
    tiesie = tiesie_d1 | tiesie_d2,
    netiesie = netiesie_d1 | netiesie_d2,
    org_trauc = org_trauc_d1 | org_trauc_d2,
    explicit = tiesie,
    implicit = netiesie & org_trauc
  )

# pārbauda tiešos kodus
stac %>% 
  count(explicit, tiesie_d1, tiesie_d2)
# explicit tiesie_d1 tiesie_d2      n
# <lgl>    <lgl>     <lgl>      <int>
# 1 FALSE    FALSE     FALSE     558475
# 2 TRUE     FALSE     TRUE        6577
# 3 TRUE     TRUE      FALSE       2060
# 4 TRUE     TRUE      TRUE         443
# OK

# pārbauda netiešos kodus
stac %>% 
  count(implicit, netiesie, org_trauc)

# implicit netiesie org_trauc      n
# <lgl>    <lgl>    <lgl>      <int>
# 1 FALSE    FALSE    FALSE     290626
# 2 FALSE    FALSE    TRUE       32553
# 3 FALSE    TRUE     FALSE     230355
# 4 TRUE     TRUE     TRUE       14021

# atmet liekās kolonnas
stac <- stac %>%
  select(-any_of(contains(c("_d1", "_d2"))))

stac %>% count(implicit, explicit)

save(stac, file = "data/proc/stac_sepsis_ind.RData")
