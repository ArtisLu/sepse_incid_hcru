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

stac <- stac %>% 
  mutate(
    tiesie = tiesie_d1 | tiesie_d2,
    netiesie = netiesie_d1 | netiesie_d2,
    org_trauc = org_trauc_d1 | org_trauc_d2,
    explicit = tiesie,
    implicit = netiesie & org_trauc
  ) %>% 
  mutate(
    date1 = as.Date(date1),
    date2 = as.Date(date2)
  )

# checks
stac %>% 
  count(explicit, tiesie_d1, tiesie_d2)

# explicit tiesie_d1 tiesie_d2      n
# <lgl>    <lgl>     <lgl>      <int>
# 1 FALSE    FALSE     FALSE     599072
# 2 TRUE     FALSE     TRUE        6806
# 3 TRUE     TRUE      FALSE       2161
# 4 TRUE     TRUE      TRUE         372

stac %>% 
  count(implicit, netiesie, org_trauc)

# implicit netiesie org_trauc      n
# <lgl>    <lgl>    <lgl>      <int>
# 1 FALSE    FALSE    FALSE     324828
# 2 FALSE    FALSE    TRUE       33624
# 3 FALSE    TRUE     FALSE     236078
# 4 TRUE     TRUE     TRUE       13881

stac <- stac %>%
  select(file:source_full, tiesie, netiesie, org_trauc, explicit, implicit)

stac %>% count(implicit, explicit)

save(stac, file = "data/proc/stac_sepsis_ind.RData")
