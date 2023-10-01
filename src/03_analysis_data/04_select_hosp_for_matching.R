# -------------------------------------------------------------------------
# Atlasa index hospitalizācijas sepses un salīdzināmajā grupā
# -------------------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Dati --------------------------------------------------------------------
load("data/03_intermediate/sepsis_hosp_w_comorb.RData")
load("data/03_intermediate/other_hosp_w_comorb.RData")


# Atlase ------------------------------------------------------------------

# apvieno sepses un pārējās hospitalizācijas
all_hosp <- bind_rows(other_hosp, sepsis_hosp)
all_hosp %>% count(implicit, explicit, tiesie, netiesie, org_trauc)

# 1. SOLIS: 1) >= 18 gadi 2) sepse vai infekcija / orgānu disfunkcija 3) 2015-2019 (source population)
source_hosp <- all_hosp %>% 
  filter(vecums >= 18) %>% 
  filter(implicit | explicit | netiesie) %>% # sepsis + infekcijas
  filter(year(date1) >= 2015 & year(date1) <= 2019)

nrow(source_hosp)
# [1] 107323

# 2. SOLIS: sadala sepses un pārējās hospitalizācijās
sepsis_hosp <- source_hosp %>% filter(implicit | explicit)
other_hosp <- source_hosp %>% filter(!(implicit | explicit))

# pārbaude
nrow(sepsis_hosp) + nrow(other_hosp) == nrow(source_hosp)
# [1] TRUE

nrow(sepsis_hosp)
# [1] 14476
nrow(other_hosp)
# [1] 92847

# 3. SOLIS: izslēdz other sepsis hospitalizācijas no pacientiem, kam ir sepses hospitalizācija
other_hosp <- other_hosp %>% 
  filter(!(pid %in% sepsis_hosp$pid))

nrow(other_hosp)
# [1] 85914

# 3. SOLIS: izslēdz hospitalizācijas, kas beidzas ar nāvi

sepsis_hosp <- sepsis_hosp %>%
  filter(izrakst_kust != "33")
nrow(sepsis_hosp)
# [1] 8896

other_hosp <- other_hosp %>%
  filter(izrakst_kust != "33")
nrow(other_hosp)
# [1] 80866

# 4. SOLIS: izslēdz hospitalizācijas > 90 naktīm
# source_hosp %>% count(is.na(date1), is.na(date2))
# `is.na(date1)` `is.na(date2)`      n
#   1 FALSE          FALSE          107323

sepsis_hosp <- sepsis_hosp %>% 
  filter(as.integer(date2 - date1) <= 90)
nrow(sepsis_hosp)
# [1] 8758

other_hosp <- other_hosp %>%
  filter(as.integer(date2 - date1) <= 90)
nrow(other_hosp)
# [1] 80657

# 5. solis: sepses un kontroles hospitalizācijām atstāj tikai pirmo hospitalizāciju 
sepsis_hosp <- sepsis_hosp %>% 
  arrange(date1) %>% 
  distinct(pid, .keep_all = TRUE)

nrow(sepsis_hosp)
# [1] 8015

other_hosp <- other_hosp %>% 
  arrange(date1) %>% 
  distinct(pid, .keep_all = TRUE)

nrow(other_hosp)
# [1] 61674

# 6. solis: vismaz 365 dienas pēc izrakstīšanās datuma
max_date <- as.Date("2020-12-31") - 364 # follow-up periods IEKĻAUJ izrakstīšanās datumu
max_date
# [1] "2020-01-02"

sepsis_hosp <- sepsis_hosp %>% 
  filter(date2 <= max_date)
nrow(sepsis_hosp)
# [1] 7948

other_hosp <- other_hosp %>%
  filter(date2 <= max_date)
nrow(other_hosp)
# [1] 61507

# pārbaude, ka tikai unikāli pid
length(unique(sepsis_hosp$pid)) == nrow(sepsis_hosp)
length(unique(other_hosp$pid)) == nrow(other_hosp)

length(c(unique(sepsis_hosp$pid), unique(other_hosp$pid))) == nrow(sepsis_hosp) + nrow(other_hosp)



# 7. solis: apvieno vienā datu kopā

cohort <- bind_rows(
  sepsis_hosp %>% mutate(cohort = "sepsis"),
  other_hosp %>% mutate(cohort = "control")
)

# Pārbauda baseline un follow-up
cohort %>% 
  mutate(
    baseline = as.numeric(date1 - as.Date("2014-01-01")),
    followup = -as.numeric(date2 - as.Date("2020-12-31"))
  ) %>% 
  select(baseline, followup) %>% 
  summary()

# saglabā visas sepses hospitalizācijas
# -------------------------------------------------------------------------
save(cohort, file = "data/03_intermediate/survivor_cohort_all.RData")
# -------------------------------------------------------------------------
