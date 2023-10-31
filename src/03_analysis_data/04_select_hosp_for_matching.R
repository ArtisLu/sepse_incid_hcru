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

# vecums
source_hosp <- all_hosp %>% 
  filter(vecums >= 18)
nrow(source_hosp)

# sepsis + infekcijas
source_hosp <- source_hosp %>% 
  filter(implicit | explicit | netiesie)
nrow(source_hosp)

# gadi
source_hosp <- source_hosp %>% 
  filter(year(date1) >= 2015)

nrow(source_hosp)
# sepsis skaits būs vienāds ar incidences datu kopas skaitu
source_hosp %>% count(sepsis = implicit | explicit)

# 2. SOLIS: sadala sepses un pārējās hospitalizācijās
sepsis_hosp <- source_hosp %>% filter(implicit | explicit)
other_hosp <- source_hosp %>% filter(!(implicit | explicit))

# pārbaude
nrow(sepsis_hosp) + nrow(other_hosp) == nrow(source_hosp)
# [1] TRUE

# Attrition:
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 17837
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 104874

# 3. SOLIS: izslēdz hospitalizācijas, kam nav 365-day follow-up
sepsis_hosp <- sepsis_hosp %>% 
  filter(year(date1) <= 2019)

other_hosp <- other_hosp %>% 
  filter(year(date1) <= 2019)

# Attrition:
remaining_sepsis - nrow(sepsis_hosp) # excluded
# [1] 3361
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 14476

remaining_other - nrow(other_hosp) # excluded
# [1] 12027
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 92847

# 4. SOLIS: izslēdz other sepsis hospitalizācijas no pacientiem, kam ir sepses hospitalizācija
other_hosp <- other_hosp %>% 
  filter(!(pid %in% sepsis_hosp$pid))

# Attrition:
remaining_other - nrow(other_hosp) # excluded
# [1] 6933
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 85914

# 5. SOLIS: izslēdz hospitalizācijas, kas beidzas ar nāvi

sepsis_hosp <- sepsis_hosp %>%
  filter(izrakst_kust != "33")

other_hosp <- other_hosp %>%
  filter(izrakst_kust != "33")

# Attrition:
remaining_sepsis - nrow(sepsis_hosp) # excluded
# [1] 5580
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 8896
remaining_other - nrow(other_hosp) # excluded
# 5048
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 80866

# 6. SOLIS: izslēdz hospitalizācijas > 90 naktīm
# source_hosp %>% count(is.na(date1), is.na(date2))
# `is.na(date1)` `is.na(date2)`      n
#   1 FALSE          FALSE          122711

sepsis_hosp <- sepsis_hosp %>% 
  filter(as.integer(date2 - date1) <= 90)

other_hosp <- other_hosp %>%
  filter(as.integer(date2 - date1) <= 90)

# Attrition:
remaining_sepsis - nrow(sepsis_hosp) # excluded
# [1] 138
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 8758

remaining_other - nrow(other_hosp) # excluded
# [1] 209
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 80657

# 6. solis: sepses un kontroles hospitalizācijām atstāj tikai pirmo hospitalizāciju 
sepsis_hosp <- sepsis_hosp %>% 
  arrange(date1) %>% 
  distinct(pid, .keep_all = TRUE)

other_hosp <- other_hosp %>% 
  arrange(date1) %>% 
  distinct(pid, .keep_all = TRUE)

# Attrition:
remaining_sepsis - nrow(sepsis_hosp) # excluded
# [1] 743
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 8015

remaining_other - nrow(other_hosp) # excluded
# [1] 18983
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 61674

# 7. solis: vismaz 365 dienas pēc izrakstīšanās datuma
max_date <- as.Date("2020-12-31") - 364 # follow-up periods IEKĻAUJ izrakstīšanās datumu
# [1] "2020-01-02"

sepsis_hosp <- sepsis_hosp %>% 
  filter(date2 <= max_date)

other_hosp <- other_hosp %>%
  filter(date2 <= max_date)

# Attrition:
remaining_sepsis - nrow(sepsis_hosp) # excluded
# [1] 67
remaining_sepsis <- nrow(sepsis_hosp)
remaining_sepsis
# [1] 7948

remaining_other - nrow(other_hosp) # excluded
# [1] 167
remaining_other <- nrow(other_hosp)
remaining_other
# [1] 61507

# pārbaude, ka tikai unikāli pid
length(unique(sepsis_hosp$pid)) == nrow(sepsis_hosp)
length(unique(other_hosp$pid)) == nrow(other_hosp)

length(c(unique(sepsis_hosp$pid), unique(other_hosp$pid))) == nrow(sepsis_hosp) + nrow(other_hosp)

# check attrition
104874 - (12027 + 6933 + 5048 + 209 + 18983 + 167)
17837 - (3361 + 5580 + 138 + 743 + 67)
# OK

# 8. solis: apvieno vienā datu kopā

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
