# -------------------------------------------------------------------------
# Atlasa index hospitalizācijas sepses un salīdzināmajā grupā
# -------------------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Dati --------------------------------------------------------------------
load("data/03_intermediate/stac_sepsis_ind.RData")

# Apstrāde ----------------------------------------------------------------

# atlasa 1. sepses hospitalizāciju
tmp1 <- stac %>%
  filter(implicit | explicit) %>% 
  arrange(date1) %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  mutate(cohort = "sepsis")

# distinct(tmp1, eid)

# atlasa hospitalizācijas pacientiem, kam nav bijis sepsis
tmp2 <- stac %>% 
  filter(!(pid %in% tmp1$pid)) %>% 
  mutate(cohort = "control")

# distinct(tmp2, eid)

cohort <- bind_rows(tmp1, tmp2)

# distinct(cohort, eid)

# nomaina kolonnu nosaukumus
cohort <- cohort %>% 
  rename(vecums = vecums_hospitalizacijas_bridi) %>% 
  rename(izrakst_kods = stac_kustiba_izrakstisanas_kustiba)

# Iekļaušanas/Izslēgšanas kritēriji ---------------------------------------

cohort <- cohort %>% 
  mutate(vecums = as.integer(vecums))

# trūkstoši dati kritērijiem

cohort %>% 
  count(vecums = is.na(vecums))

cohort %>% 
  count(izrakst_kods = is.na(izrakst_kods))

cohort %>% 
  count(
    iestasanas_dat = is.na(date1),
    izrakst_dat = is.na(date2)
  )

# nrow(cohort)
# [1] 543540

# Nav zināms vecums 
cohort <- cohort %>% 
  drop_na(vecums)
# nrow(cohort)
# [1] 535252

# Vecums >= 18
cohort <- cohort %>% 
  filter(vecums >= 18)
# nrow(cohort)
# [1] 387556

# saglabā visas sepses hospitalizācijas
# -------------------------------------------------------------------------
save(cohort, file = "data/proc/hosp_cohort_all.RData")
# -------------------------------------------------------------------------


# Izrakstīts dzīvs
cohort <- cohort %>%
  filter(izrakst_kods != "33") # Miris
# nrow(cohort)
# [1] 362671

# Vismaz 360 dienas pirms
min_date <- as.Date("2014-01-01") + 359 # baseline period INCLUDE index date

cohort <- cohort %>% 
  filter(date1 >= min_date)
# nrow(cohort)
# [1] 314066

# Vismaz 360 dienas pēc
max_date <- as.Date("2020-12-31") - 360 # follow-up period does NOT INCLUDE dishcharge date

cohort <- cohort %>% 
  filter(date2 <= max_date)
# nrow(cohort)
# [1] 273390

# Pārbauda baseline un follow-up
cohort %>% 
  mutate(
    baseline = as.numeric(date1 - as.Date("2014-01-01")),
    followup = -as.numeric(date2 - as.Date("2020-12-31"))
  ) %>% 
  select(baseline, followup) %>% 
  summary()

# Saglabā -----------------------------------------------------------------
save(cohort ,file = "data/proc/hosp_cohort_alive.RData")
# -------------------------------------------------------------------------


