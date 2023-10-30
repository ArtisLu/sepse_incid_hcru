# -------------------------------------------------------------------------
# Izveido matched kohortas
# -------------------------------------------------------------------------

library(tidyverse)
library(MatchIt)

rm(list = ls())
gc()

# Dati --------------------------------------------------------------------
# kohorta
load(file = "data/03_intermediate/survivor_cohort_all.RData")

# -------------------------------------------------------------------------
# Definē matching mainīgos
# -------------------------------------------------------------------------

# hospitalizācijas gads
cohort <- cohort %>% 
  mutate(hosp_gads = lubridate::year(date1))

# hospitalizācijas ilgums
cohort <- cohort %>% 
  mutate(hosp_ilgums = as.integer(date2 - date1))

# pārveido blakusslimības par faktoriem
cohort <- cohort %>% 
  mutate(across(mi:aids, ~{factor(., levels = c(0, 1))}))

# -------------------------------------------------------------------------
# Matching
# -------------------------------------------------------------------------

# matching mainīgie
xvars <- cohort %>% 
  select(vecums, dzimums, mi:aids, charlson_quan, hosp_gads, hosp_ilgums) %>% 
  colnames()

# matching formula
fmla <- as.formula(paste0("cohort~", paste(xvars, collapse = "+")))

# veic matching
m.out <- cohort %>% 
  mutate(cohort = ifelse(cohort == "sepsis", 1, 0)) %>% 
  matchit(
    fmla,
    data = .,
    method = "nearest", # optimal nestrādā
    distance = "glm", 
    ratio = 3, 
    exact = c("hosp_gads", "hosp_ilgums") 
    # caliper = c("hosp_ilgums" = 2) # exact matching ar +/-2 dienas hospitalizācijas ilgumam
  )

summary(m.out)

# dati ar matching indicatoriem
cohort_matched <- match.data(m.out)

cohort_matched <- cohort_matched %>% 
  mutate(cohort = ifelse(cohort == 1, "sepsis", "control"))

# pārbauda sadalījumu grupās
cohort_matched %>%
  count(cohort) %>% 
  mutate(prop = n/sum(n))

# pārbauda, vai visiem ir 1:3 matching
cohort_matched %>% 
  count(subclass) %>% 
  count(n) %>%
  mutate(prop = nn/sum(nn))
# 76% hospitalizācijām ir 3 match

# pārbauda, vai exact matching nostrādājis pareizi
cohort_matched %>% 
  group_by(subclass) %>% 
  summarise(
    range_gads = max(hosp_gads) - min(hosp_gads),
    range_ilgums = max(hosp_ilgums) - min(hosp_ilgums)
  ) %>% 
  count(range_gads, range_ilgums)

# pārbauda pirms-pēc matching

plot(summary(m.out))

# pirms
cohort %>% 
  tableone::CreateTableOne(vars = xvars, strata = "cohort", 
                             data = ., test = FALSE)
# pēc
cohort_matched %>% 
 tableone::CreateTableOne(vars = xvars, strata = "cohort", 
               data = ., test = FALSE)

save(cohort_matched, file = "data/03_intermediate/survivor_cohort_matched.RData")

