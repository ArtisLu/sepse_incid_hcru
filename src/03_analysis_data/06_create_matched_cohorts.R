# -------------------------------------------------------------------------
# Izveido matched kohortas
# -------------------------------------------------------------------------

library(tidyverse)
library(MatchIt)

rm(list = ls())
gc()

# Dati --------------------------------------------------------------------
# kohorta
load(file = "data/proc/hosp_cohort_alive_w_comorb.RData")

# Apstrāde ----------------------------------------------------------------

cohort_comorb <- cohort_comorb %>% 
  mutate(hosp_gads = lubridate::year(date1))

# pārveido blakusslimības par faktoriem
cohort_comorb <- cohort_comorb %>% 
  mutate(across(mi:aids, ~{factor(., levels = c(0, 1))}))

cohort_comorb %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  count(cohort)

# paņem nejauši vienu hospitalizāciju katram pid
set.seed(2508)
cohort_m <- cohort_comorb %>% 
  mutate(rnum = runif(nrow(cohort_comorb))) %>% 
  arrange(rnum) %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  select(-rnum)

cohort_m %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  count(cohort)

# Matching ----------------------------------------------------------------
xvars <- cohort_comorb %>% 
  select(vecums, dzimums, mi:aids, charlson, hosp_gads) %>% 
  colnames()

fmla <- as.formula(paste0("cohort~", paste(xvars, collapse = "+")))
m.out <- cohort_comorb %>% 
  mutate(cohort = ifelse(cohort == "sepsis", 1, 0)) %>% 
  matchit(fmla, data = ., method = "nearest", distance = "glm", ratio = 3, exact = "hosp_gads")

# summary(m.out)

cohort_matched <- match.data(m.out)

glimpse(cohort_matched)

cohort_matched <- cohort_matched %>% 
  mutate(cohort = ifelse(cohort == 1, "sepsis", "control"))

# pārbauda pirms-pēc matching

plot(summary(m.out))

# pirms
cohort_comorb %>% 
  tableone::CreateTableOne(vars = xvars, strata = "cohort", 
                             data = ., test = FALSE)
# pēc
cohort_matched %>% 
 tableone::CreateTableOne(vars = xvars, strata = "cohort", 
               data = ., test = FALSE)

#atlasa tikai vienu pid 
# cohort_matched <- cohort_matched %>% distinct(pid, .keep_all = TRUE)

cohort_matched %>% count(cohort)

save(m.out, file = "data/proc/match_object.RData")
save(cohort_matched, file = "data/proc/hosp_cohort_matched.RData")

