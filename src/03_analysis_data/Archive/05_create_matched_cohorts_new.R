# -------------------------------------------------------------------------
# Izveido matched kohortas
# -------------------------------------------------------------------------

library(tidyverse)
library(MatchIt)

rm(list = ls())
gc()


# Funkcijas ---------------------------------------------------------------
keep_single_pid <- function(data) {
  data %>% 
    mutate(rnum = runif(nrow(data))) %>% 
    arrange(rnum) %>% 
    distinct(pid, .keep_all = TRUE) %>% 
    select(-rnum)
}

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

# izveido 4 atsevišķas matching cohortas:
# 1) visas ne-sepses kopā
cohort_match_all <- cohort_comorb %>% 
  filter(cohort != "sepsis")

cohort_match_all %>% count(tiesie, netiesie, org_trauc)

# 2) netiesie vai org trauc
cohort_match_inf_org <- cohort_comorb %>% 
  filter(cohort != "sepsis") %>% 
  filter(netiesie | org_trauc)

cohort_match_inf_org %>% count(tiesie, netiesie, org_trauc)

# 3) netiesie
cohort_match_inf <- cohort_comorb %>% 
  filter(cohort != "sepsis") %>% 
  filter(netiesie)

cohort_match_inf %>% count(tiesie, netiesie, org_trauc)

# 4) org_trauc
cohort_match_org <- cohort_comorb %>% 
  filter(cohort != "sepsis") %>% 
  filter(org_trauc)

cohort_match_org %>% count(tiesie, netiesie, org_trauc)

# paņem nejauši vienu hospitalizāciju katram pid
set.seed(2508)
cohort_match_all <- keep_single_pid(cohort_match_all)
cohort_match_inf_org <- keep_single_pid(cohort_match_inf_org)
cohort_match_inf <- keep_single_pid(cohort_match_inf)
cohort_match_org <- keep_single_pid(cohort_match_org)

# pievieno visām sepses grupu
cohort_list <- list(cohort_match_all, cohort_match_inf_org, cohort_match_inf, cohort_match_org)
cohort_names <- c("cohort_match_all", "cohort_match_inf_org", "cohort_match_inf", "cohort_match_org") %>% 
  gsub("cohort_match_", "", .)

names(cohort_list) <- cohort_names

cohort_list <- map(cohort_list, ~{bind_rows(., cohort_comorb %>% filter(cohort == "sepsis"))})

# Matching ----------------------------------------------------------------
xvars <- cohort_comorb %>% 
  select(vecums, dzimums, mi:aids, charlson, hosp_gads) %>% 
  colnames()

fmla <- as.formula(paste0("cohort~", paste(xvars, collapse = "+")))

matchings <- list()

# perform matching
for (i in 1:4) { # i <- 1
  matchings[[i]] <- cohort_list[[i]] %>% #[sample(1:nrow(cohort_list[[i]]), 5000), ] %>% 
    mutate(cohort = ifelse(cohort == "sepsis", 1L, 0L)) %>% 
    matchit(fmla, data = ., method = "nearest", distance = "glm", ratio = 3, exact = "hosp_gads")
  
  print(i)
}

# get matched datasets
matched_datasets <- map(matchings, ~{match.data(.) %>% mutate(cohort = ifelse(cohort == 1, "sepsis", "control"))})

# pārbauda pirms-pēc matching

map(matchings, ~plot(summary(.)))

# pirms
# cohort_comorb %>% 
#   tableone::CreateTableOne(vars = xvars, strata = "cohort", 
#                              data = ., test = FALSE)
# pēc
# cohort_matched %>% 
#  tableone::CreateTableOne(vars = xvars, strata = "cohort", 
#                data = ., test = FALSE)

#atlasa tikai vienu pid 
# cohort_matched <- cohort_matched %>% distinct(pid, .keep_all = TRUE)

# cohort_matched %>% count(cohort)

save(matchings, file = "data/proc/match_objects.RData")
save(matched_datasets, file = "data/proc/hosp_cohort_matched_datasets.RData")

