# -------------------------------------------------------------------------
# Sagatavo datu kopu incidences analīzei
# -------------------------------------------------------------------------

rm(list = ls())

#--------------------------------------------------------------------
# Pakotnes
#--------------------------------------------------------------------
library(tidyverse)
library(readxl)

# -------------------------------------------------------------------------
# Dati
# -------------------------------------------------------------------------
load("data/03_intermediate/sepsis_hosp_w_comorb.RData")

# ssk-10 nosaukumi
ssk10_names <- read_xlsx("taskfiles/raw/ssk10_sekciju_nosaukumi.xlsx")

#--------------------------------------------------------------------
# Definē hospitalizāciju raksturojošos parametrus
#--------------------------------------------------------------------

# atlasa tikai virs 18 gadiem
sepsis_hosp <- sepsis_hosp %>% 
  filter(vecums >= 18)

# atlasa nepieciešamās kolonnas
hosp_descr <- sepsis_hosp %>% 
  select(pid, eid, vecums, dzimums, charlson, charlson_quan, mi:aids, diag1, date1, date2, contains("summa_"), izrakst_kust)

# hospitalizācijas gads
hosp_descr <- hosp_descr %>% 
  mutate(gads = year(date1))

# hospitalizācijas ilgums
hosp_descr <- hosp_descr %>% 
  mutate(hosp_ilgums = as.integer(date2 - date1))

# pamatdiganozes ICD-10 grupa
hosp_descr <- hosp_descr %>% 
  mutate(diag_tmp = substr(diag1, 1, 2)) %>% 
  mutate(
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[1], diag_tmp), ssk10_names$nodala[1], diag_tmp),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[2], diag_tmp_new), ssk10_names$nodala[2], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[3], diag_tmp_new), ssk10_names$nodala[3], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[4], diag_tmp_new), ssk10_names$nodala[4], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[5], diag_tmp_new), ssk10_names$nodala[5], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[6], diag_tmp_new), ssk10_names$nodala[6], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[7], diag_tmp_new), ssk10_names$nodala[7], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[8], diag_tmp_new), ssk10_names$nodala[8], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[9], diag_tmp_new), ssk10_names$nodala[9], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[10], diag_tmp_new), ssk10_names$nodala[10], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[11], diag_tmp_new), ssk10_names$nodala[11], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[12], diag_tmp_new), ssk10_names$nodala[12], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[13], diag_tmp_new), ssk10_names$nodala[13], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[14], diag_tmp_new), ssk10_names$nodala[14], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[15], diag_tmp_new), ssk10_names$nodala[15], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[16], diag_tmp_new), ssk10_names$nodala[16], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[17], diag_tmp_new), ssk10_names$nodala[17], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[18], diag_tmp_new), ssk10_names$nodala[18], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[19], diag_tmp_new), ssk10_names$nodala[19], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[20], diag_tmp_new), ssk10_names$nodala[20], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[21], diag_tmp_new), ssk10_names$nodala[21], diag_tmp_new),
    diag_tmp_new = ifelse(grepl(ssk10_names$regex_range[22], diag_tmp_new), ssk10_names$nodala[22], diag_tmp_new)
  ) %>% 
  rename(pamat_diag_grupa = diag_tmp_new) %>% 
  select(-diag_tmp) %>% 
  mutate(pamat_diag_grupa = ifelse(pamat_diag_grupa == "W6", NA, pamat_diag_grupa)) %>% 
  mutate(pamat_diag_grupa = gsub("^.*? ", "", pamat_diag_grupa))

# sakārto pamatdiagnozes secībā pēc biežuma
pamat_diag_order <- hosp_descr %>%
  count(pamat_diag_grupa) %>% 
  arrange(-n) %>% 
  drop_na(pamat_diag_grupa) %>% 
  pull(pamat_diag_grupa)

hosp_descr <- hosp_descr %>% 
  mutate(pamat_diag_grupa = fct_relevel(pamat_diag_grupa, pamat_diag_order))

# pirmā hospitalizācija gadā
hosp_descr <- hosp_descr %>% 
  arrange(pid, date1) %>% 
  group_by(pid, gads) %>% 
  mutate(seq = seq_along(pid)) %>% 
  mutate(pirma_sepse_gada = ifelse(seq == 1, 1, 0)) %>% 
  ungroup()

# nāve
hosp_descr <- hosp_descr %>% 
  mutate(nave = ifelse(izrakst_kust == "33", 1, 0))

# hospitalizācijas izmaksas (pēc formulas no Jura piemēra, epasts 2023-04-13)
hosp_descr <- hosp_descr %>% 
  mutate(summa = ifelse(summa_par_gadijumu == 0, summa_bez_pac_iem, summa_par_gadijumu)) %>% 
  select(-all_of(contains("summa_")))

# vecuma grupas
hosp_descr <- hosp_descr %>% 
  mutate(vecums_cat = cut(vecums, c(0, 20, 40, 60, 80, Inf), right = FALSE, labels = c("<20","20-39", "40-59", "60-79", "80+")))

#--------------------------------------------------------------------
# Definē pacientu raksturojošos parametrus katrā gadā
#--------------------------------------------------------------------

# hospitalizāciju ar sepsi skaits gadā
patient_descr <- hosp_descr %>% 
  count(pid, gads, name = "n_sepsis_hosp")
  
# patients readmitting within 1-year
tmp <- hosp_descr %>% 
  group_by(pid) %>% 
  mutate(next_hosp_within = as.integer(lead(date1) - date2)) %>% 
  ungroup() %>% 
  mutate(sepsis_hosp_1g = ifelse(next_hosp_within <= 365, 1, 0)) %>% 
  mutate(sepsis_hosp_1g = ifelse(is.na(sepsis_hosp_1g), 0, sepsis_hosp_1g))

tmp <- tmp %>% 
  group_by(pid, gads) %>% 
  summarise(readmit_1g = sum(sepsis_hosp_1g) > 0) %>% 
  ungroup() %>% 
  mutate(readmit_1g = ifelse(readmit_1g, 1, 0))

patient_descr <- patient_descr %>% 
  left_join(tmp, by = c("pid", "gads"))

# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
# The id variable as defined by the user;
# • mi, for myocardial infarction;
# • chf, for congestive heart failure;
# • pvd, for peripheral vascular disease;
# • cevd, for cerebrovascular disease;
# • dementia, for dementia;
# • cpd, for chronic pulmonary disease;
# • rheumd, for rheumatoid disease;
# • pud, for peptic ulcer disease;
# • mld, for mild liver disease;
# • diab, for diabetes without complications;
# • diabwc, for diabetes with complications;
# • hp, for hemiplegia or paraplegia;
# • rend, for renal disease;
# • canc, for cancer (any malignancy);
# • msld, for moderate or severe liver disease;
# • metacanc, for metastatic solid tumour;
# 6 comorbidity
# • aids, for AIDS/HIV


# -------------------------------------------------------------------------
# Saglabā 
# -------------------------------------------------------------------------

# atmet liekās kolonnas
hosp_descr <- hosp_descr %>% 
  select(-pid, -eid, -charlson, -diag1, -date1, -date2, -izrakst_kust, -seq)

# atmet 2013. gadu
hosp_descr <- hosp_descr %>% 
  filter(gads > 2014)

save(hosp_descr, file = "data/04_analysis_data/sepsis_hosp_characteristics.RData")

save(patient_descr, file = "data/04_analysis_data/sepsis_patient_characteristics.RData")


