# -------------------------------------------------------------------------
# Definē analīzes rādītājus sepses izdzīvojušo analīzei
# -------------------------------------------------------------------------

rm(list = ls())

# Pakotnes ----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Dati --------------------------------------------------------------------
# matched kohorta
load("data/03_intermediate/survivor_cohort_matched.RData")

# mirušie
load("data/02_cleaned/clean_mirusie_visi.RData")

# hospitalizācijas
load("data/03_intermediate/clean_stac_merged.RData")

# kompensējamie medikamenti
load("data/02_cleaned/clean_komp_med_stac.RData") 

# ambulatorās epizodes
load("data/02_cleaned/clean_ambul_stac.RData") 

# ssk-10 nosaukumi
ssk10_names <- read_xlsx("taskfiles/raw/ssk10_sekciju_nosaukumi.xlsx")

# specialistu kodi
spec_codes <- read_xlsx("taskfiles/raw/AM_specialitasu_klasifikators_LP_JB.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(remove == 0) %>% 
  select(-biezums_gada_2018, -lp, -remove) %>% 
  pivot_longer(laboratory_diagnostics:rehabilitation, names_to = "tips", values_to = "ind") %>% 
  drop_na() %>%
  select(spec_kods = specialitates_kods, tips)

# rakstam - rehabilitācija = other_care_manipulations
spec_codes <- spec_codes %>% 
  mutate(tips = ifelse(tips == "rehabilitation", "other_care_manipulations", tips))

#--------------------------------------------------------------------
# Definē hospitalizāciju raksturojošos parametrus
#--------------------------------------------------------------------
# follow-up start / end
cohort_analysis <- cohort_matched %>% 
  mutate(fup_start = date2, fup_end = date2 + 364)

# atlasa nepieciešamās kolonnas
cohort_analysis <- cohort_analysis %>% 
  select(pid, eid, vecums, dzimums, charlson_quan, mi:aids, diag1, date1, date2, fup_start, fup_end, cohort)

# izrakstīšanās gads
cohort_analysis <- cohort_analysis %>% 
  mutate(izrakst_gads = year(date2))

# hospitalizācijas ilgums
cohort_analysis <- cohort_analysis %>% 
  mutate(hosp_ilgums = as.integer(date2 - date1))

# pamatdiganozes ICD-10 grupa
cohort_analysis <- cohort_analysis %>% 
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
pamat_diag_order <- cohort_analysis %>%
  count(pamat_diag_grupa) %>% 
  arrange(-n) %>% 
  drop_na(pamat_diag_grupa) %>% 
  pull(pamat_diag_grupa)

cohort_analysis <- cohort_analysis %>% 
  mutate(pamat_diag_grupa = fct_relevel(pamat_diag_grupa, pamat_diag_order))

#--------------------------------------------------------------------
# Definē iznākumus follow-up periodā
#--------------------------------------------------------------------

# Nāve --------------------------------------------------------------------
cohort_analysis <- cohort_analysis %>% 
  left_join(mirusie %>% rename(nave_date = datums), by = "pid") %>% 
  mutate(obs_date_nave = pmin(fup_end, nave_date, na.rm = TRUE)) %>% 
  mutate(
    cens_nave = case_when(
      is.na(nave_date) ~ 0,
      obs_date_nave == nave_date ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(time_nave = pmax(0, as.integer(obs_date_nave - date2)))


# pārbaude
cohort_analysis %>% 
  count(cens_nave, is.na(nave_date))

# Rehospitalizācijas ------------------------------------------------------

# ņem tikai rehospitalizācijas follow-up periodā
rehosp <- stac %>% 
  left_join(cohort_analysis %>% select(pid, fup_start, fup_end), by = "pid") %>% 
  filter(date1 >= fup_start & date1 <= fup_end) %>% 
  mutate(summa = ifelse(summa_par_gadijumu == 0, summa_bez_pac_iem, summa_par_gadijumu)) %>% # Jura formula
  select(-all_of(contains("summa_")))

# aprēķina pacienta līmeņa rādītājus
rehosp <- rehosp %>% 
  mutate(hosp_ilgums = as.integer(date2 - date1)) %>% 
  mutate(summa_pac = ifelse(year(date1) < 2015, 14 * hosp_ilgums, 10 * hosp_ilgums)) %>% # pacienta iemaksa, Jura epasts
  mutate(summa_total = summa + summa_pac) %>% 
  group_by(pid) %>% 
  summarise(
    first_rehosp_date = min(date1),
    nb_hospitalizations = n(),
    hospital_nights = sum(hosp_ilgums),
    rehosp_summa = sum(summa_total)
  )

# pievieno kohortai
cohort_analysis <- cohort_analysis %>% 
  left_join(rehosp, by = "pid") %>% 
  mutate(obs_date_rehosp = pmin(fup_end, nave_date, first_rehosp_date, na.rm = TRUE)) %>% 
  mutate(
    cens_rehosp = case_when(
      is.na(first_rehosp_date) ~ 0,
      obs_date_rehosp == first_rehosp_date ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(time_rehosp = pmax(0, as.integer(obs_date_rehosp - date2))) %>% 
  mutate(nb_hospitalizations = replace_na(nb_hospitalizations, 0))

#--------------------------------------------------------------------
# HCRU
#--------------------------------------------------------------------

# Ambulatorie -------------------------------------------------------------
visits <- ambul %>% 
  filter(pid %in% cohort_analysis$pid) %>%
  left_join(cohort_analysis %>% select(pid, fup_start, fup_end), by = "pid") %>%
  filter(sakuma_datums >= fup_start & sakuma_datums <= fup_end) %>% 
  rename(spec_kods = ap_specialitate)

# pievieno speciālista tipu
visits <- visits %>% 
  left_join(spec_codes) %>% 
  drop_na(tips)

# apkopu pēc pid un tipa
visits_aggr <- visits %>% 
  mutate(across(contains("summa"), ~as.numeric(.))) %>% 
  mutate(across(contains("iemaksa"), ~as.numeric(.))) %>% 
  group_by(pid, tips) %>% 
  summarise(
    skaits = n(),
    summa_epiz = sum(summa_epizodes, na.rm = TRUE),
    summa_manip = sum(summa_manipulacijas, na.rm = TRUE),
    summa_pac_iemaks_pac = sum(pacienta_iemaksa_no_pacienta, na.rm = TRUE),
    summa_pac_iemaks_valsts = sum(pacienta_iemaksa_no_valsts, na.rm = TRUE),
    summa_kopa = summa_epiz + summa_manip + summa_pac_iemaks_pac + summa_pac_iemaks_valsts
  ) %>% 
  select(pid, tips, skaits, summa_kopa) %>% 
  ungroup()

visits_aggr_wide <- crossing(cohort_analysis %>% select(pid), spec_codes %>% select(tips)) %>% 
  left_join(visits_aggr) %>% 
  mutate(
    skaits = replace_na(skaits, 0)
  ) %>% 
  pivot_longer(skaits:summa_kopa, names_to = "var", values_to = "value") %>% 
  mutate(var = gsub("_kopa", "", var)) %>% 
  mutate(tips = paste0(tips, "_", var)) %>% 
  select(-var) %>% 
  pivot_wider(names_from = tips, values_from = value) 
  

cohort_analysis <- cohort_analysis %>% left_join(visits_aggr_wide)

# Definē utilization intensity
cohort_analysis <- cohort_analysis %>% 
  mutate(utilization_intensity = hosp_ilgums + primary_care_physician_visits_skaits + specialists_visits_skaits +other_care_manipulations_skaits + other_diagnostics_skaits) %>% 
  mutate(outpatient_skaits = utilization_intensity - hosp_ilgums) %>% 
  mutate(outpatient_summa = primary_care_physician_visits_summa + specialists_visits_summa + other_care_manipulations_summa + other_diagnostics_summa)

# Receptes ----------------------------------------------------------------
drugs <- komp_med %>% 
  filter(pid %in% cohort_analysis$pid) %>%
  left_join(cohort_analysis %>% select(pid, fup_start, fup_end), by = "pid") %>%
  filter(datums >= fup_start & datums <= fup_end) %>% 
  mutate(across(contains("summa"), ~as.numeric(gsub(",", ".", .)))) %>% 
  mutate(across(contains("lidzmaksajums"), ~as.numeric(gsub(",", ".", .))))

drug_aggr <- drugs %>% 
  group_by(pid) %>% 
  summarise(
    skaits = n(),
    summa_valsts = sum(valsts_summa),
    summa_pac = sum(pacienta_lidzmaksajums),
    summa_kopa = summa_valsts + summa_pac
  ) %>% 
  select(pid, receptes_skaits = skaits, receptes_summa = summa_kopa)

cohort_analysis <- cohort_analysis %>% 
  left_join(drug_aggr) %>% 
  mutate(receptes_skaits = replace_na(receptes_skaits, 0))

# -------------------------------------------------------------------------
# Saglabā 
# -------------------------------------------------------------------------

# sakārto raksta secībā un atmet liekās kolonnas
survivor_cohort <- cohort_analysis %>% 
  select(pid, vecums, dzimums, index_hosp_ilgums = hosp_ilgums, charlson = charlson_quan,
         mi:aids, izrakst_gads, pamat_diag_grupa, nave_date:time_nave, first_rehosp_date, obs_date_rehosp:time_rehosp,
         nb_hospitalizations, hospital_nights, 
         primary_care_physician_visits_skaits, specialists_visits_skaits, laboratory_diagnostics_skaits, 
         other_diagnostics_skaits, other_care_manipulations_skaits,
         receptes_skaits, utilization_intensity, outpatient_skaits,
         rehosp_summa, outpatient_summa, primary_care_physician_visits_summa, specialists_visits_summa,
         laboratory_diagnostics_summa, other_diagnostics_summa, other_care_manipulations_summa,
         receptes_summa, cohort)


save(survivor_cohort, file = "data/04_analysis_data/survivor_matched_cohorts.RData")

