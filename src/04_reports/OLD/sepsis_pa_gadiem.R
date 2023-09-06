load("data/proc/stac_sepsis_ind.RData")

sepsis_counts <- stac %>%
  mutate(gads = lubridate::year(date1)) %>% 
  filter(gads > 2014) %>% 
  mutate(gads = as.factor(gads)) %>% 
  filter(explicit | implicit) %>% 
  mutate(sepsis = ifelse(explicit, "explicit", "implicit")) %>% 
  count(gads, sepsis) %>% 
  arrange(gads, sepsis) %>% 
  group_by(gads) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()

sepsis_hosp_mirusie_counts <- stac %>% 
  mutate(miris_hosp = ifelse(stac_kustiba_izrakstisanas_kustiba == "33", "Y", "N")) %>% 
  mutate(gads = lubridate::year(date1)) %>% 
  filter(gads > 2014) %>% 
  mutate(gads = as.factor(gads)) %>% 
  filter(explicit | implicit) %>% 
  mutate(sepsis = ifelse(explicit, "explicit", "implicit")) %>% 
  count(gads, miris_hosp) %>% 
  arrange(gads, miris_hosp) %>% 
  group_by(gads) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()

