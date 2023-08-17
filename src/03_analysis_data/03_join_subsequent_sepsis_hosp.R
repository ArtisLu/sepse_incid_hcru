library(tidyverse)

load(file = "data/proc/hosp_cohort_all.RData")

tmp <- as_tibble(cohort) %>% 
  arrange(pid, date1) %>% 
  group_by(pid) %>% 
  mutate(daydiff = as.integer(date1 - lag(date2))) %>% 
  mutate(is_subseq = ifelse(daydiff < 2, TRUE, FALSE)) %>% 
  ungroup()

tmp %>% 
  # filter(daydiff == -121) %>% 
  # filter(pid == "L_3f9c2b83ca26c6") %>% 
  # View()
  count(daydiff, is_subseq) %>% 
  filter(is_subseq) %>% 
  print(n = Inf) %>% 
  # arrange(daydiff) %>% 
  {.}

489/273390

