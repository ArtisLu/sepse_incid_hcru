# -------------------------------------------------------------------------
# Apvieno blakus esošas hospitalizācijas epizodēs
# -------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)


# -------------------------------------------------------------------------
# Dati
# -------------------------------------------------------------------------

# visa datubāze
stac <- readRDS("data/proc/clean_stac.rds")
# stac0 <- stac
# stac <- stac0

# -------------------------------------------------------------------------
# Hospitalizāciju apvienošana
# -------------------------------------------------------------------------

# Aprēķina starpības starp sekojošām hospitalizācijām
stac <- stac %>% 
  arrange(pid, date1) %>% 
  group_by(pid) %>% 
  mutate(day_diff = as.integer(date1 - lag(date2))) %>% 
  ungroup()

# hospitalizācijas ar negatīvu day_diff
# nrow(stac %>% filter(day_diff < 0))
# 635
# hospitalizācijas ar day_diff = 0, 1
# nrow(stac %>% filter(day_diff %in% c(0, 1)))
# 40221
# 40221/nrow(stac)
# 0.06610827 - apmēram 6-7% hospitalizāciju jāapvieno
# PIEZĪME: apvieno arī hospitalizācijas ar day_diff < 0

# Atlasa pid, kuriem vismaz kāda hospitalizācija jāapvieno (day_diff <0, 0, 1 = <2)
pid_with_join <- stac %>% 
  group_by(pid) %>% 
  mutate(pid_with_join = any(day_diff < 2)) %>%
  filter(pid_with_join) %>% 
  pull(pid) %>% 
  unique()
  
pid_no_join <- setdiff(unique(stac$pid), pid_with_join)

stac_join <- stac %>% filter(pid %in% pid_with_join)
stac_no_join <- stac %>% filter(pid %in% pid_no_join)

# checks
# nrow(stac_join) + nrow(stac_no_join) == nrow(stac) # TRUE
# nrow(stac_no_join %>% filter(day_diff < 2)) # 0

# Nosaka, kuras hospitalizācijas jāpievieno iepriekšējai
join_index <- vector() # apvienojamo hospitalizāciju indeksi

k <- 1
boo1 <- FALSE
boo2 <- FALSE

# pirms pārbaudes aizstāj NA starpības ar 999999
stac_join$day_diff <- replace_na(stac_join$day_diff, 999999)

for (i in 2:nrow(stac_join)){ # 1. hospitalizāciju nevar pievienot
  
  # vai iepriekšējais pid ir tāds pats
  boo1 <- (stac_join$pid[i] == stac_join$pid[i - 1]) 
  
  # vai pagājis mazāk par 2 dienām
  boo2 <- stac_join$day_diff[i] < 2

  if (boo1 & boo2) {
    join_index[i] <- k
     
    if (i < nrow(stac_join)){
      # epizode beidzās, ja:
      # - nākamais ir cits pid
      boo3 <- stac_join$pid[i] != stac_join$pid[i + 1]
      # - starpība līdz nākamajai hospitalizācijai ir lielāka par 1
      boo4 <- stac_join$day_diff[i + 1] > 1
      
      if (boo3 | boo4) k <- k + 1 # nākamā epizode
    }
  }
  cat("\r")
  cat(round(i/nrow(stac_join)*100, 1), "%")
}

join_index_final <- map_int(1:nrow(stac_join), ~ifelse(!is.na(join_index[. + 1]), join_index[. + 1], join_index[.]))

stac_join$join_index <- join_index_final

stac_join %>% 
  filter(!is.na(join_index)) %>% 
  View()