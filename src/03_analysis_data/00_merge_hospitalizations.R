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
# Apvienojamo hospializāciju noteikšana
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

  if (all(boo1, boo2)) {   # ja abi izpildās, tad jāpievieno iepriekšējai hospitalizācijai
    join_index[i] <- k # epizodes indekss
    
    # pārbauda, vai epizode turpinās 
    if (i < nrow(stac_join)){
      # epizode beidzās, ja:
      # - nākamais ir cits pid
      boo3 <- stac_join$pid[i] != stac_join$pid[i + 1]
      # - starpība līdz nākamajai hospitalizācijai ir lielāka par 1
      boo4 <- stac_join$day_diff[i + 1] > 1
      
      if (any(boo3, boo4)) k <- k + 1 # nākamā epizode
    }
  }
  cat("\r")
  cat(round(i/nrow(stac_join)*100, 1), "%")
}

# pārbaude
# stac_join %>%
#   mutate(join_index = join_index) %>%
#   mutate(join_index_final = join_index_final) %>%
#   select(pid, date1, date2, day_diff, join_index, join_index_final) %>%
#   # drop_na(join_index_final) %>%
#   slice_head(n = 10)

join_index_final <- map_int(1:nrow(stac_join), ~ifelse(!is.na(join_index[. + 1]), join_index[. + 1], join_index[.]))

stac_join$join_index <- join_index_final

# stac_join %>% 
#   filter(!is.na(join_index)) %>% 
#   View()

# atjauno apvienojamo hopsitalizāciju sarakstu
stac_no_join <- bind_rows(
  stac_no_join,
  stac_join %>% filter(is.na(join_index)) %>% select(-join_index)
)

stac_join <- stac_join %>% drop_na(join_index)

# pārbaude
# nrow(stac_join) + nrow(stac_no_join) == nrow(stac) # TRUE

# -------------------------------------------------------------------------
# Hospitalizāciju apvienošana
# -------------------------------------------------------------------------

# skaitliskās kolonnas, kurām ņem pirmo vērtību
num_tmp1 <- stac_join %>% 
  select(pid, join_index, vecums, dzimsanas_gads) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~.x[1])) %>%  # pirmā vērtība
  ungroup()

# skaitliskās kolonnas, kurām ņem pēdējo vērtību
num_tmp2 <- stac_join %>% 
  select(pid, join_index, day_diff) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~last(.x))) %>%  # pēdējā vērtība
  ungroup()

# skaitliskās kolonnas, kurām summē
num_tmp3 <- stac_join %>% 
  select(pid, join_index, contains("summa")) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%  # pēdējā vērtība
  ungroup()

# apvieno skaitliskās kolonnas
num_tmp <- num_tmp1 %>% 
  left_join(num_tmp2, by = c("pid", "join_index")) %>% 
  left_join(num_tmp3, by = c("pid", "join_index"))

# datumu kolonnas
date_tmp <- stac_join %>% 
  group_by(pid, join_index) %>% 
  summarise(
    date1 = min(date1, na.rm = TRUE),
    date2 = max(date2, na.rm = TRUE)
  ) %>% 
  ungroup()

# simbolu kolonnas, kurām apvieno visas vērtības
cat_tmp1 <-stac_join %>% 
  select(pid, join_index, diag1, diag2,  gpf, nomesco_manip_kods, manip_kods) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~paste(na.omit(.), collapse = ";"))) %>% 
  ungroup()

# simbolu kolonnas, kurām ņem pirmo vērtību
cat_tmp2 <- stac_join %>% 
  select(pid, join_index, eid, stac_personas_atvk, stac_epizodes_id, stac_ud_nr, aik, iest_kust, dzimums) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~na.omit(.)[1])) %>% 
  ungroup()

# simbolu kolonnas, kurām ņem pēdējo vērtību
cat_tmp3 <- stac_join %>% 
  select(pid, join_index, izrakst_kust) %>% 
  group_by(pid, join_index) %>% 
  summarise(across(everything(), ~last(na.omit(.)))) %>% 
  ungroup()

# apvieno simbolu kolonnas
cat_tmp <- cat_tmp1 %>% 
  left_join(cat_tmp2, by = c("pid", "join_index")) %>% 
  left_join(cat_tmp3, by = c("pid", "join_index"))

# pārbauda rindu skaitu
all.equal(nrow(num_tmp), nrow(date_tmp), nrow(cat_tmp))

# apvieno visus failus
stac_joined <- num_tmp %>% 
  left_join(date_tmp, by = c("pid", "join_index")) %>% 
  left_join(cat_tmp, by = c("pid", "join_index"))

# setdiff(colnames(stac_join), colnames(stac_joined))
# [1] "file"        "sheet"       "source_full" OK

# kolonnu secība
cols_old <- colnames(stac_join)
cols_new <- colnames(stac_joined)
stac_joined <- stac_joined %>% 
  select(all_of(intersect(cols_old, cols_new)))

# -------------------------------------------------------------------------
# Pievieno apvienotās hospitalizācijas
# -------------------------------------------------------------------------   

stac_merged <- bind_rows(
  stac_no_join %>% select(all_of(colnames(stac_joined %>% select(-join_index)))),
  stac_joined %>% select(-join_index)
)

# nrow(stac) 
# nrow(stac_merged)
# nrow(stac_merged) - nrow(stac)

stac <- stac_merged
save(stac, file = "data/proc/clean_stac_merged.RData")
