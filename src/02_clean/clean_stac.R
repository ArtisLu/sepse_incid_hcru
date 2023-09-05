# -------------------------------------------------------------------------
# Sagatavo hospitalizāciju datubāzi
# -------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# datu mape
data_path <- "data/raw/Stac_sepse"

# visi ielasāmie faili
files <- list.files(
  data_path,
  pattern = "Stacionetie_sepse_|Stac_.*_sepse_papilddg2",
  full.names = TRUE
)

# failu saturs
all_contents <- tibble(file = files) %>% 
  mutate(sheet = map(file, ~readxl::excel_sheets(.))) %>% 
  unnest(sheet)

# cik lapas katrā failā
all_contents %>% 
  count(file)

# lapu nosaukumi
all_contents %>% 
  print(n = Inf)

all_contents <- all_contents %>% 
  mutate(table = map2(
    file, sheet,
    ~readxl::read_excel(path = .x, sheet = .y, guess_max = 10000))
  )

# salīdzina kāds pārklājums
all_contents %>% 
  mutate(file = gsub("data/raw/Stac_sepse/", "", file)) %>% 
  mutate(nrow = map_dbl(table, nrow)) %>% 
  filter(!(file == "Stacionetie_sepse_2014.xlsx" & sheet == "Sheet2")) %>% 
  mutate(year = gsub("Stac_|\\.xlsx|_sepse_papilddg2|Stacionetie_sepse_", "", file)) %>% 
  arrange(file, year, nrow) %>% 
  group_by(file) %>% 
  mutate(id = seq_along(year)) %>% 
  mutate(id = paste0("sheet_", id)) %>% 
  select(file, year, id, nrow, table) %>%
  pivot_wider(names_from = "id", values_from = c("nrow", "table")) %>% 
  ungroup() %>% 
  mutate(pids1 = map(table_sheet_1, ~{.$PID})) %>% 
  mutate(pids2 = map(table_sheet_2, ~{.$PID})) %>% 
  mutate(overlap1 = map2_dbl(pids1, pids2, ~{sum(.x %in% .y)})) %>% 
  mutate(overlap2 = map2_dbl(pids1, pids2, ~{sum(.y %in% .x)})) %>% 
  select(file, year, nrow_sheet_1, nrow_sheet_2, overlap1, overlap2)

# pārveido visas kolonnas par tekstu un pievieno avotu
data_full <- all_contents %>% 
  mutate(source_full = map_chr(file, ~gsub("^data/raw/Stac_sepse/|\\.xlsx", "", .))) %>% 
  mutate(table = map(
    table,
    ~{
      df <- .
      df %>% mutate_all(as.character)
    })
  ) %>% 
  unnest(table)

# pārbaude
data_full %>% 
  count(source_full, sheet) %>% 
  print(n = Inf)
  
# sakārto kolonnu nosaukumus
data_full <- janitor::clean_names(data_full)

# pārsauc kolonnas
stac <- data_full %>%
  rename(
    date1 = stac_kustiba_hospitalizacijas_datums,
    date2 = stac_kustiba_izrakstisanas_datums,
    diag1 = stac_pamatdiagnozes_kods,
    diag2 = stac_papild_diagnozes_kods,
    aik = stac_aik,
    iest_kust = stac_kustiba_iestasanas_kustiba,
    izrakst_kust = stac_kustiba_izrakstisanas_kustiba,
    gpf = stac_gpf,
    summa_bez_manip = stac_kartes_summa_bez_manipulacijam,
    summa_bez_pac_iem = stac_kartes_summa_bez_pacienta_iemaksas,
    summa_par_gadijumu = stac_kartes_summa_par_gadijumu,
    summa_par_gultu_dienam = stac_kartes_summa_par_gultu_dienam,
    nomesco_manip_kods = stac_nomesco_manip_kods,
    manip_kods = stac_manip_kods,
    vecums = vecums_hospitalizacijas_bridi 
  )

# nrow(stac)
# 1158078

# pārbauda, cik pid ir trūkstoši
stac %>% count(is.na(pid))
# `is.na(pid)`       n
# <lgl>          <int>
#   1 FALSE        1158078

# pārbauda, cik stac id ir trūkstoši
stac %>%
  count(source_full, is_stac = !is.na(stac_epizodes_id)) %>% 
  pivot_wider(names_from = is_stac, values_from = n)

# nomaina "-" uz NA
stac <- stac %>% 
  mutate(across(everything(), ~replace(., .== "-", NA)))

# Dublikātu noņemšana -----------------------------------------------------

# 1) noņem visas pilnīgi identiskās hospitalizācijas
tmp <- stac
nrow(tmp)
glimpse(tmp)

tmp1 <- tmp %>% distinct(across(-c(file, sheet, source_full, stac_epizodes_id)), .keep_all = TRUE)
tmp2 <- tmp %>% distinct(across(-c(file, sheet, source_full, stac_epizodes_id, pid)), .keep_all = TRUE)
nrow(tmp1)
nrow(tmp2)

stac_unique <- tmp1

# 2) noņem pid ar kļūdainiem dzimumiem
pid_remove_dzim <- stac_unique %>% 
  distinct(pid, dzimums) %>% 
  count(pid) %>% 
  filter(n > 1) %>% 
  pull(pid)

stac_unique <- stac_unique %>% 
  filter(!(pid %in% pid_remove_dzim))

# 2) izveido hospitalizācijas kodus no pid + date1 + date2 + ārstniec. iestādes kods
# izveido jaunu epizodes kodu
stac_unique <- stac_unique %>% 
  mutate(eid1 = map2_chr(pid, date1, ~ paste(c(.x, .y), collapse = "_"))) %>% 
  mutate(eid2 = map2_chr(eid1, date2, ~ paste(c(.x, .y), collapse = "_"))) %>%
  mutate(eid3 = map2_chr(eid2, stac_personas_atvk, ~ paste(c(.x, .y), collapse = "_"))) %>% 
  arrange(eid1, eid2, eid3)

# dublikāti pēc eid1?
nrow(stac_unique)
nrow(distinct(stac_unique, eid1)) 
nrow(distinct(stac_unique, eid2))
nrow(distinct(stac_unique, eid3))
# [1] 608493

stac_unique <- stac_unique %>%
  add_count(eid3, name = "n_eid3")

# Šīm hospitalizācijām atšķiras diagnozes, manipulāciju kodi, stac. kartes summa utml.
# Tā kā šādu hospitalizāciju nav daudz - noņemam.
eid_remove_inconsistent <- stac_unique %>% 
  filter(n_eid3 > 2) %>%
  pull(eid3) %>% 
  unique()

stac_unique <- stac_unique %>% 
  filter(!(eid3 %in% eid_remove_inconsistent))

# pārbauda, vai visām atlikušajām hospitalizācijām ar n_eid3 > 1 sakrīt diag1
eid_remove_diag_inconsistent <- stac_unique %>% 
  filter(n_eid3 == 2) %>% 
  group_by(eid3) %>% 
  summarise(diag1_n = length(unique(diag1))) %>% 
  filter(diag1_n > 1) %>% 
  pull(eid3)

stac_unique <- stac_unique %>% 
  filter(!(eid3 %in% eid_remove_diag_inconsistent))

# no atlikušajiem n_eid3 > 1 atstāj tos, kam garāka kopējā diagnoze
tmp1 <- stac_unique %>% 
  filter(n_eid3 == 1)

tmp2 <- stac_unique %>% 
  filter(n_eid3 == 2) %>% 
  mutate(len_diag = map2_int(diag1, diag2, ~ nchar(paste0(.x, .y)))) %>% 
  arrange(eid3, -len_diag) %>% 
  distinct(eid3, .keep_all = TRUE)

stac_unique <- bind_rows(tmp1, tmp2)
nrow(distinct(stac_unique, eid3))


# Sakārto datu tipus ------------------------------------------------------
glimpse(stac_unique)
# simbolu mainīgie
char_cols <- c("file", "sheet", "pid", "stac_personas_atvk", "stac_epizodes_id", "stac_ud_nr",
               "diag1", "diag2", "aik", "iest_kust", "izrakst_kust",
               "gpf", "nomesco_manip_kods", "manip_kods", "dzimums", "source_full", "eid1", "eid2", "eid3")

# skaitliskie mainīgie
num_cols <- c("summa_bez_manip", "summa_bez_pac_iem",
              "summa_par_gadijumu", "summa_par_gultu_dienam",
              "vecums", "dzimsanas_gads", "n_eid3", "len_diag")

# datumu mainīgie
date_cols <- c("date1", "date2")

# pārbauda
setdiff(colnames(stac_unique), c(char_cols, num_cols, date_cols))

# pārveido simbolu mainīgos
stac_unique <- stac_unique %>% 
  mutate(across(all_of(char_cols), ~as.character(.)))

# pārveido skaitliskos mainīgos
stac_unique <- stac_unique %>% 
  mutate(across(all_of(num_cols), ~as.character(.))) %>% 
  mutate(across(all_of(num_cols), ~gsub(",", ".", .))) %>% 
  mutate(across(all_of(num_cols), ~as.numeric(.)))

# pārveido datumu mainīgos
stac_unique <- stac_unique %>% 
  mutate(across(all_of(date_cols), ~as.Date(.)))

stac_unique %>% glimpse()

stac <- stac_unique %>%
  select(file:source_full, eid = eid3) %>%
  relocate(eid, .after = pid)

save(stac, file = "data/02_cleaned/clean_stac.RData")
