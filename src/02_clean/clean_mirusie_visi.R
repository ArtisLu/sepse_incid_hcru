# -------------------------------------------------------------------------
# Sagatavo mirušo datubāzi - mirušie, kas bijuši hospitalizēti ar sepses kodiem
# -------------------------------------------------------------------------

library(tidyverse)
library(readxl)

rm(list = ls())

# data folder
data_path <- "data/raw/Sepse_mirusie"

# files to read
files_all <- list.files(path = "data/raw/Sepse_mirusie", recursive = TRUE, full.names = TRUE)

sheets_all <- map(files_all, ~excel_sheets(.))

all_contents <- tibble(
  file = files_all,
  sheet = sheets_all
) %>% 
  unnest(sheet) %>% 
  mutate(content = map2(file, sheet, ~read_xlsx(.x, .y)))

mirusie_contents <- all_contents %>% 
  mutate(cols = map(content, ~colnames(.))) %>% 
  mutate(is_mirs = map_lgl(cols, ~{any(grepl("mir", ., ignore.case = TRUE))})) %>% 
  filter(is_mirs)

# process
tmp <- mirusie_contents %>% 
  mutate(nave_dati = map(content, ~{.x %>% select(PID, contains("dat"))})) %>% 
  mutate(nave_dati = map(nave_dati, ~{.x %>% mutate(across(everything(), ~as.character(.)))})) %>% 
  mutate(nave_dati = map(nave_dati, ~{.x %>% janitor::clean_names()})) %>% 
  # mutate(ncol = map_int(nave_dati, ~{.x %>% ncol()})) %>% 
  # filter(ncol == 3) %>% 
  # mutate(not_equal = map_int(nave_dati, ~{all.equal(.x$mirsanas_datums_2, .x$mirsanas_datums_3)})) %>% 
  # mutate(not_equal = map(nave_dati, ~{.x %>% filter(mirsanas_datums_2 != mirsanas_datums_3)})) %>% 
  # pull(not_equal) %>% 
  # mutate(nave_dati = map(nave_dati, ~{.x %>% select(-contains("mirsanas_datums_2"))})) %>%
  mutate(nave_dati = map(nave_dati, ~{.x %>% rename(datums = 2)})) %>%
  pull(nave_dati) %>%
  bind_rows() %>%
  # mutate(datums_new = lubridate::ymd(datums)) %>% 
  # filter(is.na(datums_new)) %>% 
  # count(datums) %>% 
  {.}

tmp <- tmp %>% 
  mutate(format = gsub("[0-9]", "x", datums))

tmp %>% 
  count(format)

tmp1 <- tmp %>% 
  filter(format == "xxxxx") %>% 
  mutate(datums_new = as.Date(as.integer(datums), origin = "1899-12-30")) 

tmp2 <- tmp %>% 
  filter(format == "xxxx-xx-xx") %>% 
  mutate(datums_new = lubridate::ymd(datums))
  
mirusie <- bind_rows(
  tmp1 %>% select(pid, datums = datums_new),
  tmp2 %>% select(pid, datums = datums_new)
) %>% 
  # mutate(format = gsub("[0-9]", "x", datums)) %>% 
  # count(format) %>% 
  {.}

# atstāj tikai unikālos
mirusie <- mirusie %>% 
  distinct()

# mirusie %>% distinct(pid)

save(mirusie, file = "data/cleaned/clean_mirusie_visi.RData")
