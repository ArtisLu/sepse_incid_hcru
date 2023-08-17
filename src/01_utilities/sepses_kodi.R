# -------------------------------------------------------------------------
# Pārveido sepses kodu sarakstus no teksta uz "raw" tabulām
# -------------------------------------------------------------------------


rm(list = ls())

# Pakotnes ----------------------------------------------------------------
library(tidyverse)

# Dati --------------------------------------------------------------------
paths <- list.files("taskfiles/raw/sepse", pattern = "kodi_txt", full.names = TRUE)

files_out <- list()

# Apstrāde ----------------------------------------------------------------
files_out <- map(paths, ~readLines(.))

files_out <- map(files_out, ~{strsplit(., ",") %>% unlist() %>% gsub(" ", "", .)})

files_out <- map(files_out, ~tibble(intervals = .))


# Saglabā -----------------------------------------------------------------
sheet_names <- gsub("taskfiles/raw/sepse/|_kodi_txt\\.txt", "", paths)
names(files_out) <- sheet_names

writexl::write_xlsx(files_out, path = "taskfiles/derived/sepses_kodi.xlsx")
