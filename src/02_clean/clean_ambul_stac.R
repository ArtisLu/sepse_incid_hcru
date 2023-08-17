# -------------------------------------------------------------------------
# Sagatavo ambulatoro epizožu (outpatient) datubāzi
# -------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# datu mape
data_path <- "data/raw/Stac_sepse"

# visi ielasāmie faili
files <- list.files(
  data_path,
  pattern = "ambul",
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

# salīdzina kolonnas
map(all_contents$table, colnames) %>% 
  map(length) %>% 
  unlist()

map(all_contents$table, colnames) %>% 
  map(~as.data.frame(t(as.matrix(.)))) %>% 
  bind_rows()

# noņem liekās
all_contents$table <- all_contents$table %>% 
  map(~{.x %>% select(-contains("AIK nosaukums"))})

# normalizē kolonnu nosaukumus
all_contents$table <- all_contents$table %>% 
  map(~{.x %>% janitor::clean_names()})

# nomaina vienu atšķirīgo kolonnu
col_names <- all_contents$table[[1]] %>% colnames()


all_contents$table <- all_contents$table %>% 
  map(
    ~{
      tmp <- .x
      colnames(tmp) <- col_names
      tmp
    }
  )

# apvieno vienā datu tabulā
ambul <- all_contents$table %>% bind_rows()

# nomaina kolonnu nosaukumus
old_colnames <- colnames(ambul)

new_colnames <- old_colnames %>% 
  gsub("^.*?_", "", .) %>% 
  gsub("^epi.*?_", "", .)

colnames(ambul) <- new_colnames

# pārbauda, cik ir trūkstošu id un pid
ambul %>% count(is.na(pid))
ambul %>% count(is.na(id))

# atmet atkārtojošās
ambul <- ambul %>% distinct(id, .keep_all = TRUE)

save(ambul, file = "data/proc/clean_ambul_stac.RData")

