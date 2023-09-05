# -------------------------------------------------------------------------
# Sagatavo kompensējamo medikamentu datubāzi
# -------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# datu mape
data_path <- "data/raw/Stac_sepse"

# visi ielasāmie faili
files <- list.files(
  data_path,
  pattern = "KM",
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

# normalizē kolonnu nosaukumus
all_contents$table <- all_contents$table %>% 
  map(~{.x %>% janitor::clean_names()})

# apvieno vienā datu tabulā
komp_med <- all_contents %>%
  unnest(table) %>% 
  select(-sheet)

# nomaina kolonnu nosaukumus
old_colnames <- colnames(komp_med)

new_colnames <- old_colnames %>% 
  gsub("^.*?_", "", .)

colnames(komp_med) <- new_colnames

komp_med <- komp_med %>% 
  rename(datums = atprecosanas_datums)

# pārbauda, cik ir trūkstošu id un pid
komp_med %>% count(is.na(pid))

komp_med %>% apply(2, function(x) sum(is.na(x)))

# atmet atkārtojošās

# vai katra faila ietvaros atkārtojas?
all_contents$table %>% 
  map(~{nrow(.x) == nrow(distinct(.x))})

all_contents$table %>% 
  map(~{paste0(nrow(.x), " -- ", nrow(distinct(.x)))})

# tmp1 <- komp_med %>% distinct(pid, datums, atk_kods, izsniegta_med_inr, izsniegtais_daudzums, maksatajs, valsts_summa, .keep_all = TRUE)
# 
# tmp2 <- tmp1 %>% 
#   add_count(pid, datums, atk_kods, izsniegta_med_inr, izsniegtais_daudzums, maksatajs)
# 
# tmp2 %>% count(n)
# 
# tmp2 %>% filter(n > 1) %>% View()

komp_med %>% distinct(.keep_all = TRUE)
komp_med <- komp_med %>% distinct(across(-file), .keep_all = TRUE)

save(komp_med, file = "data/02_cleaned/clean_komp_med_stac.RData")
