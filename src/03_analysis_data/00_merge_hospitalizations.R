# -------------------------------------------------------------------------
# Apvieno blakus esošas hospitalizācijas epizodēs
# -------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)

# Dati --------------------------------------------------------------------
# visa datubāze
stac <- readRDS("data/proc/clean_stac.rds")


# Hospitalizāciju apvienošana ---------------------------------------------

glimpse(stac)
