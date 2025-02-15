library(here)
# library(knitr)
# library(danGoat)
library(tidyverse)
library(rmarkdown)
library(viridis)
library(plater)
library(ggbeeswarm)
library(janitor)
library(drc)
library(readxl)

source(here('scripts', 'functions.R')) 
#get file path
#also reflecting update to g_coordinate_analysis folder
file_path <- here("data_input", "mc_dropbox_data", "unzipped", "FLIM", "EICL-000US_000UA_2DGDCA_tech-reps.xlsx")
file_path2 <- here("data_input", "mc_dropbox_data", "unzipped", "FLIM", "EICL-000US-H2BGFP_CAF-CM_avg-biol-reps_v3.xlsx")
file_path3 <- here("data_input", "mc_dropbox_data", "unzipped", "FLIM", "EICL-000US-H2BGFP_CAF-CM_avg-biol-reps_v5.xlsx")
file_path4 <- here("data_input", "mc_dropbox_data", "unzipped", "FLIM", "EICL-000US-H2BGFP_CAF-CM_avg-biol-reps-STA_v6.xlsx")

trt_untrt <- read_flim_data(file_path)
crc_caf <- read_flim_data(file_path2)
crc_caf2 <- read_flim_data(file_path3)
crc_caf3 <- read_flim_data(file_path4) %>%
  mutate(compound = "STA")

crc_caf <- rbind(crc_caf, crc_caf2, crc_caf3) %>%
  distinct() %>%
  #drug did not work for 20240304 experiment
  # 0814 can be replaced, we wanted to redo the experiment because we suspected 3-bp drug was not affective that time. We confirmed the correct phasor distribution on 0311 data and remove 0814.
  filter(experiment_date != 20240304) %>%
  filter(experiment_date != 20230814)

saveRDS(crc_caf, here("data_output", "ua-us_flim_crc-caf.rds"))
saveRDS(trt_untrt, here("data_output", "ua-us_flim_trt-untrt.rds"))


