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

# source(here('scripts', 'file-paths.R'))
source(here('scripts', 'functions.R'))


files <- list.files(path = here("data_input", "mc_dropbox_data", "unzipped"), recursive = TRUE, full.names = TRUE)
files1 <- files[grepl("EICL-000UP-ORG|EICL-000US-ORG|organoid_data_from_TME", files)] |>
  #filter out elements without .xl pattern
  discard(function(x) !grepl(".xl", x)) |>
  #filter out files in the "old" folder
  discard(function(x) grepl("old", x))
  
raw_df0 <- files1 %>% map_df(xls3plater) %>%
  rename(plate_id = sheet_name) 

#import organoid data from the tme project
files1_tme <- files[grepl("organoid_data_from_TME", files)]
#elizabeth used a different xls2plater variant function
#so i just copied that into my functions script and an using that on her files
#my xls2plater variant (which I have called xls3plater because... i m stupid) doesn't work on her data
 
raw_df0_tme <- files1_tme %>% map_df(ctg2plater) %>%
  rename(wells = well,
         compound_dose = drug_concentration,
         compound_name = condition,
         exp_date = experiment_date,
         trt_duration = treatment_duration) %>%
  mutate(trt_duration = as.character(trt_duration),
         image_qc_corrected = as.numeric(image_qc_corrected))

raw_df <- bind_rows(raw_df0, raw_df0_tme) %>%
  #remove wells that had seeding issues
  #wells to remove: B3 from Dropbox (EITM)/Bioanalytics_Metabolic-Crosstalk/input/EICL-000US-ORG/20230731_EICL-000UP-CTO_3BP-5FU/20230731_EICL-000UP-CTO_5FU.xlsx
  #confirmed with Emma that this well had seeding issues
  filter(!(grepl("20230731_EICL-000UP-CTO_5FU", file_name) & wells == "B03")) %>%
  #wells to remove: G3 from 20230731_EICL-000US-CTO_3BP
  filter(!(grepl("20230731_EICL-000US-CTO_3BP", file_name) & wells == "G03")) %>%
  filter(!is.na(compound_name)) %>%
  filter(!is.na(compound_dose)) %>%
  mutate(row = substr(wells, 1, 1),
         column = substr(wells, 2, length(wells))) %>%
  group_by(compound_name, compound_dose, plate_id, file_name, file_path, drug_unit, exp_date) %>%
  mutate(var_in_condition = sd(signal, na.rm = TRUE),
         mean_in_condition = mean(signal, na.rm = TRUE),
         perc_cv = var_in_condition/mean_in_condition*100) %>%
  ungroup() %>%
  group_by(plate_id, row, file_name, file_path, exp_date) %>%
  mutate(ctrl_100_mean_row = mean(signal[compound_name %in% c("Untreated", "Media", "DMSO Media", 
                                                              "DMSO", "DMSO Media (Media changed on Day -2)")], na.rm = TRUE)) %>%
  ungroup(row) %>%
  #group by plate
  # group_by(plate_id, file_name, file_path, exp_date) %>%
  mutate(plate_cv = mean(perc_cv, na.rm = TRUE),
         #if there's more than one dose of STA, use the STA does
         sta_doses = length(unique(compound_dose[compound_name == "STA"])),
         # perc100_rows = nrow(compound_dose[compound_name %in% c("Untreated", "Low EGF")]),
         ctrl_100_mean_plate = mean(signal[compound_name %in% c("Untreated", "untreated", "Media", "DMSO Media", 
                                                                "DMSO", "DMSO Media (Media changed on Day -2)")], na.rm = TRUE),
         ctrl_100_sd = sd(signal[compound_name %in% c("Untreated", "untreated", "Media", "DMSO Media", 
                                                      "DMSO", "DMSO Media (Media changed on Day -2)")], na.rm = TRUE),
         ctrl_0_mean = mean(signal[compound_name == "STA"], na.rm = TRUE),
         ctrl_0_sd = sd(signal[compound_name == "STA"], na.rm = TRUE),
         z_prime_factor = 1 - (3*ctrl_100_sd + 3*ctrl_0_sd)/abs(ctrl_100_mean_plate - ctrl_0_mean)) %>%
  ungroup() %>%
  #filter plates without sta 
  # filter(sta_doses > 0) %>%
  #normalize by row posctrl WHERE POSSIBLE
  #otherwise use average
  mutate(rel_signal = ((signal - ctrl_0_mean)/(ctrl_100_mean_plate - ctrl_0_mean))*100) %>%
           # case_when(is.na(ctrl_100_mean_row) ~ ((signal - ctrl_0_mean)/(ctrl_100_mean_plate - ctrl_0_mean))*100,
           #                      is.nan(ctrl_100_mean_row) ~ ((signal - ctrl_0_mean)/(ctrl_100_mean_plate - ctrl_0_mean))*100,
           #                      ctrl_100_mean_row < 10000 ~ ((signal - ctrl_0_mean)/(ctrl_100_mean_plate - ctrl_0_mean))*100,
           #                      TRUE ~ ((signal - ctrl_0_mean)/(ctrl_100_mean_row - ctrl_0_mean))*100)) %>%
  mutate(compound_name = recode(compound_name,
                                'SN28' = 'SN38',
                                '3BP' = '3-BP',
                                '5FU' = '5-FU'),
         cell_line = recode(cell_line,
                            'DLD-1MUT' = 'DLD-1 MUT',
                            'DLD1-MUT' = 'DLD-1 MUT',
                            'DLD-1WT' = 'DLD-1 WT',
                            'DLD1-WT' = 'DLD-1 WT'),
         drug_unit = case_when(is.na(drug_unit) & compound_name == 'SN38' ~ 'uM',
                               is.na(drug_unit) & compound_name == 'CTX' ~ 'ug/mL',
                               .default = drug_unit),
         cell_line = recode(cell_line,
                            "EICL-000US CTO" = "US",
                            "EICL_000US" = "US",
                            "EITB_000US" = "US",
                            "EITB_000UK" = "UK",
                            "EICL_000UK" = "UK",
                            "EICL-000UP CTO" = "UP",
                            "EICL-000UP" = "UP",
                            "EICL_000UP" = "UP",
                            "EICL_000UA" = "UA"),
         compound_name = case_when(grepl("caf", file_name, ignore.case = TRUE) ~ paste(compound_name, "CAF-CM"),
                                   TRUE ~ compound_name)) %>%
  #filter out high outliers
  # filter(!(compound_dose > 10000 & rel_signal > 50)) %>%
  #get var_in_condition for relative signal
  group_by(compound_name, compound_dose, plate_id) %>%
  mutate(var_in_condition_rel = sd(rel_signal, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(cell_line)) %>%
  mutate(drug_unit = case_when(is.na(drug_unit) ~ "uM",
                               TRUE ~ drug_unit),
         #flag wells that had debris when image qc'ed
         #experiment "20230918_EICL-000US_CAF-CM_3BP" had debris in wells:
          #C4, D4, G8, G10
         well_flag = case_when(file_name == "20230918_EICL-000US_CAF-CM_3BP" & wells %in% c("C4", "D4", "G8", "G10") ~ "debris in well",
                               TRUE ~ "passed QC"))


#make a dataframe to group experiments and nest so you can make a drc for each row
drms0 <- raw_df %>%
  filter(well_flag == "passed QC") %>%
  filter(!(compound_name %in% c("Low EGF", "STA", "lowEGF", "Media", "PBS", "DMSO Media", "0", "Untreated", "untreated"))) %>%
  filter(!grepl("DMSO", compound_name, ignore.case = TRUE)) %>%
  filter(!is.na(cell_line)) %>%
  #this plate had no 100 control
  # filter(file_name != "20221007_DLD-1MUT_3BP_v2") %>%
  # filter(z_prime_factor > 0) %>%
  # filter(!(compound_name %in% c('STA', 'Untreated', 'blank', 'Blank', 'PBS', '0'))) %>%
  group_by(cell_line, compound_name, drug_unit, plate_id, exp_date, plate_cv, z_prime_factor, file_name, sta_doses, file_path) %>%
  nest() %>%
  mutate(condition = paste(compound_name, cell_line),
         n_doses = map_int(data, function(data) length(unique(data$compound_dose))),
         #can this experiment be modelled with a 4parameter log logistic dose response curve?
         doesItModel = map_lgl(data, function(data) getDRM(data)),
         #can this experiment be modelled with any number of parameter log logistic drc?
         doesItModel_ll = map_lgl(data, function(data) getDRM_ll(data)),
         plot_namer = paste(plate_id, compound_name, sep = ","))

#maybe SN38 can be modeled with 4param in organoids?
#it can't lol
sn38s <- drms0 %>%
  filter(grepl("SN38|5-FU CAF-CM", compound_name) & doesItModel_ll == TRUE & doesItModel == FALSE) %>%
  mutate(model = lapply(data, function(df1) drm(rel_signal ~ compound_dose, data = df1, fct = llogistic())),
         rse = vapply(model, function(model) summary(model)$rseMat[[1]], numeric(1)))
#make a dataframe for all those rows that don't converge with drc
doesnt_model <- drms0 %>%
  filter(doesItModel == FALSE | (compound_name == "SN38" & doesItModel_ll == FALSE)) %>%
  mutate(model = NULL,
         rse = NA,
         rel_ec50 = NA,
         rel_ec50_se = NA,
         relative_lower_ci = NA,
         relative_upper_ci = NA,
         min_dose = NA,
         extrapolated_ec50 = NA,
         max_response_ci_upper = NA,
         max_response_ci_lower = NA,
         hill = NA,
         bend1 = NA,
         bend2 = NA,
         upper_asymptote_doses = NA,
         lower_asymptote_doses = NA,
         lin_region_doses = NA,
         flag = "didn't fit model")
#make a dataframe for all the rows that *do* converge with drc
drms_that_fit <- drms0 %>%
  filter(doesItModel == TRUE) %>%
  #... and make a drc for each row
  mutate(model = lapply(data, function(df1) drm(rel_signal ~ compound_dose, data = df1, fct = LL.4(names = c("hill", "min_value", 'max_value', "ec_50")))),
         rse = vapply(model, function(model) summary(model)$rseMat[[1]], numeric(1)))
#add sn38 back to the dataframe that has all the other compounds
drms <- bind_rows(drms_that_fit, sn38s)
#get ec50s
abs_EDs <- lapply(drms$model,
                  function(model) ED(object = model, respLev = 50, type = "absolute", interval = "delta", display = FALSE))
rel_EDs <- lapply(drms$model,
                  function(model) ED(object = model, respLev = 50, interval = "delta", display = FALSE))
#add the relative effective doses to the dataframe of experiments that converged with drc
drms[c("rel_ec50", "rel_ec50_se",
       "relative_lower_ci", "relative_upper_ci")] <- do.call(rbind, rel_EDs)
drms[c("absolute_ec50", "absolute_ec50_se",
       "abs_lower_ci", "abs_upper_ci")] <- do.call(rbind, abs_EDs)
drms <- drms %>%
  ungroup() %>%
  mutate(min_dose = map(data, function(data) min(data$compound_dose)),
         max_dose = map(data, function(data) max(data$compound_dose)),
         extrapolated_ec50 = case_when(compound_name == "CTX" ~ "CTX",
                                       absolute_ec50 < min_dose ~ "extrapolated",
                                       absolute_ec50 > max_dose ~ "extrapolated",
                                       is.na(absolute_ec50) ~ "extrapolated",
                                       TRUE ~ ""),
         #get 95% conf ints for max model value
         max_response_ci_upper = map(model, function(model) as.numeric(confint(model)[3,2])),
         max_response_ci_lower = map(model, function(model) as.numeric(confint(model)[3,1])),
         #get bend points for each curve
         hill = map(model, function(model) as.numeric(model$coefficients[[1]])),
         einf = map(model, function(model) as.numeric(model$coefficients[[2]])),
         bend1 = rel_ec50*(4.6805^(1/as.numeric(hill))),
         bend2 = rel_ec50*((1/4.6805)^(1/as.numeric(hill))),
         upper_asymptote_doses = map2(data, bend2, function(data, bend2) nrow(unique(data[data$compound_dose < bend2, "compound_dose"]))),
         lower_asymptote_doses = map2(data, bend1, function(data, bend1) nrow(unique(data[data$compound_dose > bend1, "compound_dose"]))),
         lin_region_doses = pmap(list(bend1, bend2, data), function(bend1, bend2, data) nrow(unique(data[data$compound_dose > bend2 & data$compound_dose < bend1, "compound_dose"]))),
         #flag for qc issues
         flag = case_when(extrapolated_ec50 == "extrapolated" & compound_name != "CTX" ~ "extrapolated IC50",
                          TRUE ~ "passed QC"),
         flag = case_when(flag == "passed QC" & z_prime_factor < 0 ~ "Z' Factor < 0",
                          flag != "passed QC" & z_prime_factor < 0 ~ paste(flag, ", ", "Z' Factor < 0", sep = ""),
                          # flag != "passed QC" & z_prime_factor < 0 ~ paste(flag, ", ", "Z' Factor < 0", sep = ""),
                          TRUE ~ "passed QC"),
         flag = case_when(flag == "passed QC" & plate_cv >= 30 ~ "plate %CV >= 30",
                          flag != "passed QC" & plate_cv >= 30 ~ paste(flag, ", ", "plate %CV >= 30", sep = ""),
           # flag == "passed QC" & plate_cv >= 20 & cell_line != "US" ~ "plate %CV >= 20",
           #                flag != "passe d QC" & plate_cv >= 20 & cell_line != "US" ~ paste(flag, ", ", "plate %CV >= 20", sep = ""),
           #                flag == "passed QC" & plate_cv >= 30 & cell_line == "US" ~ "US + plate %CV >= 30",
           #                flag != "passed QC" & plate_cv >= 30 & cell_line == "US" ~ paste(flag, ", ", "US + plate %CV >= 30", sep = ""),
                          
                          TRUE ~ flag),
         flag = case_when(flag == "passed QC" & rse >= 30 ~ "RSE >= 30",
                          flag != "passed QC" & rse >= 30 ~ paste(flag, ", ", "RSE >= 30", sep = ""),
                          TRUE ~ flag),
         flag = case_when(flag == "passed QC" & sta_doses == 0 ~ "no 0% ctrl",
                          flag != "passed QC" & sta_doses == 0 ~ paste(flag, ", ", "no 0% ctrl", sep = ""),
                          TRUE ~ flag),
         # #add flag for when models' max values' 95% ci's are less than 55
         flag = case_when(
           # flag == "passed QC" & max_response_ci_lower > 150 ~ "max response > 150",
                          flag == "passed QC" & max_response_ci_upper < 55 ~ "max response < 55",
                          # flag != "passed QC" & max_response_ci_lower > 150 ~ paste(flag, ", max response > 150", sep = ""),
                          flag != "passed QC" & max_response_ci_upper < 55 ~ paste(flag, ", max response < 55", sep = ""),
                          TRUE ~ flag),
         flag = case_when(flag == "passed QC" & upper_asymptote_doses < 1 & !grepl("5-FU", compound_name) ~ "<1 dose in upper asymptote",
                          flag == "passed QC" & lower_asymptote_doses < 1 & !grepl("5-FU", compound_name) ~ "<1 dose in lower asymptote",
                          # flag == "passed QC" & lin_region_doses < 1 & compound_name != "SN38" ~ "<1 dose in linear region",
                          flag != "passed QC" & upper_asymptote_doses < 1 & !grepl("5-FU", compound_name) ~ paste(flag, ", <1 dose in upper asymptote", sep = ""),
                          flag != "passed QC" & lower_asymptote_doses < 1 & !grepl("5-FU", compound_name) ~ paste(flag, ", <1 dose in lower asymptote", sep = ""),
                          # flag != "passed QC" & lin_region_doses < 1 & compound_name != "SN38" ~ paste(flag, ", <1 dose in linear region", sep = ""),
                          TRUE ~ flag)) %>%
  dplyr::select(-extrapolated_ec50)

drms <- bind_rows(drms, doesnt_model) %>%
  mutate(flag = case_when(n_doses == 1 ~ "single dose experiment",
                          n_doses == 2 ~ "two dose experiment",
                          TRUE ~ flag))

tracker <- drms %>%
  dplyr::select(c(compound_name, cell_line, exp_date)) %>%
  arrange(compound_name, cell_line, exp_date)

write_csv(tracker, here("data_output", "tracker.csv"))

#summary statistics csv
summary_stats <- drms %>%
  ungroup() %>%
  dplyr::select(c(compound_name, cell_line, exp_date, plate_cv, z_prime_factor, rse, rel_ec50))

write_csv(summary_stats, here("data_output", "summary_stats.csv"))

saveRDS(drms, here("data_output", "organoids_all_drms.rds"))
saveRDS(raw_df, here("data_output", "organoids_all_raw_data.rds"))

#CTX US Test for effect
ctx_us <- drms %>%
  filter(compound_name == "CTX" & cell_line == "US") %>%
  filter(lengths(model) > 0)
noeffect_tests <- lapply(ctx_us$model, noEffect)

comp_drms <- drms %>%
  filter(flag == "passed QC") %>%
  unnest(cols = c(data)) %>%
  group_by(cell_line, compound_name, drug_unit) %>%
  nest() %>%
  # rowwise() %>%
  mutate(model = map2(compound_name, data, function(compound_name, data) get_comp_models(compound_name, data)),
         rse = vapply(model, function(model) summary(model)$rseMat[[1]], numeric(1)),
         rel_ec50 = map(model, function(model) as.numeric(ED(object = model, respLev = 50, interval = "delta", display = FALSE)[[1]])),
         hill = map(model, function(model) as.numeric(model$coefficients[[1]])),
         max_response = map(model, function(model) as.numeric(model$coefficients[[3]])),
         min_response = map(model, function(model) as.numeric(model$coefficients[[2]])),
         bend1 = as.numeric(rel_ec50)*(4.6805^(1/as.numeric(hill))),
         bend2 = as.numeric(rel_ec50)*((1/4.6805)^(1/as.numeric(hill))))

saveRDS(comp_drms, here("data_output", "organoids_comp_drms.rds"))
