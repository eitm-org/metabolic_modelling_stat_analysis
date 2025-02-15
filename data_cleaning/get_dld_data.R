library(here)
library(renv)
library(knitr)
# library(danGoat)
library(tidyverse)
library(rmarkdown)
library(viridis)
library(plater)
library(ggbeeswarm)
library(janitor)
library(drc)

source(here('scripts', 'functions.R'))


files <- list.files(path = file.path(here("data_input", "mc_dropbox_data", "unzipped")), recursive = TRUE, full.names = TRUE)
files <- files[grepl("CSV-files|0522", files)]

raw_df0 <- files %>% map_df(xls3plater)
# print(range(raw_df0$signal, na.rm = TRUE))

raw_df <- raw_df0 %>%
  filter(!is.na(compound_name)) %>%
  filter(!is.na(compound_dose)) %>%
  mutate(row = substr(wells, 1, 1),
         column = substr(wells, 2, length(wells))) %>%
  group_by(compound_name, compound_dose, sheet_name, file_name, file_path, drug_unit, exp_date) %>%
  mutate(var_in_condition = sd(signal, na.rm = TRUE),
         mean_in_condition = mean(signal, na.rm = TRUE),
         perc_cv = var_in_condition/mean_in_condition*100) %>%
  ungroup() %>%
  group_by(sheet_name, row, file_name, file_path, exp_date) %>%
  mutate(pos_ctrl_mean_row = mean(signal[compound_dose == 0 & compound_name != "NA"], na.rm = TRUE)) %>%
  ungroup(row) %>%
  #group by plate
  # group_by(sheet_name, file_name, file_path, exp_date) %>%
  mutate(plate_cv = mean(perc_cv, na.rm = TRUE),
         #if there's more than one dose of STA, use the STA does
         sta_doses = length(unique(compound_dose[compound_name == "STA"])),
         # perc100_rows = nrow(compound_dose[compound_name %in% c("Untreated", "Low EGF")]),
         pos_ctrl_mean_plate = mean(signal[compound_dose == 0 & compound_name != "NA"], na.rm = TRUE),
         pos_ctrl_sd_plate = sd(signal[compound_dose == 0 & compound_name != "NA"], na.rm = TRUE),
         neg_ctrl_mean = case_when(sta_doses == 5 ~ mean(signal[compound_name == "STA" & compound_dose == 1], na.rm = TRUE),
                                   sta_doses == 2 ~ mean(signal[compound_name == "STA" & compound_dose == 3], na.rm = TRUE),
                                   TRUE ~ mean(signal[compound_name == "STA"], na.rm = TRUE)),
         neg_ctrl_sd = case_when(sta_doses == 5 ~ mean(signal[compound_name == "STA" & compound_dose == 1], na.rm = TRUE),
                                 sta_doses == 2 ~ sd(signal[compound_name == "STA" & compound_dose == 3], na.rm = TRUE),
                                 TRUE ~ sd(signal[compound_name == "STA"], na.rm = TRUE)),
         z_prime_factor = 1 - (3*pos_ctrl_sd_plate + 3*neg_ctrl_sd)/abs(pos_ctrl_mean_plate - neg_ctrl_mean)) %>%
  ungroup() %>%
  #filter plates without sta 
  filter(sta_doses > 0) %>%
  #normalize by row posctrl WHERE POSSIBLE
  #otherwise use average
  mutate(rel_signal = case_when(is.na(pos_ctrl_mean_row) ~ ((signal - neg_ctrl_mean)/(pos_ctrl_mean_plate - neg_ctrl_mean))*100,
                                is.nan(pos_ctrl_mean_row) ~ ((signal - neg_ctrl_mean)/(pos_ctrl_mean_plate - neg_ctrl_mean))*100,
                                pos_ctrl_mean_row < 10000 ~ ((signal - neg_ctrl_mean)/(pos_ctrl_mean_plate - neg_ctrl_mean))*100,
                                TRUE ~ ((signal - neg_ctrl_mean)/(pos_ctrl_mean_row - neg_ctrl_mean))*100)) %>%
  mutate(compound_name = recode(compound_name,
                                'SN28' = 'SN38',
                                '3BP' = '3-BP',
                                '5-FU' = '5FU'),
         cell_line = recode(cell_line,
                            'DLD-1MUT' = 'DLD-1 MUT',
                            'DLD1-MUT' = 'DLD-1 MUT',
                            'DLD-1WT' = 'DLD-1 WT',
                            'DLD1-WT' = 'DLD-1 WT'),
         drug_unit = case_when(is.na(drug_unit) & compound_name == 'SN38' ~ 'uM',
                               is.na(drug_unit) & compound_name == 'CTX' ~ 'ug/mL',
                               .default = drug_unit) )%>%
  #filter out high outliers
  filter(!(compound_dose > 10000 & rel_signal > 50)) %>%
  #get var_in_condition for relative signal
  group_by(compound_name, compound_dose, sheet_name) %>%
  mutate(var_in_condition_rel = sd(rel_signal, na.rm = TRUE)) %>%
  ungroup()

#make a dataframe to group experiments and nest so you can make a drc for each row
drms0 <- raw_df %>%
  filter(!(compound_name %in% c("Low EGF", "STA", "lowEGF", "Untreated", "blank", "Blank"))) %>%
  #this plate had no 100 control
  filter(file_name != "20221007_DLD-1MUT_3BP_v2") %>%
  # filter(z_prime_factor > 0) %>%
  group_by(cell_line, compound_name, drug_unit, sheet_name, exp_date, plate_cv, z_prime_factor, file_name) %>%
  nest() %>%
  mutate(condition = paste(compound_name, cell_line),
         n_doses = map_int(data, function(data) length(unique(data$compound_dose))),
         #can this experiment be modelled with a 4parameter log logistic dose response curve?
         doesItModel = map_lgl(data, function(data) getDRM(data)),
         #can this experiment be modelled with any number of parameter log logistic drc?
         doesItModel_ll = map_lgl(data, function(data) getDRM_ll(data)),
         plot_namer = paste(sheet_name, compound_name, sep = ","))
#make a dataframe for all those rows that don't converge with drc
doesnt_model <- drms0 %>%
  filter(compound_name == "SN38" & doesItModel_ll == TRUE) %>%
  mutate(model = lapply(data, function(df1) drm(rel_signal ~ compound_dose, data = df1, fct = llogistic())),
         rse = vapply(model, function(model) summary(model)$rseMat[[1]], numeric(1)))
#make a dataframe for all the rows that *do* converge with drc
drms1 <- drms0 %>%
  filter(doesItModel == TRUE & compound_name != "SN38") %>%
  #... and make a drc for each row
  mutate(model = lapply(data, function(df1) drm(rel_signal ~ compound_dose, data = df1, fct = LL.4(names = c("hill", "min_value", 'max_value', "ec_50")))),
         rse = vapply(model, function(model) summary(model)$rseMat[[1]], numeric(1)))
#add sn38 back to the dataframe that has all the other compounds
drms <- bind_rows(doesnt_model, drms1)
#get ec50s
rel_EDs <- lapply(drms$model,
                  function(model) ED(object = model, respLev = 50, interval = "delta", display = FALSE))
#add the relative effective doses to the dataframe of experiments that converged with drc
drms[c("rel_ec50", "rel_ec50_se",
       "relative_lower_ci", "relative_upper_ci")] <- do.call(rbind, rel_EDs)
#get other model parameters
coefficients <- lapply(drms$model, 
                       function(model) summary(model)$coefficients[2,1])
drms[c("einf")] <- do.call(rbind, coefficients)

#add absolute ic50s
abs_EDs <- lapply(drms$model, 
                  function(model) ED(object = model, respLev = 50, type = "absolute", interval = "delta", display = FALSE))
drms[c("absolute_ec50", "absolute_ec50_se", 
            "absolute_lower_ci", "absolute_upper_ci")] <- do.call(rbind, abs_EDs)
drms <- drms %>%
  ungroup() %>%
  mutate(min_dose = map(data, function(data) min(data$compound_dose)),
         extrapolated_ec50 = case_when(compound_name == "CTX" ~ "CTX",
                                       rel_ec50 < min_dose ~ "extrapolated",
                                       TRUE ~ ""),
         #get 95% conf ints for max model value
         max_response_ci_upper = map(model, function(model) as.numeric(confint(model)[3,2])),
         max_response_ci_lower = map(model, function(model) as.numeric(confint(model)[3,1])),
         #get bend points for each curve
         hill = map(model, function(model) as.numeric(model$coefficients[[1]])),
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
                          TRUE ~ flag),
         flag = case_when(flag == "passed QC" & plate_cv >= 21 ~ "plate %CV >= 21",
                          flag != "passed QC" & plate_cv >= 21 ~ paste(flag, ", ", "plate %CV >= 21", sep = ""),
                          TRUE ~ flag),
         # ,
         # #add flag for when models' max values' 95% ci's don't include 100
         # flag = case_when(flag == "passed QC" & max_response_ci_lower > 110 ~ "max response > 110",
         #                  flag == "passed QC" & max_response_ci_upper < 70 ~ "max response < 70",
         #                  flag != "passed QC" & max_response_ci_lower > 110 ~ paste(flag, ", max response > 110", sep = ""),
         #                  flag != "passed QC" & max_response_ci_upper < 70 ~ paste(flag, ", max response < 70", sep = ""),
         #                  TRUE ~ flag),
         flag = case_when(flag == "passed QC" & compound_name != "CTX" & upper_asymptote_doses < 1 ~ "<1 dose in upper asymptote",
                          flag == "passed QC" & compound_name != "CTX" & lower_asymptote_doses < 1 ~ "<1 dose in lower asymptote",
                          flag == "passed QC" & compound_name != "CTX" & lin_region_doses < 1 & compound_name != "SN38" ~ "<1 dose in linear region",
                          flag != "passed QC" & compound_name != "CTX" & upper_asymptote_doses < 1 ~ paste(flag, ", <1 dose in upper asymptote", sep = ""),
                          flag != "passed QC" & compound_name != "CTX" & lower_asymptote_doses < 1 ~ paste(flag, ", <1 dose in lower asymptote", sep = ""),
                          flag != "passed QC" & compound_name != "CTX" & lin_region_doses < 1 & compound_name != "SN38" ~ paste(flag, ", <1 dose in linear region", sep = ""),
                          TRUE ~ flag)) %>%
  dplyr::select(-extrapolated_ec50)


saveRDS(drms, here("data_output", "all_drms.rds"))
saveRDS(raw_df, here("data_output", "all_raw_data.rds"))

#get composite models
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

  

saveRDS(comp_drms, here("data_output", "comp_drms.rds"))
