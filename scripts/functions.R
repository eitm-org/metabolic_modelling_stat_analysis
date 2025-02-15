
redoSheetNames <- function(xls_path) {
  require(readxl)
  require(dplyr)
  sheets_names <- excel_sheets(path = xls_path)
  
  
}

xls3plater <- function(xls_path) {
  require(readxl)
  require(dplyr)
  require(janitor)
  # print(xls_path)
  #making this for the function to return if there"s no data read in
  return_null <- data.frame(file_name = character(0), file_path = character(0), wells = character(0), signal = numeric(0), compound_name = character(0),
                            rep = numeric(0), compound_dose = numeric(0), l_concentration = numeric(0),
                            activity = character(0), cell_line = character(0), sheet_name_id = character(0),
                            exp_date = numeric(0), passage_number = character(0), trt_duration = character(0),
                            sheet_name_reader = character(0), operator = character(0))
  
  #get a list of all sheets in this excel file
  all_sheet_names <- excel_sheets(path = xls_path)
  
  #read in the sheet2 metadata from our new temsheet_name
  #filter out sheet2"s
  if ("sheet2" %in% all_sheet_names) {
    sheet2 <- read_excel(xls_path, sheet = "sheet2", col_names = FALSE) %>%
      #i don"t remember what this line does lol
      # select_if(function(x) any(x != "separate with \",\"")) %>%
      filter(...1 %in% c("passage_number", "trt_duration", "sheet_name_reader", "operator"))
  } else {
  #removed this chunk because elizabeth's tme data has a sheet called metadata
    #that is formatted differently than mine
  # else if ("metadata" %in% all_sheet_names) {
  #   sheet2 <- read_excel(xls_path, sheet = "metadata", col_names = FALSE) %>%
  #     #i don"t remember what this line does lol
  #     # select_if(function(x) any(x != "separate with \",\"")) %>%
  #     filter(...1 %in% c("Experiment_Date", "Tech", "Hour", "Passage_numger_at_day_sheet_named"))
  # } 
    #this is a fun surprise for later!
    #i will assign it to a column later (if this input sheet didn"t use our sheet2 metadata temsheet_name)
    if (str_detect(xls_path, pattern = "HS|BH|AC|MH|GZ")) {
      if (grepl("HS", xls_path)) {
        operator_val <- "HS"
      } else if (grepl("BH", xls_path)) {
        operator_val <- "BH"
      } else if (grepl("AC", xls_path)) {
        operator_val <- "AC"
      } else if (grepl("GZ", xls_path)) {
        operator_val <- "GZ"
      } else {
        operator_val <- NA
      }
    } else {
      operator_val <- NA
    }
  } 
  filtered_sheet_names <- all_sheet_names[!(all_sheet_names %in% 
                                              c("Measurements", "Formatting Parameters", "Sheet1", "Sheet2", "sheet2", "metadata", "template dictionary", "plate template"))]
  filtered_sheet_names <- filtered_sheet_names[!grepl("raw", filtered_sheet_names, ignore.case = TRUE)]
  
  if (length(filtered_sheet_names) == 0) {
    return(return_null)
  }
  #I try not to use for loops in r but this is a rare case where I think it works better
  #especially because we usually only have one sheet with data on it for this project
  formatted_dfs <- c()
  for (i in 1:length(filtered_sheet_names)) {
    # print(filtered_sheet_names[i])
    #read your plater formatted excel sheet
    df <- read_excel(xls_path, sheet = filtered_sheet_names[i], range = cell_cols("A:M"))
    #some of the tme data has stuff outside the columns
    # df <- df[,1:13]
    # #some of the signal things say omit
    # df <- data.frame(lapply(df, function(x) gsub("Omit", NA, x)))

    #filter out files that have the biotech raw data format
    if (grepl("Experiment File Path:", df[2,1])) {
      next
    }
    #get the name of where you want to output this df to a csv file
    #check if the (here("data_input", "csvs") directory exists
    if(!dir.exists(here("data_input", "csvs"))) {
      #check and see if it exists again
      if (!dir.exists(here("data_input", "csvs"))) {
        #and if it doesn't, create it
        dir.create(here("data_input", "csvs"))
      }
    }
    local_csv_file_name <- here("data_input", "csvs", paste(filtered_sheet_names[i], ".csv", sep = ""))
    #write your csv file
    write_csv(df, file = local_csv_file_name)
    #check that it"s in proper plater format
    check_plater_format(file = local_csv_file_name)
    #read in the sheet / sheet_name using read_plate()
    formatted_df <- read_plate(local_csv_file_name)
    #run clean_names() on the dataframe so that it looks all nice
    formatted_df <- clean_names(formatted_df)
    formatted_df['file_path'] <- xls_path
    split_filename <- strsplit(xls_path, "/")
    xl_name <- split_filename[[1]][length(split_filename[[1]])]
    formatted_df['file_name'] <- substr(xl_name, 1, unlist(gregexpr(".xl", xl_name)) - 1)
    #standardize compound_name/compound_name/compound column name between old and new data
    if (length(grep("condition", names(formatted_df))) > 0) {
      #^this length statement is the best way I"ve found to figure out if we have a column called "x" in our dataframe
      names(formatted_df)[grep("condition", names(formatted_df))] <- "compound_name"
    } else {
      print("oh god no what column tells u the drugname in ")
      print(xls_path)
      print(filtered_sheet_names[i])
      print("the colnames are:")
      print(names(formatted_df))
      break
    }
    #add a column for experiment_id, which will be the name of your sheet_name
    #should i rename this "sheet_name"? it would be more descriptive, sheet_name id sounds good but it's kind of vague
    formatted_df["sheet_name"] <- filtered_sheet_names[i]
    #^sometimes people fill out their sheets and forget to change the sheet name
    #you might need to add the excel file name as a variable 
    #... and this would be the place to do that
    #another fun thing lab people do is forget to change the date within the excel sheet
    #so here I am taking the date from the excel sheet title
    db_file_name_split <- str_split(xls_path, "/")[[1]]
    #would love to make this a date variable in hree so we don"t have to do it at the end but 
    #I couldn"t figure it out in time
    #if exp_date is not a variable in the excel sheet, grab it from the file title
    if (length(grep("exp_date", names(formatted_df))) == 0) {
      #grab only the numbers from the beginning of the excel file title
      exp_date_val0 <- gsub("([0-9]+).*$", "\\1", db_file_name_split[[length(db_file_name_split)]])
      #and strip out the characters for the weird cases where there were characters before the numbers
      exp_date_val <- gsub("[^[:digit:], ]", "", exp_date_val0)
      formatted_df["exp_date"] <- exp_date_val
    } else {
      #make sure that if there are any underscores in the date, they are removed
      formatted_df$exp_date <- gsub("_", "", formatted_df$exp_date)
    }
    formatted_df$exp_date <- as.numeric(formatted_df$exp_date)
    #the old excel files have a column called rep that isn"t in the new excel files
    #i am setting it to NA for the new files just in case this is something we decide to add back in the future
    if (length(grep("rep", names(formatted_df))) == 0) {
      formatted_df["rep"] <- NA
    }
    #doing the same for "activity"
    #i am not even sure what that column was being used for
    if (length(grep("activity", names(formatted_df))) == 0) {
      formatted_df["activity"] <- NA
    }
    #older files call the compound_name concentration d_concentration, but new ones call it drug_u_m
    if (length(grep("concentration", names(formatted_df))) > 0) {
      names(formatted_df)[grep("concentration", names(formatted_df))] <- "compound_dose"
    } else if (length(grep("drug_u_m", names(formatted_df))) > 0) {
      formatted_df <- formatted_df %>% rename(compound_dose = drug_u_m)
    } else if (length(grep("drug_", names(formatted_df))) == 0) {
      
      print("oh my god what is the compound_name column name for ")
      print(xls_name)
      print(filtered_sheet_names[i])
      print("?????")
      print("cuz it's not any of the normal ones")
      break
    }
    #make sure signal is a numeric variable
    if(!(is.numeric(formatted_df$signal))) {
      formatted_df$signal <- as.numeric(formatted_df$signal)
    }
    
    #make sure compound_dose is a numeric variable
    if (length(grep("compound_dose", names(formatted_df))) > 0) {
      if (!(is.numeric(formatted_df$compound_dose))) {
      formatted_df$compound_dose <- as.numeric(formatted_df$compound_dose)
      }
    }
    if (!(is.character(formatted_df$wells))) {
      formatted_df$wells <- as.character(formatted_df$wells)
    }
    #older files call the ligand concentration l_concentration
    #newer ones call it r1881_pm
    #standardizing that here
    if (length(grep("r1881_p_m", names(formatted_df))) > 0) {
      formatted_df <- formatted_df %>% rename(l_concentration = r1881_p_m)
    }
    if (length(grep("l_concentration", names(formatted_df))) > 0) {
      if (!(is.numeric(formatted_df$l_concentration))) {
        formatted_df$l_concentration <- as.numeric(formatted_df$l_concentration)
      }
    }
    if (length(grep("imamge_qc_corrected", names(formatted_df))) > 0) {
      formatted_df <- formatted_df %>%
        rename(image_qc_corrected = imamge_qc_corrected)
    }
    if (length(grep("^image_qc_corrected", names(formatted_df))) > 0) {
      if (!(is.numeric(formatted_df$image_qc_corrected))) {
        formatted_df$image_qc_corrected <- as.numeric(formatted_df$image_qc_corrected)
      }
    }
    
    formatted_dfs[[i]] <- formatted_df
  }
  return_df <- bind_rows(formatted_dfs)
  #add sheet2 data
  #if this input sheet uses the special sheet2 format
  # metadata_cols <- c("passage_number", "trt_duration", "sheet_name_reader")
  if (exists("sheet2")) {
    if (nrow(sheet2) > 0) {
      for (j in nrow(sheet2)) {
        #get the value to the right of the title
        val <- sheet2[j, ncol(sheet2)][[1]]
        name <- sheet2[i,1][[1]]
        return_df[name] <- val
      }
    } else {
      # for (i in 1:length(metadata_cols)) {
      #   return_df[metadata_cols[i]] <- NA
      # }
      return_df["passage_number"] <- "NA"
      return_df["trt_duration"] <- "NA"
      if (length(grep("reader", names(return_df))) > 0 ) {
        return_df <- return_df %>% rename(sheet_name_reader = reader)
      } else {
        return_df["sheet_name_reader"] <- "NA"
      }
      return_df["operator"] <- operator_val
    }
    rm(sheet2)
  } else {
    return_df["passage_number"] <- "NA"
    return_df["trt_duration"] <- "NA"
    if (length(grep("reader", names(return_df))) > 0 ) {
      return_df <- return_df %>% rename(sheet_name_reader = reader)
    } else {
      return_df["sheet_name_reader"] <- "NA"
    }
    return_df["operator"] <- operator_val
  }
  if(nrow(return_df) == 0) {return(return_null)} else  {return(return_df)}
}

xls2plater <- function(xls_path) {
  
  # #grab only the numbers from the beginning of the excel file title
  # exp_date_val0 <- gsub("([0-9]+).*$", "\\1", db_file_name_split[[length(db_file_name_split)]])
  # #and strip out the characters for the weird cases where there were characters before the numbers
  # exp_date_val <- gsub("[^[:digit:], ]", "", exp_date_val0)
  
  
  
  require(readxl)
  require(dplyr)
  sheets_names <- excel_sheets(path = xls_path)
  all_sheets <- lapply(sheets_names, function(x) read_excel(xls_path, sheet = x))
  names(all_sheets) <- sheets_names
  sheet_name_names <- names(all_sheets[names(all_sheets) %in% c("Formatting Parameters", "Sheet1", "Sheet2", "sheet2", "metadata") == FALSE])
  
  csv_names <- lapply(sheet_name_names, function(x) here("data_input", "csvs", paste(x, ".csv")))
  
  sapply(
    sheet_name_names,
    function(x) write_csv(all_sheets[[x]], file = here("data_input", "csvs", paste(x, ".csv")))
  )
  
  # read in all the csv that were just written using plater
  sapply(sheet_name_names, function(x) check_plater_format(file = here("data_input", "csvs", paste(x, ".csv"))))
  
  # print(xls_path)
  sheet_name_df <- read_plates(csv_names, well_ids_column = "Wells")
  
  if (is.numeric(sheet_name_df$compound_name)) {
    sheet_name_df <- sheet_name_df %>%
      mutate(compound_name = as.character(compound_name))
  }
  if (!is.numeric(sheet_name_df$compound_dose)) {
    sheet_name_df <- sheet_name_df %>%
      mutate(compound_dose = as.numeric(compound_dose))
  }
  
  # for (i in 1:length(sheet_name_names)) {
  #   sheet_name_df[[i]]$sheet_name_ID <- sheet_name_names[[i]]
  #   }
  
  # convert list to large dataframe
  # bound_data <- tibble()
  # for (psample in sheet_name_names) {
  #   temp <- bind_rows(sheet_name_df[c(1:10), psample])
  #   bound_data <- bind_rows(bound_data, temp)
  # }
  return(sheet_name_df)
}

csv2plater <- function(file_path) {
  df <- read_plate(file_path)
  df["file_path"] <- file_path
  split <- str_split(file_path, "/")[[1]]
  file_name <- str_remove(split[[length(split)]], ".csv")
  df["file_name"] <- file_name
  df["exp_date"] <- str_extract(file_name, "[[:digit:]]{8}")
  return(df)
}


plot_diagnostic <- function(experiment_df) {
  require(here)
  require(viridis)
  
  G <- ggplot(experiment_df, aes(x = compound_dose,
                                 y = signal,
                                 col = compound_name))
  
  print(G + 
          geom_jitter() + 
          scale_x_log10() + 
          labs(title = "All sheet_names Combined - raw data") +
          danGoat::scale_color_ei()
  )
  print(G +
          geom_jitter() +
          scale_x_log10() +
          facet_wrap(~sheet_name) +
          labs(title = "Each sheet_name - raw data") +
          danGoat::scale_color_ei()
  )

  destination <- here("figures")

}


#this function takes in a row from the drm dataframe
#this is a nested dataframe with a column called data and another column called model
get_single_drug_model <- function(row) {
  
  compound_name <- row$compound_name
  cell <- row$cell_line
  inputDF <- row$data
  unit <- row$drug_unit
  this_drm <- row$model
  # summy <- summary(drm)
  rse <- row$rse
  plate_cv <- row$plate_cv
  file_name <- row$file_name

  #using 20 points to get preds so we can have a smoother prediction line
  # doseStart <- min(inputDF$compound_dose)
  # if (!is.finite(doseStart)) {
  #   doseStart <- min(inputDF[inputDF$DrugConcentration > min(inputDF$compound_dose),]$compound_dose)
  # }
  predoses <- data.frame(dose = seq(from = min(inputDF$compound_dose), to = max(inputDF$compound_dose), length.out = 100))
  predoses['preds'] <- predict(this_drm, data.frame(predoses))
  ec50 <- this_drm$coefficients[[length(this_drm$coefficients)]]
  
  #get bendpoints
  #hX = hill slope for compound_name X
  h <- this_drm$coefficients[[1]]
  
  k <- 4.6805
  bend1 <- ec50*(k^(1/h))
  bend2 <- ec50*((1/k)^(1/h))
  
  #oh my god i'm not even using this line in this function....
  #TO DO: change this function so it uses this instead of predoses
  curve_data <- PR(this_drm, xVec = seq(min(inputDF$compound_dose), max(inputDF$compound_dose), .1))
  
  curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
  
  #l2 sets the top value at 1, so i am multiplying y by 100 here
  if (length(this_drm$coefficients) == 2) {
    curve.data <- curve.data %>%
      mutate(y = y*100)
  }
  
  top = max(inputDF$rel_signal) + 50
  bottom = min(min(inputDF$rel_signal) - 10, 0)
  d1Plot <- ggplot() +
    geom_line(data = curve.data, aes(x = x, y = y)) +
    geom_point(data = inputDF, aes(x = compound_dose, y = rel_signal)) +
    scale_x_log10(labels = as.character(sort(unique(inputDF$compound_dose))), breaks = sort(unique(inputDF$compound_dose))) +
    theme_bw() +
    ylim(c(bottom, top)) +
    labs(subtitle = file_name) +
    scale_color_viridis(discrete=TRUE) +
    geom_vline(xintercept = c(bend1, bend2), color= 'darkgray') +
    geom_vline(xintercept = ec50, color = 'gray') +
    geom_label(aes(x = ec50, y = top, label = 'EC50')) +
    ggtitle(paste(cell, compound_name, sep = '')) +
    # theme(legend.position = "none") +
    ylab('% Response') +
    # ylim(c(-50, 150)) +
    xlab(paste(compound_name, unit, sep = '  ')) +
    geom_label(aes(x = median(inputDF$compound_dose), y = 100, label = paste('RSE = ', round(rse, 2), '\nPlate %CV = ', round(plate_cv, 2), sep = '')))

  return(d1Plot)
}

get_single_drug_model_no_effect <- function(row) {
  
  compound_name <- row$compound_name
  cell <- row$cell_line
  inputDF <- row$data
  unit <- row$drug_unit
  this_drm <- row$model
  # summy <- summary(drm)
  rse <- row$rse
  plate_cv <- row$plate_cv
  file_name <- row$file_name
  no_effect <- row$no_effect_test_adjusted
  date <- row$exp_date
  qc_flag <- row$flag
  
  #using 20 points to get preds so we can have a smoother prediction line
  # doseStart <- min(inputDF$compound_dose)
  # if (!is.finite(doseStart)) {
  #   doseStart <- min(inputDF[inputDF$DrugConcentration > min(inputDF$compound_dose),]$compound_dose)
  # }
  predoses <- data.frame(dose = seq(from = min(inputDF$compound_dose), to = max(inputDF$compound_dose), length.out = 100))
  predoses['preds'] <- predict(this_drm, data.frame(predoses))
  ec50 <- this_drm$coefficients[[length(this_drm$coefficients)]]
  
  #get bendpoints
  #hX = hill slope for compound_name X
  h <- this_drm$coefficients[[1]]
  
  k <- 4.6805
  bend1 <- ec50*(k^(1/h))
  bend2 <- ec50*((1/k)^(1/h))
  
  #oh my god i'm not even using this line in this function....
  #TO DO: change this function so it uses this instead of predoses
  curve_data <- PR(this_drm, xVec = seq(min(inputDF$compound_dose), max(inputDF$compound_dose), .1))
  
  curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
  
  #l2 sets the top value at 1, so i am multiplying y by 100 here
  if (length(this_drm$coefficients) == 2) {
    curve.data <- curve.data %>%
      mutate(y = y*100)
  }
  sub_title <- paste("No effect test:", round(no_effect, 4), "\n", qc_flag)
  
  if (no_effect > .05) {
    color <- "black"
  } else {
    color <- "blue"
  }
  
  top = max(inputDF$rel_signal) + 50
  bottom = min(min(inputDF$rel_signal) - 10, 0)
  d1Plot <- ggplot() +
    geom_line(data = curve.data, aes(x = x, y = y), color = color) +
    geom_point(data = inputDF, aes(x = compound_dose, y = rel_signal), color = color) +
    scale_x_log10(labels = as.character(sort(unique(inputDF$compound_dose))), breaks = sort(unique(inputDF$compound_dose))) +
    theme_bw() +
    ylim(c(bottom, top)) +
    labs(subtitle = sub_title) +
    scale_color_viridis(discrete=TRUE) +
    geom_vline(xintercept = c(bend1, bend2), color= 'darkgray') +
    geom_vline(xintercept = ec50, color = 'gray') +
    geom_label(aes(x = ec50, y = top, label = 'EC50')) +
    ggtitle(paste(cell, compound_name, date)) +
    # theme(legend.position = "none") +
    ylab('% Response') +
    # ylim(c(-50, 150)) +
    xlab(paste(compound_name, unit, sep = '  ')) +
    geom_label(aes(x = median(inputDF$compound_dose), y = 50, label = paste('RSE = ', round(rse, 2), '\nPlate %CV = ', round(plate_cv, 2), sep = '')))
  
  return(d1Plot)
}

get_non_drc_plot <- function(row) {
  
  compound_name <- row$compound_name
  cell <- row$cell_line
  inputDF <- row$data
  unit <- row$drug_unit
  file_name <- row$file_name
  plate_cv <- row$plate_cv
  
  top = max(inputDF$rel_signal) + 10
  bottom = min(min(inputDF$rel_signal) - 10, 0)
    # 
  d1Plot <- ggplot(data = inputDF, aes(x = compound_dose, y = rel_signal)) +
    geom_point() +
    geom_smooth(method = "lm", color = "black", linewidth = .5, se = FALSE) +
    scale_x_log10() +
    theme_bw() +
    ylim(c(bottom, top)) +
    labs(subtitle = file_name) +
    scale_color_viridis(discrete=TRUE) +
    ggtitle(paste(cell, compound_name, sep = '')) +
    # theme(legend.position = "none") +
    ylab('% Response') +
    xlab(paste(compound_name, unit, sep = '  '))
  # +
  #   geom_label(aes(x = median(inputDF$compound_dose), y = -5, label = paste('Plate %CV = ', round(plate_cv, 2), sep = '')))
  # 
    # geom_label(aes(x = min(inputDF$compound_dose) + 20, y = bottom + 5, label = paste('RSE = ', round(summy$rseMat[[1]], 2), sep = '')))
  
  # +
  #   geom_label(aes(x = bend1_d1 - 1.5, y = 80, label = 'Upper Bend Point'), hjust = 0) +
  #   geom_label(aes(x = bend2_d1 + .19, y = 50, label= 'Lower Bend Point'), hjust = 1)
  
  
  return(d1Plot)
}


getDRM <- function(.x) {
  
  tryCatch({
    drm(rel_signal ~ compound_dose, data = .x, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getDRM_fb <- function(.x) {
  
  tryCatch({
    drm(fraction_bound ~ concentration_u_m, data = .x, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getDRM_g <- function(.x) {
  
  tryCatch({
    drm(g_coordinate ~ concentration_u_m, data = .x, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getDRM_fb_ll <- function(.x) {
  
  tryCatch({
    drm(fraction_bound ~ concentration_u_m, data = .x, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}


getDRM_ll <- function(.x) {
  
  tryCatch({
    drm(rel_signal ~ compound_dose, data = .x, fct = llogistic())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getDRM_bc <- function(.x) {
  
  tryCatch({
    drm(rel_signal ~ compound_dose, data = .x, fct = braincousens(fixed = c(NA, NA, NA, NA, NA),
                                                                  names = c("b", "c", "d", "e", "f"),
                                                                  method = c("1", "2", "3", "4"), ssfct = NULL))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getDRM_bi <- function(.x) {
  
  tryCatch({
    drm(rel_signal ~ compound_dose, data = .x, fct = twophase())
    return(TRUE)
  }, error = function(e) {
    print(e)
    return(FALSE)
  })
}

getDRM_wb <- function(.x) {
  
  tryCatch({
    drm(rel_signal ~ compound_dose, data = .x, fct = weibull1())
    return(TRUE)
  }, error = function(e) {
    print(e)
    return(FALSE)
  })
}





get_ed_bc <- function(.x) {
  
  tryCatch({
    ED(object = .x, respLev = 50, display = FALSE)
    return(TRUE)
  }, error = function(e) {
    print(e)
    return(FALSE)
  })
}


pretty_heatmap <- function(df, scale = "none", save_plot = FALSE) {
  require(pheatmap)
  require(dplyr)
  
  the_matrix <- df %>%
    dplyr::select(row, column, signal, sheet_name) %>%
    arrange(column) %>%
    pivot_wider(names_from = column, values_from = signal) %>%
    dplyr::select(-sheet_name) %>%
    arrange(row) %>%
    column_to_rownames(var = "row") %>%
    as.matrix()
  
  if (save_plot == TRUE) {
    the_pheat <- pheatmap(the_matrix, scale = scale, cluster_rows = FALSE, cluster_cols = FALSE, main = paste("Day ", df$day[1], ", ", df$plate[1], sep = ""),
                          filename = here( "/outputs/plots/", Sys.Date(), "_", df$experiment_date[1], "_heatmap.tiff")
    )
  } else if (save_plot == FALSE) {
    the_pheat <- pheatmap(the_matrix, scale = scale, cluster_rows = FALSE, 
                          cluster_cols = FALSE, 
                          main = paste(df$exp_date[1], "\n", df$sheet_name[1], sep = ""))
  }
  return(the_pheat)
}


#this function is used within the positionEffects function
#to return a plot of row vs live Signal 
rowPlot <- function(drugName, acrossRowRepDF) {
  thisDrug <- acrossRowRepDF %>%
    filter(compound_name == drugName)
  
  rowGraph <- ggplot(data = thisDrug, aes(x = row_plate, y = signal)) +
    geom_boxplot() +
    geom_quasirandom() +
    ylim(c(min(acrossRowRepDF$signal) - 100, max(acrossRowRepDF$signal) + 100)) +
    ylab("Signal") +
    xlab("row") +
    ggtitle(drugName) +
    theme_bw() +
    theme(text = element_text(family = "serif"))
  return(rowGraph)
}
rowPlot_noplate <- function(drugName, acrossRowRepDF) {
  thisDrug <- acrossRowRepDF %>%
    filter(compound_name == drugName)
  
  rowGraph <- ggplot(data = thisDrug, aes(x = row, y = signal)) +
    geom_boxplot() +
    geom_quasirandom() +
    ylim(c(min(acrossRowRepDF$signal) - 100, max(acrossRowRepDF$signal) + 100)) +
    ylab("Signal") +
    xlab("row") +
    ggtitle(drugName) +
    theme(text = element_text(family = "serif"))
  return(rowGraph)
}


#this function is used within the positionEffects function
#to return a plot of col vs live Signal 
colPlot <- function(drugName, acrossColRepDF) {
  thisDrug <- acrossColRepDF %>%
    filter(compound_name == drugName)
  
  colGraph <- ggplot(data = thisDrug, aes(x = col_plate, y = signal)) +
    geom_boxplot() +
    geom_quasirandom() +
    ylim(c(min(acrossColRepDF$signal) - 100, max(acrossColRepDF$signal) + 100)) +
    ylab("Signal") +
    xlab("column") +
    theme_bw() +
    ggtitle(drugName) +
    theme(text = element_text(family = "serif"))
  
  return(colGraph)
}
colPlot_noplate <- function(drugName, acrossColRepDF) {
  thisDrug <- acrossColRepDF %>%
    filter(compound_name == drugName)
  
  colGraph <- ggplot(data = thisDrug, aes(x = col, y = signal)) +
    geom_boxplot() +
    geom_quasirandom() +
    ylim(c(min(acrossColRepDF$signal) - 100, max(acrossColRepDF$signal) + 100)) +
    ylab("Signal") +
    xlab("column") +
    ggtitle(drugName) +
    theme(text = element_text(family = "serif"))
  return(colGraph)
}

#this function is used within the positionEffects function
#to return a plot of near media vs live Signal 
mediaPlot <- function(drugName, acrossMedRepDF, bigdf) {
  thisDrug <- acrossMedRepDF %>%
    filter(compound_name == drugName)
  
  medModel <- lm(signal ~ near_ctrl0, data = thisDrug)
  summarY <- summary(medModel)
  rsq <- summarY$r.squared
  
  medGraph <- ggplot(data = thisDrug, aes(x = near_ctrl0, y = signal)) +
    geom_point() +
    theme_bw() +
    scale_color_viridis() +
    geom_smooth(method = "lm", color = "black") +
    ylim(c(min(acrossMedRepDF$signal) - 100, max(acrossMedRepDF$signal) + 100)) +
    ylab("Signal") +
    xlab("Near Media Wells") +
    ggtitle(drugName) +
    annotate(geom = "text", x = (max(thisDrug$near_ctrl0)/4 + min(thisDrug$near_ctrl0)), 
             hjust = 0, y = min(bigdf$signal), label = paste("R^2: ", round(rsq, 4), sep = "")) +
    theme(text = element_text(family = "serif"))
  return(medGraph)
}

surroundPlot <- function(drugName, bigdf) {
  thisDrug <- bigdf %>%
    filter(compound_name == drugName)
  

  linmod <- lm(signal ~ surroundCount, data = thisDrug)
  lm_summary <- summary(linmod)
  rsq <- lm_summary$r.squared

  sGraph <- ggplot(data = thisDrug, aes(x = surroundCount, y = signal)) +
    geom_smooth(method = "lm", color = "black") +
    theme_bw() +
    # geom_point(data = bigdf, aes(col = compound_name)) +
    geom_point(data = thisDrug, aes(col = plate)) +
    # scale_color_paletteer_d("trekcolors::lcars_cardassian") +
    ggtitle(paste("Surrounding Wells vs. Signal for\n", drugName)) +
    xlab("Sum of the Signals of the Surrounding Wells") +
    ylab("Signal for the Middle Well") +
    theme(text = element_text(family = "serif")) +
    annotate("text", x = min(thisDrug$surroundCount), y = max(thisDrug$signal), 
             label = paste("Rsq =", round(rsq, 4)), hjust = 0,
             family = "serif")
}
#this function takes day x dataframe as input
#will output all position effects
#run an anova test for each compound_name condition to determine whether there is a difference in
#plate, row, col Signal

#ctrl0 should be the string we use to detect the wells with the highest signal
        #or the wells that have the most potential for bleedthrough
positionEffects <- function(df,
                            ctrl0 = "media|untreated") {
  require(gridExtra)
  
  #data cleaning
  bigdf <- df %>%
    mutate(col_plate = paste(plate, column, sep = " "),
           row_plate = paste(plate, row, sep = " ")) %>%
    #group by plate and column
    group_by(plate, column) %>%
    #sort by row
    arrange(row) %>%
    #get the Signals for the cells below and above each well
    #group_item corresponds to the row within this plate+column
    mutate(group_item = row_number(),
           #above gives you the compound_name in the df row above this one
              #so also the row above this one on the plate
           above = lag(compound_name),
           #above gives you the signal for the well one row above this well on the plate
           aboveCount = lag(signal),
           #below gives you the compound_name in the well above this one on the plate
           below = lead(compound_name),
           #belowCount gives you the signal in the well one row above this one on the plate
           belowCount = lead(signal)) %>%
    ungroup() %>%
    #basically do it the same again but looking at right/left instead of up down
    #so start by grouping by plate and row
    group_by(plate, row) %>%
    #and arrange by column so when we descend through the dataframe, we're also moving left-to-right on the plate
    arrange(column) %>%
    #get the Signals for the cells to the right and left of each well
    #left gives you the drug in the well to the left of this one
    mutate(left = lag(compound_name),
           #leftCount gives you the signal in the well to the left of this one
           leftCount = lag(signal),
           #right gives you the drug in the well to the right of this one
           right = lead(compound_name),
           #and guess what this one is that's right it's the signal in the well to the right of this one
           rightCount = lead(signal)) %>%
    ungroup() %>%
    rowwise() %>%
    #count up the surrounding Signals
    mutate(surroundCount = sum(aboveCount, belowCount, leftCount, rightCount, na.rm = TRUE),
           all_around = paste(above, below, left, right, sep = " "),
           #near media counts up how many media wells are around each well
           near_ctrl0 = str_count(all_around, ctrl0)) %>%
    arrange(compound_dose)
  
  #make "plate position helper" df
  #grouped by compound_name  with columns to designate well plate location
  plate_pos_helper <- bigdf %>%
    mutate(column = paste(plate, column, sep = " "),
           row = paste(plate, row, sep = " ")) %>%
    group_by(compound_name) %>%
    nest()
  
  #make dataframe to calculate p values for plate, row, surrounding Signal
  ps <- data.frame(matrix(ncol = 6, nrow = 0))
  names(ps) <- c("compound_name", "platepval", "rowpval", "colpval",  "surroundCountP", "mediapval")
  
  #make a posthoc list
    #if the anova tests yield a significant p-value, 
      #this function will automatically add tukeys post hoc tests and individual box plots
  plate_posthoc_tests <- c()
  plate_posthoc_bps <- c()
  
  #loop through each compound_name condition and conduct anova tests to compare row, col, surrounding Signal
  for (row in 1:nrow(plate_pos_helper)) {
    df <- plate_pos_helper[row, "data"][[1]][[1]]
    compound_name <- as.character(plate_pos_helper$compound_name[[row]])
    
    #test if there is a difference between rows
    #(anova)
    if (length(unique(df$row_plate)) == length(unique(df$plate)) | (length(unique(df$row_plate)) < 2)) {
      rowpval <- NA
      rowadj <- NA
    } else {
      anovaT <- aov(signal ~ row_plate, data = df)
      summarY <- summary(anovaT)
      rowpval <- as.numeric(summarY[[1]][1,5])
      if (length(rowpval) == 0) {
        rowpval = NA
      }
    }
    
    #test if there is a difference between cols
    #(anova)
    if (length(unique(df$col_plate)) < 2) {
      colpval <- NA
      coladj <- NA
    } else {
      anovaT <- aov(signal ~ col_plate, data = df)
      summarY <- summary(anovaT)
      colpval <- as.numeric(summarY[[1]][1,5])
      if (length(colpval) == 0) {
        colpval = NA
      }
    }
    
    #test if there is a difference between plates
    #(anova)
    if (length(unique(df$plate)) < 2) {
      platepval <- NA
      plateadj <- NA
    #if there are only two plates for this compound, run a t test
    } else if (length(unique(df$plate)) == 2) {
      first_plate <- sort(unique(df$plate))[[1]]
      second_plate <- sort(unique(df$plate)) [[2]]
      
      t_test <- t.test(df[df$plate == first_plate, "signal"], df[df$plate == second_plate, "signal"])
      platepval = t_test$p.value
      #if the p-value is less than .05, add a boxplot to the output list
      if (platepval < .05) {
        boxplot <- ggplot(data = df, aes(x = plate, y = signal)) +
          geom_boxplot() +
          geom_quasirandom(alpha = .5) +
          theme_bw() +
          ggtitle(paste(compound_name, "Signal by Plate"))  +
          theme(text = element_text(family = "serif"))
        plate_posthoc_bps[[compound_name]]<- boxplot
      }
    #if there are more than two plates for this compound, 
    } else if (length(unique(df$plate)) > 2) {
      anovaT <- aov(signal ~ plate, data = df)
      summarY <- summary(anovaT)
      platepval <- as.numeric(summarY[[1]][1,5])
      if (length(platepval) == 0) {
        platepval = NA
      #if the anova test for mean difference in signal between plates is 
          #less than .05, run posthoc tests and add them to the plate posthoc 
          #lists you made at the top of this for loop
      } else if (platepval < .05) {
        plate_posthoc_tests[[compound_name]] <- TukeyHSD(anovaT)
        
        plate_posthoc_boxplot <- ggplot(data = df, aes(x = plate, y = signal)) +
          geom_boxplot() +
          geom_quasirandom(alpha = .5) +
          theme_bw() +
          ggtitle(paste(compound_name, "Signal by Plate"))  +
          theme(text = element_text(family = "serif"))
        
        plate_posthoc_bps[[compound_name]] <- plate_posthoc_boxplot
      }
    }
    
    #test if there is an effect from surrounding well Signals
    #(lin model)
    lmT <- lm(signal ~ surroundCount, data = df)
    summarY <- summary(lmT)
    surroundpval <- summarY$coefficients[2,4]
    
    if (length(unique(df$near_ctrl0)) == 1) {
      mediapval <- NA
    } else {
      lmT <- lm(signal ~ near_ctrl0, data = df)
      summarY <- summary(lmT)
      mediapval <- summarY$coefficients[2,4]
    }
    
    thesePs <- c(compound_name, round(platepval, 4), round(rowpval, 4), round(colpval, 4), round(surroundpval, 4), round(mediapval, 4))
    ps[row,] <- thesePs
  }
  
  #make plate table output
  plateTable <- ps[!is.na(ps$platepval), c("compound_name", "platepval")]
  plateTable$platepval <- p.adjust(plateTable$platepval)
  names(plateTable) <- c("Compound", "Adjusted P-Value")
  #make plate graph output 
  acrossPlateReps <- ps[!(is.na(ps$platepval)), "compound_name"]
  
  acrossPlateRepDF <- bigdf %>%
    filter(compound_name %in% acrossPlateReps) %>%
    arrange(signal) %>%
    mutate(drug_plate = paste(compound_name, plate))
  
  plateGraph <- ggplot(data = acrossPlateRepDF, aes(x = plate, y = signal)) +
    geom_boxplot() +
    geom_quasirandom() +
    theme_bw() +
    facet_wrap(~compound_name) +
    xlab("Plate") +
    ylab("Signal") +
    labs(colour="Plate") +
    # theme(axis.text.x = element_blank()) +
    ggtitle("Compounds with Across-Plate Replicates") +
    theme(text = element_text(family = "serif"))
  
  #make row table output
  rowTable <- ps[!is.na(ps$rowpval), c("compound_name", "rowpval")]
  rowTable$rowpval <- p.adjust(rowTable$rowpval)
  names(rowTable) <- c("Compound", "Adjusted P-Value")
  #make row graph output 
  acrossRowReps <- ps[!(is.na(ps$rowpval)), "compound_name"]
  
  acrossRowRepDF <- bigdf %>%
    filter(compound_name %in% acrossRowReps) %>%
    arrange(signal) %>%
    mutate(drug_row = paste(compound_name, row, plate))
  
  rowGraphs <- lapply(acrossRowReps, rowPlot, acrossRowRepDF)
  
  #make column table output
  colTable <- ps[!is.na(ps$colpval), c("compound_name", "colpval")]
  colTable$colpval <- p.adjust(colTable$colpval)
  names(colTable) <- c("Compound", "Adjusted P-Value")
  #make column graph output
  acrossColReps <- ps[!(is.na(ps$colpval)), "compound_name"]
  
  acrossColRepDF <- bigdf %>%
    filter(compound_name %in% acrossColReps)
  
  colGraphs <- lapply(acrossColReps, colPlot, acrossColRepDF)
  
  #make surrounding Signal table output
  surroundCountTable <- ps[!is.na(ps$surroundCountP), c("compound_name", "surroundCountP")]
  surroundCountTable$surroundCountP <- p.adjust(surroundCountTable$surroundCountP)
  #add r-squared to surroundCountTable
  get_rsq_func <- function(compound_name) {
    linmod <- lm(signal ~ surroundCount, data = bigdf[bigdf$compound_name == compound_name,])
    lm_summary <- summary(linmod)
    rsq <- lm_summary$r.squared
    return(as.numeric(rsq))
  }
  surroundCountTable["Rsq"] <- round(unlist(lapply(surroundCountTable$compound_name, get_rsq_func)), 4)
  names(surroundCountTable) <- c("Compound", "Adjusted P-Value", "Rsquared")
  #make surrounding Signal graph output
  surroundGraph <- ggplot(data = bigdf, aes(x = surroundCount, y = signal)) +
    geom_smooth(method = "lm", color = "black") +
    theme_bw() +
    # geom_point(data = bigdf, aes(col = compound_name)) +
    geom_point(data = bigdf) +
    facet_wrap(~compound_name) +
    # scale_color_paletteer_d("trekcolors::lcars_cardassian") +
    ggtitle("Surrounding Wells vs. Signal for All Drugs") +
    xlab("Sum of the Signals of the Surrounding Wells") +
    ylab("Signal for the Middle Well") +
    theme(text = element_text(family = "serif"))
  
  surroundGraphs <- lapply(unique(bigdf$compound_name), surroundPlot, bigdf)
  names(surroundGraphs) <- unique(bigdf$compound_name)
  #make near media table output
  mediaTable <- ps[!is.na(ps$mediapval), c("compound_name", "mediapval")]
  mediaTable$mediapval <- p.adjust(mediaTable$mediapval)
  
  #make near media graph
  acrossMedReps <- ps[!(is.na(ps$mediapval)), "compound_name"]
  
  acrossMedRepDF <- bigdf %>%
    filter(compound_name %in% acrossMedReps)
  
  medGraphs <- lapply(acrossMedReps, mediaPlot, acrossMedRepDF, bigdf)
  
  output <- list("plateTable" = plateTable,
                 "plateGraph" = plateGraph,
                 "plateIndivGraphs" = plate_posthoc_bps,
                 "platePostHocTests" = plate_posthoc_tests,
                 "rowTable" = rowTable,
                 "rowGraphs" = rowGraphs,
                 "colTable" = colTable,
                 "colGraphs" = colGraphs,
                 "surroundCountTable" = surroundCountTable,
                 "surroundGraph" = surroundGraph,
                 "surroundGraphs" = surroundGraphs,
                 "mediaTable" = mediaTable,
                 "mediaGraphs" = medGraphs)
  return(output)
}

positionEffects_no_plate <- function(df,
                            ctrl0 = "media|untreated") {
  require(gridExtra)
  
  #data cleaning
  bigdf <- df %>%
    mutate(col_plate = paste(plate, column, sep = " "),
           row_plate = paste(plate, row, sep = " ")) %>%
    #group by plate and column
    group_by(plate, column) %>%
    #sort by row
    arrange(row) %>%
    #get the Signals for the cells below and above each well
    #group_item corresponds to the row within this plate+column
    mutate(group_item = row_number(),
           #above gives you the compound_name in the df row above this one
           #so also the row above this one on the plate
           above = lag(compound_name),
           #above gives you the signal for the well one row above this well on the plate
           aboveCount = lag(signal),
           #below gives you the compound_name in the well above this one on the plate
           below = lead(compound_name),
           #belowCount gives you the signal in the well one row above this one on the plate
           belowCount = lead(signal)) %>%
    ungroup() %>%
    #basically do it the same again but looking at right/left instead of up down
    #so start by grouping by plate and row
    group_by(plate, row) %>%
    #and arrange by column so when we descend through the dataframe, we're also moving left-to-right on the plate
    arrange(column) %>%
    #get the Signals for the cells to the right and left of each well
    #left gives you the drug in the well to the left of this one
    mutate(left = lag(compound_name),
           #leftCount gives you the signal in the well to the left of this one
           leftCount = lag(signal),
           #right gives you the drug in the well to the right of this one
           right = lead(compound_name),
           #and guess what this one is that's right it's the signal in the well to the right of this one
           rightCount = lead(signal)) %>%
    ungroup() %>%
    rowwise() %>%
    #count up the surrounding Signals
    mutate(surroundCount = sum(aboveCount, belowCount, leftCount, rightCount, na.rm = TRUE),
           all_around = paste(above, below, left, right, sep = " "),
           #near media counts up how many media wells are around each well
           near_ctrl0 = str_count(all_around, ctrl0)) %>%
    arrange(compound_dose)
  
  #make "plate position helper" df
  #grouped by compound_name  with columns to designate well plate location
  plate_pos_helper <- bigdf %>%
    mutate(column = paste(plate, column, sep = " "),
           row = paste(plate, row, sep = " ")) %>%
    group_by(compound_name) %>%
    nest()
  
  #make dataframe to calculate p values for plate, row, surrounding Signal
  ps <- data.frame(matrix(ncol = 5, nrow = 0))
  names(ps) <- c("compound_name", 
                 # "platepval", 
                 "rowpval", "colpval",  "surroundCountP", "mediapval")
  
  #loop through each compound_name condition and conduct anova tests to compare row, col, surrounding Signal
  for (row in 1:nrow(plate_pos_helper)) {
    df <- plate_pos_helper[row, "data"][[1]][[1]]
    compound_name <- as.character(plate_pos_helper$compound_name[[row]])
    
    #test if there is a difference between rows
    #(anova)
    if (length(unique(df$row_plate)) == length(unique(df$plate)) | (length(unique(df$row_plate)) < 2)) {
      rowpval <- NA
      rowadj <- NA
    } else {
      anovaT <- aov(signal ~ row, data = df)
      summarY <- summary(anovaT)
      rowpval <- as.numeric(summarY[[1]][1,5])
      if (length(rowpval) == 0) {
        rowpval = NA
      }
    }
    
    #test if there is a difference between cols
    #(anova)
    if (length(unique(df$col)) < 2) {
      colpval <- NA
      coladj <- NA
    } else {
      anovaT <- aov(signal ~ col, data = df)
      summarY <- summary(anovaT)
      colpval <- as.numeric(summarY[[1]][1,5])
      if (length(colpval) == 0) {
        colpval = NA
      }
    }
    
    #test if there is an effect from surrounding well Signals
    #(lin model)
    lmT <- lm(signal ~ surroundCount, data = df)
    summarY <- summary(lmT)
    surroundpval <- summarY$coefficients[2,4]
    
    if (length(unique(df$near_ctrl0)) == 1) {
      mediapval <- NA
    } else {
      lmT <- lm(signal ~ near_ctrl0, data = df)
      summarY <- summary(lmT)
      mediapval <- summarY$coefficients[2,4]
    }
    
    thesePs <- c(compound_name, 
                 # round(platepval, 4), 
                 round(rowpval, 4), round(colpval, 4), round(surroundpval, 4), round(mediapval, 4))
    ps[row,] <- thesePs
  }
  
  rowTable <- ps[!is.na(ps$rowpval), c("compound_name", "rowpval")]
  rowTable$rowpval <- p.adjust(rowTable$rowpval)
  names(rowTable) <- c("Compound", "Adjusted P-Value")
  #make row graph output 
  acrossRowReps <- ps[!(is.na(ps$rowpval)), "compound_name"]
  
  acrossRowRepDF <- bigdf %>%
    filter(compound_name %in% acrossRowReps) %>%
    arrange(signal) %>%
    mutate(drug_row = paste(compound_name, row, plate))
  
  rowGraphs <- lapply(acrossRowReps, rowPlot_noplate, acrossRowRepDF)
  
  #make column table output
  colTable <- ps[!is.na(ps$colpval), c("compound_name", "colpval")]
  colTable$colpval <- p.adjust(colTable$colpval)
  names(colTable) <- c("Compound", "Adjusted P-Value")
  #make column graph output
  acrossColReps <- ps[!(is.na(ps$colpval)), "compound_name"]
  
  acrossColRepDF <- bigdf %>%
    filter(compound_name %in% acrossColReps)
  
  colGraphs <- lapply(acrossColReps, colPlot_noplate, acrossColRepDF)
  
  #make surrounding Signal table output
  surroundCountTable <- ps[!is.na(ps$surroundCountP), c("compound_name", "surroundCountP")]
  surroundCountTable$surroundCountP <- p.adjust(surroundCountTable$surroundCountP)
  names(surroundCountTable) <- c("Compound", "Adjusted P-Value")
  #make surrounding Signal graph output
  surroundGraph <- ggplot(data = bigdf, aes(x = surroundCount, y = signal)) +
    geom_smooth(method = "lm", color = "black") +
    geom_point(data = bigdf, aes(col = compound_name)) +
    facet_wrap(~compound_name) +
    theme(text = element_text(family = "serif")) +
    # scale_color_paletteer_d("trekcolors::lcars_cardassian") +
    ggtitle("Surrounding Wells vs. Signal for All Drugs") +
    xlab("Sum of the Signals of the Surrounding Wells") +
    ylab("Signal for the Middle Well")
  
  #make near media table output
  mediaTable <- ps[!is.na(ps$mediapval), c("compound_name", "mediapval")]
  mediaTable$mediapval <- p.adjust(mediaTable$mediapval)
  
  #make near media graph
  acrossMedReps <- ps[!(is.na(ps$mediapval)), "compound_name"]
  
  acrossMedRepDF <- bigdf %>%
    filter(compound_name %in% acrossMedReps)
  
  medGraphs <- lapply(acrossMedReps, mediaPlot, acrossMedRepDF, bigdf)
  
  output <- list(
    # "plateTable" = plateTable,
    #              "plateGraph" = plateGraph,
                 "rowTable" = rowTable,
                 "rowGraphs" = rowGraphs,
                 "colTable" = colTable,
                 "colGraphs" = colGraphs,
                 "surroundCountTable" = surroundCountTable,
                 "surroundGraph" = surroundGraph,
                 "mediaTable" = mediaTable,
                 "mediaGraphs" = medGraphs)
  return(output)
}


plain_old_row_plot <- function(row) {
  row_df <- row$data
  row_cell <- row$cell_line
  row_date <- row$exp_date
  exp_compound <- unique(row_df$compound_name)
  exp_compound <- exp_compound[!grepl("Untreated|STA|Low EGF", exp_compound)]
  print(exp_compound)
  row_graph <- ggplot(data = row_df, aes(x = row, y = signal, color = compound_name)) +
    geom_point() +
    ggtitle(paste(row_date, row_cell, exp_compound)) +
    labs(subtitle = "Raw Signal by Row") +
    scale_color_viridis_d(begin = .8, end = .1) +
    theme_bw() +
    ylab("Raw Signal") +
    xlab("Row") +
    labs(colour="Compound")
  return(row_graph)
}

plain_old_row_plot_normalized <- function(row) {
  row_df <- row$data
  row_cell <- row$cell_line
  row_date <- row$exp_date
  exp_compound <- unique(row_df$compound_name)
  exp_compound <- exp_compound[!grepl("Untreated|STA|Low EGF", exp_compound)]
  print(exp_compound)
  row_graph <- ggplot(data = row_df, aes(x = row, y = rel_signal, color = compound_name)) +
    geom_point() +
    ggtitle(paste("2023-01-30", row_cell, exp_compound)) +
    labs(subtitle = "Signal, Normalized to STA and Untreated by Row") +
    scale_color_viridis_d(begin = .8, end = .1) +
    theme_bw() +
    ylab("Normalized Signal") +
    xlab("Row") +
    labs(colour="Compound")
  return(row_graph)
}


plain_old_column_plot <- function(row) {
  row_df <- row$data
  row_cell <- row$cell_line
  row_date <- row$exp_date
  exp_compound <- unique(row_df$compound_name)
  exp_compound <- exp_compound[!grepl("Untreated|STA|Low EGF", exp_compound)]
  print(exp_compound)
  row_graph <- ggplot(data = row_df, aes(x = column, y = signal, color = compound_name)) +
    geom_point() +
    ggtitle(paste("2023-01-30", row_cell, exp_compound)) +
    labs(subtitle = "Raw Signal by Column") +
    scale_color_viridis_d(begin = .8, end = .1) +
    theme_bw() +
    ylab("Raw Signal") +
    xlab("Column") +
    labs(colour="Compound")
  return(row_graph)
}

plain_old_column_plot_normalized <- function(row) {
  row_df <- row$data
  row_cell <- row$cell_line
  row_date <- row$exp_date
  exp_compound <- unique(row_df$compound_name)
  exp_compound <- exp_compound[!grepl("Untreated|STA|Low EGF", exp_compound)]
  print(exp_compound)
  row_graph <- ggplot(data = row_df, aes(x = column, y = rel_signal, color = compound_name)) +
    geom_point() +
    ggtitle(paste("2023-01-30", row_cell, exp_compound)) +
    labs(subtitle = "Normalized Signal by Column") +
    scale_color_viridis_d(begin = .8, end = .1) +
    theme_bw() +
    ylab("Normalized Signal") +
    xlab("Column") +
    labs(colour="Compound")
  return(row_graph)
}

get_curve_data <- function(rowi) {
  this_df_i <- rowi$data
  this_model <- rowi$model
  
  
  
  return(curve.data)
}

make_overlay_plots <- function(row) {
  this_compound <- row$compound_name
  this_cell <- row$cell_line
  this_df <- row$data
  this_unit <- row$drug_unit
  
  #get a list of the colors viridis will generate for each sheet_name
    #i make it end at .8 so it doesn't make one of the colors yellow
    #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  col_list <- viridis(2, end = .8)
  #make plot with datapoints to add overlays to 
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  overlay_plot <- ggplot() +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = cv_discrete), alpha = .5) +
    theme_bw() + 
    ylim(c(-50, 150)) +
    ggtitle(paste(this_compound, this_cell)) +
    scale_color_viridis_d(end = .8) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(unique(this_df_points$compound_dose))) +
    ylab('% Response') +
    labs(colour="Plate %CV") +
    xlab(paste(this_compound, this_unit, sep = '  '))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
    #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
                           TRUE ~ col_list[2])
    this_df_i <- this_df[i,]$data[[1]]
    this_model <- this_df[i,]$model[[1]]
    
    curve_data <- PR(this_model, xVec = seq(min(inputDF$compound_dose), max(inputDF$compound_dose), .1))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
    
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_plot <- overlay_plot +
      geom_line(data = curve.data, aes(x = x, y = y), color = this_color)
    
  }

  
  return(overlay_plot)
  
}

make_overlay_plots_filt <- function(row) {
  this_compound <- row$compound_name
  this_cell <- row$cell_line
  this_df <- row$data %>%
    arrange(exp_date)
  this_unit <- row$drug_unit
  this_msr <- row$MSR

  #get a list of the colors viridis will generate for each sheet_name
  #i make it end at .8 so it doesn't make one of the colors yellow
  #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  col_list <- viridis(nrow(this_df), option = "magma", begin = .2, end = .7)
  #make plot with datapoints to add overlays to 
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  overlay_plot <- ggplot() +
    ylim(c(-50, 180)) +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5, size = 2) +
    theme_bw() + 
    ggtitle(paste(this_compound, this_cell)) +
    scale_color_viridis_d(option = "magma", begin = .2, end = .7) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(unique(this_df_points$compound_dose))) +
    ylab('% Response') +
    labs(colour="Experiment Date") +
    xlab(paste(this_compound, this_unit, sep = '  ')) 
    # annotate("text", x = median(this_df_points$compound_dose), y = 140, label = paste("MSR =", round(this_msr, 2)))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    # this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
    #                        TRUE ~ col_list[2])
    this_df_i <- this_df[i,]$data[[1]] %>%
      ungroup()
    this_model <- this_df[i,]$model[[1]]
    this_ec50 <- as.numeric(this_df[i, "rel_ec50"])
    this_lower_ci <- this_df[i, "relative_lower_ci"][[1]]
    this_color <- col_list[i]
    #make sure the lower ci limit is not negative so you can graph it on the log scale
    this_lower_ci <- case_when(this_lower_ci <= 0 ~ min(this_ec50, min(this_df_points$compound_dose)),
                               TRUE ~ this_lower_ci)
    this_upper_ci <- this_df[i, "relative_upper_ci"][[1]]
    
    curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data))) %>%
      ungroup()
    
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_plot <- overlay_plot +
      geom_line(data = curve.data, aes(x = x, y = y), color = this_color, size = 1) +
      geom_vline(xintercept = this_ec50, color = this_color, alpha = .5, size = 1) +
      annotate("rect", fill = this_color, alpha = 0.3, xmin = this_lower_ci, xmax = this_upper_ci, ymin = -Inf, ymax = Inf)
    # +
      # geom_rect(ymin = 0, ymax = 100, xmin = this_lower_ci, xmax = this_upper_ci, color = col_list[i], alpha = .3) +
      # geom_vline(xintercept = this_lower_ci, color = col_list[i], alpha = .3, size = .5) +
      # geom_vline(xintercept = this_upper_ci, color = col_list[i], alpha = .3, size = .5) +
      # annotate("text", x = this_ec50, vjust = -.8, angle = "90", y = -30, label = paste("EC50 =", round(this_ec50, 2)))
        # aes(x = this_ec50, y = -30), label = paste("EC50 =", round(this_ec50, 2)))
    
  }
  
  
  return(overlay_plot)
  
}

make_overlay_plots_filt_nocis <- function(row) {
  this_compound <- row$compound_name
  this_cell <- row$cell_line
  this_df <- row$data %>%
    arrange(exp_date)
  this_unit <- row$drug_unit
  # this_msr <- row$MSR
  
  #get a list of the colors viridis will generate for each sheet_name
  #i make it end at .8 so it doesn't make one of the colors yellow
  #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  col_list <- viridis(nrow(this_df), option = "magma", begin = .2, end = .7)
  #make plot with datapoints to add overlays to 
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  overlay_plot <- ggplot() +
    ylim(c(-50, 180)) +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5, size = 2) +
    theme_bw() + 
    ggtitle(paste(this_compound, this_cell)) +
    scale_color_viridis_d(option = "magma", begin = .2, end = .7) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(unique(this_df_points$compound_dose))) +
    ylab('% Response') +
    labs(colour="Experiment Date") +
    xlab(paste(this_compound, this_unit, sep = '  ')) 
  # annotate("text", x = median(this_df_points$compound_dose), y = 140, label = paste("MSR =", round(this_msr, 2)))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    # this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
    #                        TRUE ~ col_list[2])
    this_df_i <- this_df[i,]$data[[1]]
    this_model <- this_df[i,]$model[[1]]
    this_ec50 <- this_df[i, "rel_ec50"][[1]]
    this_lower_ci <- this_df[i, "relative_lower_ci"][[1]]
    #make sure the lower ci limit is not negative so you can graph it on the log scale
    this_lower_ci <- case_when(this_lower_ci <= 0 ~ min(this_ec50, min(this_df_points$compound_dose)),
                               TRUE ~ this_lower_ci)
    this_upper_ci <- this_df[i, "relative_upper_ci"][[1]]
    
    curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
    
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_plot <- overlay_plot +
      geom_line(data = curve.data, aes(x = x, y = y), color = col_list[i], size = 1) +
      geom_vline(xintercept = this_ec50, color = col_list[i], alpha = .5, size = 1) 
      # annotate("rect", fill = col_list[i], alpha = 0.3, xmin = this_lower_ci, xmax = this_upper_ci, ymin = -Inf, ymax = Inf)
    # +
    # geom_rect(ymin = 0, ymax = 100, xmin = this_lower_ci, xmax = this_upper_ci, color = col_list[i], alpha = .3) +
    # geom_vline(xintercept = this_lower_ci, color = col_list[i], alpha = .3, size = .5) +
    # geom_vline(xintercept = this_upper_ci, color = col_list[i], alpha = .3, size = .5) +
    # annotate("text", x = this_ec50, vjust = -.8, angle = "90", y = -30, label = paste("EC50 =", round(this_ec50, 2)))
    # aes(x = this_ec50, y = -30), label = paste("EC50 =", round(this_ec50, 2)))
    
  }
  
  
  return(overlay_plot)
  
}

sn38_plot_maker <- function(row) {
  this_compound <- row$compound_name
  this_cell <- row$cell_line
  this_df <- row$data
  this_unit <- row$drug_unit

  #get a list of the colors viridis will generate for each sheet_name
  #i make it end at .8 so it doesn't make one of the colors yellow
  #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  col_list <- viridis(nrow(this_df), option = "magma", begin = .2, end = .7)
  #make plot with datapoints to add overlays to 
   
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  
  # %>%
  #   unnest(cols = c(data))
  
  overlay_plot <- ggplot() +
    ylim(c(-50, 180)) +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5, size = 2) +
    theme_bw() + 
    ggtitle(paste(this_compound, this_cell)) +
    scale_color_viridis_d(option = "magma", begin = .3, end = .7) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(unique(this_df_points$compound_dose))) +
    ylab('% Response') +
    labs(colour="Experiment Date") +
    xlab(paste(this_compound, this_unit, sep = '  ')) 
  # annotate("text", x = median(this_df_points$compound_dose), y = 140, label = paste("MSR =", round(this_msr, 2)))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    
    if (is.na(this_df[i, "doesItModel"])) {
      this_df_i <- this_df[i,]$data[[1]]
      overlay_plot <- overlay_plot +
        geom_smooth(data = this_df_i, aes(x = compound_dose, y = rel_signal), color = col_list[i], linewidth = .5, se = FALSE)
        # geom_line(data = curve.data, aes(x = x, y = y), color = col_list[i], size = 1) +
        # geom_vline(xintercept = this_ec50, color = col_list[i], alpha = .5, size = 1) 
    } else {
      # this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
      #                        TRUE ~ col_list[2])
      this_df_i <- this_df[i,]$data[[1]]
      this_model <- this_df[i,]$model[[1]]
      this_ec50 <- this_df[i, "rel_ec50"][[1]]
      this_lower_ci <- this_df[i, "relative_lower_ci"][[1]]
      #make sure the lower ci limit is not negative so you can graph it on the log scale
      this_lower_ci <- case_when(this_lower_ci <= 0 ~ min(this_ec50, min(this_df_points$compound_dose)),
                                 TRUE ~ this_lower_ci)
      this_upper_ci <- this_df[i, "relative_upper_ci"][[1]]
      
      curve.data.drc <- PR(this_model, 
                           xVec = seq(min(this_df_i$compound_dose), 
                                      max(this_df_i$compound_dose), .1))
      curve.data <- data.frame(y = as.vector(unlist(curve.data.drc)),
                               x = as.numeric(names(curve.data.drc)))
      overlay_plot <- overlay_plot +
        geom_line(data = curve.data, aes(x = x, y = y), color = col_list[i], size = 1) 
      # +
      #   geom_vline(xintercept = this_ec50, color = col_list[i], alpha = .5, size = 1) 
    }
  }

  

}



overlay_norm_noci_forsummary <- function(this_df, this_compound, this_cell, col_list) {
  #make plot with datapoints to add overlays to 
  # this_df <- this_df %>%
  #   filter(flag == "passed QC") 
  these_colors <- as.list(col_list[names(col_list) %in% gsub("-", "", unique(this_df$exp_date))])
  
  units <- unique(this_df$drug_unit)[[1]]
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  overlay_plot <- ggplot() +
    ylim(c(-50, 180)) +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5, size = 2) +
    theme_bw() + 
    theme(text = element_text(family = "serif")) +
    ggtitle(paste(this_compound, this_cell)) +
    scale_color_manual(values = unname(these_colors)) +
    # scale_color_viridis_d(option = "magma", begin = .2, end = .7) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(unique(this_df_points$compound_dose))) +
    ylab('% Response') +
    labs(colour="Experiment Date", subtitle = "No IC50 Confidence Intervals") +
    xlab(paste(this_compound, units)) 
  # annotate("text", x = median(this_df_points$compound_dose), y = 140, label = paste("MSR =", round(this_msr, 2)))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    # this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
    #                        TRUE ~ col_list[2])
    this_df_i <- this_df[i,]$data[[1]]
    this_model <- this_df[i,]$model[[1]]
    this_ec50 <- this_df[i, "rel_ec50"][[1]]
    this_date <- gsub("-", "", this_df[i, "exp_date"][[1]])
    # this_color <- col_list[i]
    this_color = col_list[[as.character(this_date)]]
    #if this row has a model / if this experiment converged in drc:
    does_it_model <- as.logical(this_df[i,]$doesItModel)
    if (does_it_model == TRUE) {
      
      curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
      curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
    
      #l2 sets the top value at 1, so i am multiplying y by 100 here
      if (length(this_model$coefficients) == 2) {
        curve.data <- curve.data %>%
          mutate(y = y*100)
      }
      overlay_plot <- overlay_plot +
        geom_line(data = curve.data, aes(x = x, y = y), color = this_color, size = 1) +
        geom_vline(xintercept = this_ec50, color = this_color, alpha = .5, size = 1) 
    }
  }
  
  
  return(overlay_plot)
  
}


overlay_norm_forsummary <- function(this_df, compound, cell, col_list) {
  these_colors <- col_list[names(col_list) %in% gsub("-", "", unique(this_df$exp_date))]
  
  # this_df <- this_df %>%
  #   filter(flag == "passed QC")
  #get a list of the colors viridis will generate for each sheet_name
  #i make it end at .8 so it doesn't make one of the colors yellow
  #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  units <- unique(this_df$drug_unit)[[1]]
  
  #make plot with datapoints to add overlays to 
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  overlay_plot <- ggplot() +
    ylim(c(-50, 180)) +
    geom_point(data = this_df_points, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5, size = 2) +
    theme_bw() + 
    theme(text = element_text(family = "serif")) +
    ggtitle(paste(compound, cell)) +
    scale_color_manual(values = these_colors) +
    # scale_color_viridis_d(option = "magma", begin = .2, end = .7) +
    scale_x_log10(labels = as.character(sort(unique(this_df_points$compound_dose))), breaks = sort(round(unique(this_df_points$compound_dose), 2))) +
    ylab('% Response') +
    labs(colour="Experiment Date", subtitle = "IC50 95% Confidence Intervals") +
    xlab(paste(compound, units)) 
  # annotate("text", x = median(this_df_points$compound_dose), y = 140, label = paste("MSR =", round(this_msr, 2)))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    # this_color = case_when(this_df[i,"cv_discrete"][[1]] == "high" ~ col_list[1],
    #                        TRUE ~ col_list[2])
    this_df_i <- this_df[i,]$data[[1]] %>%
      ungroup()
    this_model <- this_df[i,]$model[[1]]
    this_ec50 <- as.numeric(this_df[i, "rel_ec50"])
    this_lower_ci <- this_df[i, "relative_lower_ci"][[1]]
    this_date <- this_df[i, "exp_date"][[1]]
    # this_color <- col_list[i]
    this_color = col_list[[as.character(this_date)]]
    #make sure the lower ci limit is not negative so you can graph it on the log scale
    this_lower_ci <- case_when(this_lower_ci <= 0 ~ min(this_ec50, min(this_df_points$compound_dose)),
                               TRUE ~ this_lower_ci)
    this_upper_ci <- this_df[i, "relative_upper_ci"][[1]]
    
    curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data))) %>%
      ungroup()
    
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_plot <- overlay_plot +
      geom_line(data = curve.data, aes(x = x, y = y), color = this_color, size = 1) +
      geom_vline(xintercept = this_ec50, color = this_color, alpha = .5, size = 1) +
      annotate("rect", fill = this_color, alpha = 0.3, xmin = this_lower_ci, xmax = this_upper_ci, ymin = -Inf, ymax = Inf)
  }
  
  
  return(overlay_plot)
  
}

raw_plotter <- function(compound_cell_data, all_data, compound = compound, cell = cell) {
  # ,
  # xmin = .01, xmax = 10000
  # raw_df0 <- compound_cell_data
  file_list <- unique(compound_cell_data$file_name)
  units <- unique(compound_cell_data$drug_unit)[[1]]
  
  raw_df <- all_data %>%
    filter(file_name %in% file_list) %>%
    # unnest(data, names_sep = ".") %>%
    filter(compound_name %in% c(compound, "Untreated", "Low EGF", "STA")) %>%
    mutate(compound_dose = case_when(is.na(compound_dose) & !is.na(compound_name) ~ min(compound_dose[compound_dose > 0], na.rm = TRUE),
                                     compound_dose == 0 ~ min(compound_dose[compound_dose > 0], na.rm = TRUE),
                                     TRUE ~ compound_dose))
  raw_plot <- ggplot(raw_df, aes(x = compound_dose, y = signal, color = as.factor(exp_date))) +
    # scale_x_log10(limits = c(xmin, xmax)) +
    scale_x_log10() +
    geom_point(aes(shape = as.factor(compound_name)), size = 2, alpha = .5) +
    ggtitle(paste(compound, cell)) +
    # scale_fill_manual(labels = c(unique(raw_df[,c("exp_date", "plate_id")])$exp_date), breaks = unique(raw_df$plate_id)) +
    theme_bw() +
    theme(text = element_text(family = "serif")) +
    labs(colour="Experiment Date", shape = "Compound", subtitle = "with Plate Controls", linetype = "Line") +
    ylab("Raw Signal") +
    xlab(paste("Dose", units)) +
    geom_smooth(aes(linetype = compound_name), method = "loess", linewidth = .6, se = FALSE, alpha = .6) +
    scale_color_viridis_d(option = "magma", begin = .2, end = .7)
  # +
  #   scale_linetype_manual(values = linetypes_list)

  #dash for enz!!!!!
  return(raw_plot)
}


get_qc_table <- function(input_df, label) {

  input_df <- input_df %>%
    filter(flag %in% c("passed QC"))
  n <- nrow(input_df)
  br_lab <- case_when(n == 1 ~ "biological replicate.",
                      TRUE ~ "biological replicates.")
  dossier_table <- input_df %>%
    dplyr::select(c(exp_date, absolute_ec50, rel_ec50, einf, rse)) %>%
    arrange(exp_date) %>%
    gt(rowname_col = c("exp_date")) %>%
    opt_table_font("serif") %>%
    tab_caption(paste(n, br_lab)) %>%
    tab_spanner(label = label, columns = c(exp_date, rel_ec50, absolute_ec50, einf, rse)) %>%
    grand_summary_rows(columns = c(rel_ec50, rse),
                       fns = list(Mean ~ mean(., na.rm = TRUE),
                                  SEM ~ se(.)),
                       fmt = ~ fmt_number(., use_seps = FALSE)) %>%
    fmt_number(decimals = 2) %>%
    cols_label(rel_ec50 = "Relative EC50",
               absolute_ec50 = "Absolute EC50",
               einf = "Einf",
               rse = "RSE")
  
  tmp <- tempfile(fileext = '.png') #generate path to temp .png file
  gtsave(dossier_table, tmp) #save gt table as png
  table_png_avgs <- png::readPNG(tmp, native = TRUE) # read tmp png file
  return(table_png_avgs)
}

se <- function(x) {
  x <- na.omit(x)
  sd(x) / sqrt(length(x)) 
}


get_raw_data_table <- function(input_df, label) {

  dossier_table <- input_df %>% 
    dplyr::select(c(exp_date, rel_ec50, rel_ec50_ci, absolute_ec50, einf, rse, z_prime_factor, plate_cv, flag)) %>%
    arrange(exp_date) %>%
    gt(rowname_col = c("exp_date")) %>%
    opt_table_font("serif") %>%
    tab_spanner(label = label, columns = c(exp_date, rel_ec50, rel_ec50_ci, absolute_ec50, einf, rse, z_prime_factor, plate_cv, flag)) %>%
    fmt_number(decimals = 2) %>%
    cols_label(rel_ec50 = "Relative EC50",
               rel_ec50_ci = "Rel EC50 CI",
               absolute_ec50 = "Absolute EC50",
               einf = "Einf",
               rse = "RSE",
               z_prime_factor = "Z' Factor",
               plate_cv = "Plate %CV",
               flag = "QC Flag")
  #in order to print with patchwork, our table has to be saved as a ggplot object... jk ....  a png
  #how siiiickkkkk is that
  # ggp_table <- ggplot() + 
  #   theme_void() +
  #   annotate(geom = "table", x = 1, y = 1, label = dossier_table)
  tmp <- tempfile(fileext = '.png') #generate path to temp .png file
  gtsave(dossier_table, tmp) #save gt table as png
  table_png <- png::readPNG(tmp, native = TRUE) # read tmp png file
}



get_compound_summary <- function(compound_cell_drms, compound_cell_data, all_data, compound = compound, cell = cell) {
  #get color list for compound and enz models to pass to normalized plotter function
  col_list <- viridis(length(unique(compound_cell_drms$exp_date)), option = "magma", begin = .2, end = .7)
  names(col_list) <- sort(unique(compound_cell_drms$exp_date))
  if ("passed QC" %in% compound_cell_drms$flag) {
    norm_plot_cis <- overlay_norm_forsummary(compound_cell_drms[compound_cell_drms$flag == "passed QC",], compound, cell, col_list)
    norm_plot_no_cis <- overlay_norm_noci_forsummary(compound_cell_drms[compound_cell_drms$flag == "passed QC",], compound, cell, col_list)
  } else {
    norm_plot_cis <- ggplot()
    norm_plot_no_cis <- ggplot()
  }
  #get plot of all raw data (incl enz)
  raw_plot <- raw_plotter(compound_cell_drms, all_data, compound, cell)
  #get a plot of all normalized data
  all_norm_plot_nocis <- overlay_norm_noci_forsummary(compound_cell_drms, compound, cell, col_list)
  #now time for tables!
  all_table <- compound_cell_drms %>%
    mutate(rel_ec50_ci = paste("(", round(relative_lower_ci, 2), ", ", round(relative_upper_ci, 2), ")", sep = "")) %>%
      # absolute_ec50_ci = paste("(", round(absolute_lower_ci, 2), ", ", round(absolute_upper_ci, 2), ")", sep = ""),
    ungroup() %>%
    mutate(exp_date = as.Date(as.character(exp_date), "%Y%m%d"))
  #make a filtered df for that paper data averages table kp wants
  
  if (nrow(all_table) > 0 & ("passed QC" %in% compound_cell_drms$flag)) {
    table_png_avgs <- get_qc_table(all_table, label = paste(compound, cell, "Passed QC"))
  } else {
    table_png_avgs <- data.frame()
  }
  all_output_table <- get_raw_data_table(all_table, label = paste(compound, cell, "All Data"))
  
  #get non paper data
  non_qc_data <- all_table %>%
    filter(flag != "passed QC") %>%
    ungroup()
  
  if (nrow(non_qc_data[non_qc_data$doesItModel == TRUE,]) > 0) {
    non_qc_plot <- overlay_norm_noci_forsummary(non_qc_data, compound, cell, col_list)                                    
    non_qc_table <- get_raw_data_table(non_qc_data, label = paste(compound, cell, "Data that Didn't Pass QC"))
  } else {
    non_qc_plot <- ggplot()
    non_qc_table <- data.frame()
  }
  return_list <- list("norm_plot_cis" = norm_plot_cis,
                      "norm_plot_nocis" = norm_plot_no_cis,
                      "raw_plot" = raw_plot,
                      "all_table" = all_output_table,
                      "qc_data_avgs" = table_png_avgs,
                      "non_qc_table" = non_qc_table,
                      "non_qc_plot" = non_qc_plot,
                      "all_norm_plot_nocis" = all_norm_plot_nocis)
  return(return_list)
}

another_single_drug_plot_function <- function(drms_for_overlay_by_vibe2, compound, cell) {
  df_by_vibe2 <- drms_for_overlay_by_vibe2 %>%
    filter(compound_name == compound & cell_line == cell) %>%
    unnest(cols = c(data)) %>%
    unnest(cols = c(data))
  this_drm <- drm(rel_signal ~ compound_dose, data = df_by_vibe2, fct = LL.4())
  # plot(this_drm)
  drm_sum <- summary(this_drm)
  rse <- drm_sum$rseMat[[1]]
  
  predoses <- data.frame(dose = seq(from = min(df_by_vibe2$compound_dose), to = max(df_by_vibe2$compound_dose), length.out = 100))
  predoses['preds'] <- predict(this_drm, data.frame(predoses))

  ec50 <- this_drm$coefficients[[length(this_drm$coefficients)]]
  
  #get bendpoints
  #hX = hill slope for compound_name X
  h <- this_drm$coefficients[[1]]
  
  k <- 4.6805
  bend1 <- ec50*(k^(1/h))
  bend2 <- ec50*((1/k)^(1/h))
  
  #oh my god i'm not even using this line in this function....
  #TO DO: change this function so it uses this instead of predoses
  curve_data <- PR(this_drm, xVec = seq(min(df_by_vibe2$compound_dose), max(df_by_vibe2$compound_dose), .1))
  
  curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
  
  #l2 sets the top value at 1, so i am multiplying y by 100 here
  if (length(this_drm$coefficients) == 2) {
    curve.data <- curve.data %>%
      mutate(y = y*100)
  }
  
  top = max(df_by_vibe2$rel_signal) + 50
  bottom = min(min(df_by_vibe2$rel_signal) - 10, 0)
  d1Plot <- ggplot() +
    geom_line(data = curve.data, aes(x = x, y = y)) +
    geom_point(data = df_by_vibe2, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date))) +
    scale_x_log10(labels = as.character(sort(unique(df_by_vibe2$compound_dose))), breaks = sort(unique(df_by_vibe2$compound_dose))) +
    theme_bw() +
    ylim(c(bottom, top)) +
    # labs(subtitle = file_name) +
    scale_color_viridis_d(begin = .8, end = .1) +
    geom_vline(xintercept = c(bend1, bend2), color= 'darkgray') +
    geom_vline(xintercept = ec50, color = 'gray') +
    geom_label(aes(x = ec50, y = top, label = 'EC50')) +
    ggtitle(paste(compound, cell)) +
    # theme(legend.position = "none") +
    ylab('% Response') +
    # ylim(c(-50, 150)) +
    xlab(paste(compound_name, unit, sep = '  ')) +
    geom_label(aes(x = median(df_by_vibe2$compound_dose), y = 125, label = paste('RSE = ', round(rse, 2), sep = ''))) +
    guides(color=guide_legend(title="Date"))
  
  print(d1Plot)
  
  
}

#this is very similar to another_single_drug_plot_function, but it takes a row of a dataframe as input
comp_plotter <- function(row) {
  compound <- row$compound_name
  cell <- row$cell_line
  unit <- row$drug_unit

  df <- row$data
  if (compound == "SN38") {
    this_drm <- drm(rel_signal ~ compound_dose, data = df, fct = LL.5())
  } else {
    this_drm <- drm(rel_signal ~ compound_dose, data = df, fct = LL.4())
  }
  
# plot(this_drm)
  drm_sum <- summary(this_drm)
  rse <- drm_sum$rseMat[[1]]
  
  # predoses <- data.frame(dose = seq(from = min(df$compound_dose), to = max(df$compound_dose), length.out = 100))
  # predoses['preds'] <- predict(this_drm, data.frame(predoses))

  ec50 <- this_drm$coefficients[[length(this_drm$coefficients)]]
  
  #get bendpoints
  #hX = hill slope for compound_name X
  h <- this_drm$coefficients[[1]]
  
  k <- 4.6805
  bend1 <- ec50*(k^(1/h))
  bend2 <- ec50*((1/k)^(1/h))
  
  #oh my god i'm not even using this line in this function....
  #TO DO: change this function so it uses this instead of predoses
  curve_data <- PR(this_drm, xVec = lseq(min(df$compound_dose), max(df$compound_dose), 100))
  
  curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
  
  #l2 sets the top value at 1, so i am multiplying y by 100 here
  if (length(this_drm$coefficients) == 2) {
    curve.data <- curve.data %>%
      mutate(y = y*100)
  }
  
  top = max(df$rel_signal) + 50
  bottom = min(min(df$rel_signal) - 10, 0)
  d1Plot <- ggplot() +
    geom_line(data = curve.data, aes(x = x, y = y)) +
    geom_point(data = df, aes(x = compound_dose, y = rel_signal, color = as.factor(exp_date)), alpha = .5) +
    scale_x_log10(labels = as.character(sort(unique(df$compound_dose))), breaks = sort(unique(df$compound_dose))) +
    theme_bw() +
    ylim(c(bottom, top)) +
    # labs(subtitle = file_name) +
    scale_color_viridis_d(begin = .8, end = .1) +
    geom_vline(xintercept = c(bend1, bend2), color= 'darkgray') +
    geom_vline(xintercept = ec50, color = 'gray') +
    geom_label(aes(x = ec50, y = top, label = 'EC50')) +
    ggtitle(paste(compound, cell)) +
    # theme(legend.position = "none") +
    ylab('% Response') +
    # ylim(c(-50, 150)) +
    xlab(paste(compound, unit, sep = '  ')) +
    geom_label(aes(x = median(df$compound_dose), y = 125, label = paste('RSE = ', round(rse, 2), sep = ''))) +
    guides(color=guide_legend(title="Date")) +
    theme(text = element_text(family = "serif"))
  
  return(d1Plot)
  
  
}

get_comp_models <- function(compound_name, df) {
  if (compound_name == "SN38") {
    this_drm <- drm(rel_signal ~ compound_dose, data = df, fct = LL.5())
  } else if (compound_name == "CTX") {
    this_drm <- drm(rel_signal ~ compound_dose, data = df, fct = llogistic())
  } else {
    this_drm <- drm(rel_signal ~ compound_dose, data = df, fct = LL.4())
  }
  return(this_drm)
}

get_dfs <- function(row) {
  bend2_d1 <- as.numeric(row$bend2)
  bend1_d1 <- as.numeric(row$bend1)
  ec501 <- as.numeric(row$rel_ec50)
  cell <- as.character(row$cell_line)
  #make a list of dilution factors
  #dilf here stands for dilution factor here obviously
  #get your mind out of the gutter
  dilfs_d1 <- as.data.frame(seq(1, 20, by = 1)) %>%
    #TO DO: fix this so you don't have to change it every time
    #also make dfs function input
    rename(dilf = 'seq(1, 20, by = 1)') %>%
    #calculate dilutions for each viable dilution factor
    #make sure that the dilution series includes the IC50
    mutate(ec50 = round(ec501, 0),
           dil3 = ec50/dilf,
           dil2 = dil3/dilf,
           dil1 = dil2/dilf,
           dil5 = ec50*dilf,
           dil6 = dil5*dilf,
           dil7 = dil6*dilf,
           cell_line = cell) %>%
    #filter these dilution series to include only those with two points on each asymptote (beyond the d1 bendpoints)
    filter(dil2 < bend2_d1 & dil6 > bend1_d1) %>%
    #but we also want at least one other point *within* the bend points
    #or in the linear region of the plot
    filter(dil3 > bend2_d1 | dil5 < bend1_d1) %>%
    #nrow = 22, so I'll narrow this down further to dilution series with easier dilution factors
    filter(dilf %%.5 == 0)
  
  return(dilfs_d1)
}

plot_df <- function(row, d1_plots, d1_doses) {
  cell <- as.character(row[["cell_line"]])
  this_dilf <- as.numeric(row[["dilf"]])
  graph_df <- d1_doses %>% 
    #will need to add an if statement here when you get the option to do a manual design or serial dilutions
    filter(cell_line == cell)
    # filter(cell_line == cell & dilf == this_dilf)
  dilution_series_plot <- d1_plots[[cell]] + 
    geom_point(data = graph_df, aes(x = doses, y = grapher), size = 5, color = 'blue') +
    labs(subtitle = paste('Dilution Factor = ', this_dilf, sep = ''))

  return(dilution_series_plot)
}

getd2_ep_doses <- function(cell, comp_models, d1_doses) {
  #used to have dilf as input too
  #filter d1_doses to only include relevant cell and dilution factor
  d1_doses_2 <- d1_doses %>%
    filter(cell_line == cell)
  # filter(cell_line == cell & dilf == this_dilf)
  
  this_model <- comp_models %>%
    filter(cell_line == cell & compound_name == drug1)
  drm1 <- this_model$model[[1]]
  #TO DO: FIX DEPRECATED
  d1_doses_2[c('response', 'lower', 'upper')] <- predict(drm1, as.data.frame(d1_doses_2$dose), interval = 'prediction')
  d1_doses_2 <- d1_doses_2 %>%
    mutate(relative_percent_response = (as.numeric(this_model$max_response) - response)/(as.numeric(this_model$max_response) - as.numeric(this_model$min_response))*100,
           join_factor = as.factor(round(relative_percent_response, 5)))
  this_model_2 <- comp_models %>%
    filter(cell_line == cell & compound_name == drug2)
  drm2 <- this_model_2$model[[1]]
  d2_eds <- ED(drm2, d1_doses_2$relative_percent_response)
  
  #grabbed the drug 2 estimated doses by plugging the drug 1 responses into the estimated dose function and the drug 2 model
  d2_eds <- ED(drm2, d1_doses_2$relative_percent_response, display = FALSE)
  #this gives you the doses of drug 2 that give the same response as the d1Doses
  
  #make a d2DosesDF that coincides with the d1DosesDF
  d2_doses_df <- cbind(rownames(d2_eds), as.data.frame(d2_eds, row.names = FALSE)) %>%
    rename(relative_percent_response = 'rownames(d2_eds)',
           se = 'Std. Error') %>%
    mutate(grapher = 60,
           min_d2 = Estimate - se*1.1,
           max_d2 = Estimate + se*1.1,
           relative_percent_response = as.numeric(substr(relative_percent_response, 5, 15)),
           join_factor = as.factor(round(relative_percent_response, 5))) %>%
    #make sure max_d2 is less than the next highest dose
    mutate(max_d2 = case_when(max_d2 > lead(min_d2) ~ lead(min_d2) - .001,
                              TRUE ~ max_d2))
  d2_doses_df[c('response', 'lower', 'upper')] <- predict(drm2, as.data.frame(d2_doses_df$Estimate), interval = 'prediction')
  d2_doses_df <- d2_doses_df %>%
    rename('doses' = 'Estimate')
  
  all_doses <- full_join(d1_doses_2, d2_doses_df, by = join_by(join_factor), suffix = c(".d1", ".d2"))
  return(all_doses)
}

plot_d2_plots <- function(all_doses_df) {
  cell <- unique(all_doses_df$cell_line)
  this_dilf <- unique(all_doses_df$dilf)
  graph_df <- d1_doses %>% 
    filter(cell_line == cell & dilf == this_dilf)
  
  dilution_series_plot <- d2_plots[[cell]] + 
    geom_point(data = all_doses_df, aes(x = doses.d2, y = grapher.d2), size = 4, color = 'blue') +
    labs(subtitle = paste('Dilution Factor = ', this_dilf, sep = ''))

  return(dilution_series_plot)
}

see_ray <- function(all_doses_df, comp_models) {
  print(all_doses_df)
  ray_points <- all_doses_df %>% 
    dplyr::select(c(doses.d1, doses.d2)) %>%
    filter(doses.d1 != min(doses.d1[doses.d1 > min(doses.d1)]) & doses.d1 != max(doses.d1[doses.d1 < max(doses.d1)]) & doses.d1 != min(doses.d1[doses.d1 > median(doses.d1)])) 
  
  cell <- unique(all_doses_df$cell_line)
  bend1_d1 <- as.numeric(comp_models %>%
    ungroup() %>%
    filter(cell_line == cell & compound_name == drug1) %>%
    dplyr::select(bend1))
  bend2_d1 <- as.numeric(comp_models %>%
                           ungroup() %>%
                           filter(cell_line == cell & compound_name == drug1) %>%
                           dplyr::select(bend2))
  bend1_d2 <- as.numeric(comp_models %>%
                           ungroup() %>%
                           filter(cell_line == cell & compound_name == drug2) %>%
                           dplyr::select(bend1))
  bend2_d2 <- as.numeric(comp_models %>%
                           ungroup() %>%
                           filter(cell_line == cell & compound_name == drug2) %>%
                           dplyr::select(bend2))
  ec501 <- as.numeric(comp_models %>%
                           ungroup() %>%
                           filter(cell_line == cell & compound_name == drug1) %>%
                           dplyr::select(rel_ec50))
  ec502 <- as.numeric(comp_models %>%
                        ungroup() %>%
                        filter(cell_line == cell & compound_name == drug2) %>%
                        dplyr::select(rel_ec50))
  matrixPlot <- ggplot(data = ray_points, aes(x = doses.d1, y = doses.d2)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() +
    scale_fill_discrete(guide="none") +
    theme(legend.position = 'none') +
          # , text = element_text(family = "serif")) +
    scale_color_viridis(discrete=TRUE) +
    geom_vline(xintercept = c(bend1_d1, bend2_d1), color = 'darkgray') +
    geom_vline(xintercept = ec501, color = 'gray') +
    geom_hline(yintercept = c(bend1_d2, bend2_d2), color = 'darkgray') +
    geom_hline(yintercept = ec502, color = 'gray') +
    labs(x = drug1, y = drug2) +
    geom_label(aes(x = .42, y = bend1_d2, label = 'Upper Bend Point'), size = 2.5, hjust = 0) +
    geom_label(aes(x = .42, y = bend2_d2, label = 'Lower Bend Point'), size = 2.5, hjust = 0) +
    geom_label(aes(x = .51, y = ec502, label = 'EC50'), size = 2.5, hjust = 0) +
    geom_label(aes(x = bend1_d1, y = 70, label = 'Upper Bend Point'), size = 2.5, hjust = 0.5) +
    geom_label(aes(x = bend2_d1, y = 70, label = 'Lower Bend Point'), size = 2.5, hjust = 0.5) +
    geom_label(aes(x = ec501, y = 70, label = 'EC50'), size = 2.5, hjust = .5) +
    ggtitle(paste(cell, 'Ray Design'))
  
  return(matrixPlot)
  
}


# read in CTG xls files, create a .csv file. read in using plater
ctg2plater <- function(xls_path, collect_metadata=FALSE) {
  require(readxl)
  require(dplyr)
  require(openxlsx)
  
  #collect the sheets containing plate data
  sheets_names <- excel_sheets(path = xls_path)
  all_sheets <- lapply(sheets_names, function(x) read_excel(xls_path, sheet = x, range = cell_cols("A:M")))
  names(all_sheets) <- sheets_names
  plate_names <- names(all_sheets[names(all_sheets) %in% c("Formatting Parameters", "Sheet1", "Sheet2", 
                                                           "metadata", "plate template", "template dictionary", 
                                                           # "^(Raw Data)", "^(Working Data)", 
                                                           "Layout", 
                                                           "Associated Files") == FALSE])
  
  # plate_names <- plate_names %>% str_subset(pattern = "^(Raw Data)", negate = TRUE)
  
  plate_names <- plate_names %>% str_subset(pattern = "Working")
  
  #plate_names <- plate_names %>% str_subset(pattern = "^(Raw)", negate = TRUE)
  
  #collect the sheets containing metadata
  meta_sheet_names <- names(all_sheets[names(all_sheets) %in% c("metadata", "Metadata")])
  #read in metadata & write to living csv
  meta_df <- read_xlsx(path = xls_path, sheet = meta_sheet_names)
  
  #check if the local_dest directory exists
  if(!dir.exists(here("data_input", "CTG"))) {
    #check and see if it exists again
    if (!dir.exists(here("data_input", "CTG"))) {
      #and if it doesn't, create it
      dir.create(here("data_input", "CTG"))
    }
  }
  if(collect_metadata == TRUE) {
    write_csv(meta_df, file = here("data_input", "CTG", "ctg_metadata_tracker.csv"), append = TRUE)
  }
  
  #check if the local_dest directory exists
  if(!dir.exists(here("data_input", "CTG", "csvs"))) {
    #check and see if it exists again
    if (!dir.exists(here("data_input", "CTG", "csvs"))) {
      #and if it doesn't, create it
      dir.create(here("data_input", "CTG", "csvs"))
    }
  }
  #write plater data to csv
  sapply(
    plate_names,
    function(x) write_csv(all_sheets[[x]], file = here("data_input", "CTG", "csvs", paste(x, ".csv", sep = "")))
  )
  
  # read in all the csv that were just written using plater
  sapply(plate_names, function(x) check_plater_format(file = here("data_input", "CTG", "csvs", paste(x, ".csv", sep = ""))))
  plate_df <- lapply(plate_names, function(x) read_plate(
    file = here("data_input", "CTG", "csvs", paste(x, ".csv", sep = "")), # full path to the .csv file
    well_ids_column = "Well" # name to give column of well IDs
  ))
  
  #extract plate_id from the sheet names so we can merge the plate data w metadata
  #force ImageQC_Corrected to be character type so that we can combine plates that include 'Omit's
  for (i in 1:length(plate_names)) {
    plate_df[[i]]$plate_id <- meta_df$plate_id[i]
    if (length(grep("ImamgeQC_Corrected", names(plate_df[[i]]))) > 0) {
      plate_df[[i]] <- plate_df[[i]] %>%
        rename(ImageQC_Corrected = ImamgeQC_Corrected)
    }
    if(length(grep("ImageQC_Corrected", names(plate_df[[i]]))) > 0) {
      plate_df[[i]]$ImageQC_Corrected <- as.character(plate_df[[i]]$ImageQC_Corrected)
    }
    if(length(grep("Signal", names(plate_df[[i]]))) > 0) {
      plate_df[[i]]$Signal <- as.integer(plate_df[[i]]$Signal)
    }
    # else if (length(grep("ImamgeQC_Corrected", names(plate_df))) > 0) {
    #   plate_df[[i]] <- plate_df[[i]] %>%
    #     rename(ImageQC_Corrected = ImamgeQC_Corrected) %>%
    #     mutate(ImageQC_Corrected = as.character(ImageQC_Corrected))
    #   # $ImageQC_Corrected <- as.character(plate_df[[i]]$ImamgeQC_Corrected)
    #   # plate_df[[i]] <- plate_df[[i]] %>% dplyr::select(-ImamgeQC_Corrected)
    # }
    
  }
  #bind plates into one dataframe
  bound_data <- bind_rows(plate_df)
  
  #merge plate data with metadata
  experiment_df <- full_join(bound_data, meta_df, by = "plate_id")
  experiment_df <- experiment_df %>% janitor::clean_names()
  experiment_df['file_path'] <- xls_path
  split_filename <- strsplit(xls_path, "/")
  xl_name <- split_filename[[1]][length(split_filename[[1]])]
  experiment_df['file_name'] <- substr(xl_name, 1, unlist(gregexpr(".xl", xl_name)) - 1)
  return(experiment_df)
}

dss_rows <- function(row) {
  model <- row$model
  ic50 <- model$coefficients[[4]]
  slope <- model$coefficients[[1]]
  max <- model$coefficients[[3]]
  min <- model$coefficients[[2]]
  
  data <- row$data
  min.conc.tested <- min(data$compound_dose, na.rm = TRUE)
  max.conc.tested <- max(data$compound_dose, na.rm = TRUE)
  #x2 = the concentration at which the curve crosses the minimum activity level
  # x2 <- ED(model, 10)[[1]]
  
  auc <- integrate(function(x) {min + (max - min)/(1 + 10^(slope*(ic50-x)))}, min.conc.tested, max.conc.tested, subdivisions = 1000)
  dividey <- 100*(max - min)
  dss1 <- auc$value/dividey
  t <- 10
  dss1_y10 <- (auc$value - t*(max-min))/((100-t)*(max-min))
  dss2 <- dss1/log10(max)
  dss2_y10 <- dss1_y10/log10(max)
  # dss3_y10 <- 
  return_list <- c("dss1_y10" = dss1_y10,
                   "dss1" = dss1,
                   "dss2" = dss2,
                   "dss2_y10" = dss2_y10)
  return(return_list)
}

get_mean_if_numeric <- function(x) {
  if (is.numeric(x)) {
    returnval <- round(mean(x, na.rm = TRUE), 2)
  } else {
    returnval <- ""
  }
  return(returnval)
}

get_sem_if_numeric <- function(x) {
  if (is.numeric(x)) {
    returnval <- round(se(x), 2)
  } else {
    returnval <- ""
  }
  return(returnval)
}

#you can put dropbox_downloader.R in the scripts folder of your project
#and use it to copy over dropbox files for your project
#if you do it like this, you won't have to open up old files to read them in!
#it's similar to rdrop2, but rdrop2 needs a manager and might die soon :/
#also imo this is marginally easier to do


#first, get the link to the dropbox folder you want to read in 
#it will look like this: https://www.dropbox.com/sh/v8526atx7vfj7vb/AABFVp2nSFV1cCzNUppo34pHa?dl=0
#change the 0 at the end of your link to a 1
#like this: https://www.dropbox.com/sh/v8526atx7vfj7vb/AABFVp2nSFV1cCzNUppo34pHa?dl=1
#this means that instead of taking you to the folder, this link will automatically download the folder
#this example is the dropbox link to  'Dropbox (EITM)', 'EITM AR SPRC 2022 Docs', 'Data' folder
# dropbox_link <- "https://www.dropbox.com/sh/v8526atx7vfj7vb/AABFVp2nSFV1cCzNUppo34pHa?dl=1"

#local dest must be a character string that refers to a directory within your project
dropbox_downloader <- function(dropbox_link = dropbox_link, local_dest = local_dest) {
  
  #check that the dropbox_link ends with a 1
  if (substr(dropbox_link, nchar(dropbox_link), nchar(dropbox_link) + 1) != "1") {
    stop("ERROR: Your dropbox link does not end in a 1!\nRemember to change the 0 at the end of your dropbox link to a 1!")
    
  }
  
  #check if the local_dest directory exists
  if(!dir.exists(local_dest)) {
    #check and see if it exists again
    if (!dir.exists(local_dest)) {
      #and if it doesn't, create it
      dir.create(local_dest)
    }
  }
  
  zip_file_path <- file.path(local_dest, "db_download.zip")
  #this sets the folder where you want to download your dropbox files to
  #I usually put my dropbox input files in a subdirectory of the Data_Input folder
  #this line downloads the dropbox folder you designated in the link into the folder you designated in the destination_dropbox statement
  message(curl::multi_download(url = dropbox_link, destfile = zip_file_path))
  
  #unzip the file
  message(zip::unzip(zipfile = zip_file_path, exdir = here(local_dest, "unzipped")))
  
}

#write a function to run a hypothesis test on absolute ec50
#between caf/no caf
caf_comp <- function(compound) {
  bp_df <- drms %>%
    filter(cell_line == "US") %>%
    filter(grepl(compound, compound_name)) %>%
    distinct(absolute_ec50, einf, rel_ec50, rse, .keep_all = TRUE) %>%
    #filter this out because there is a v2 of this
    filter(file_name != "20230918_EICL-000US_CAF-CM_3BP") %>%
    filter(flag != "single dose experiment") %>%
    #make a column for whether or not this comdition had cafs
    #if the compound is not named [DRUG] CAF-CM, then it was conditioned in CRC media
    #don't need the color labels to have the drug name in them
    mutate(caf_cond = case_when(grepl("CAF-CM", compound_name) ~ "CAF-CM",
                                !grepl("CAF-CM", compound_name) ~ "CRC media"))
  
  #make ic50 boxplots
  ic50s_bp_all <- ggplot(data = bp_df, aes(x = caf_cond, y = absolute_ec50)) +
    geom_boxplot() + 
    theme_bw(base_size = 16) +
    geom_quasirandom(aes(color = caf_cond), size = 2, alpha = .6) +
    scale_color_viridis_d(end = .9) +
    ggtitle("PDTO Absolute IC50s by Compound") +
    labs(subtitle = "All Experiments") +
    guides(color = "none") +
    xlab("Condition") +
    ylab("Absolute IC50 (uM)")
  
  bp_df_qc <- bp_df %>%
    filter(flag == "passed QC")
    
  ic50s_bp_qc <- ggplot(data = bp_df_qc, aes(x = caf_cond, y = absolute_ec50)) +
    geom_boxplot() +
    guides(color = "none") +
    theme_bw(base_size = 16) +
    geom_quasirandom(aes(color = caf_cond), size = 2, alpha = .6) +
    scale_color_viridis_d(end = .9) +
    ggtitle("PDTO Absolute IC50s by Compound") +
    labs(subtitle = "Passed QC Only") +
    xlab("Condition") +
    ylab("Absolute IC50 (uM)")

  #run hypothesis tests
  cafs <- bp_df %>%
    filter(grepl("CAF-CM", compound_name)) 
  nocafs <- bp_df %>%
    filter(!grepl("CAF-CM", compound_name))
  # t_all <- t.test(cafs$absolute_ec50, nocafs$absolute_ec50, alternative = "less")
  t_all <- t.test(cafs$absolute_ec50, nocafs$absolute_ec50)
  
  cafs_qc <- bp_df_qc %>%
    filter(grepl("CAF-CM", compound_name)) 
  nocafs_qc <- bp_df_qc %>%
    filter(!grepl("CAF-CM", compound_name)) 
  
  if (nrow(cafs_qc) < 2 | nrow(nocafs_qc) < 2) {
    t_qc <- NA
  } else {
    # t_qc <- t.test(cafs_qc$absolute_ec50, nocafs_qc$absolute_ec50, alternative = "less")
    t_qc <- t.test(cafs_qc$absolute_ec50, nocafs_qc$absolute_ec50)
    
  }
  
  #make overlay plots
  
  col_list <- viridis(length(unique(bp_df$caf_cond)), option = "magma", end = .7, begin = .2)
  names(col_list) <- unique(bp_df$caf_cond)
  units <- unique(bp_df$drug_unit)[[1]]
  
  #make supplementary ic50 tables
  #this has to be above the overlay plots,
  #becaues in the code for the overlay plots we filter out 
    #experiments that didn't fit the drc model
  tables_list <- list()
  
  for (i in 1:length(unique(bp_df$compound_name))) {
    this_compound <- sort(unique(bp_df$compound_name))[[i]]
    this_table <- bp_df %>%
      filter(compound_name == this_compound) %>%
      # filter(flag == "passed QC") %>%
      # mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::select(exp_date, rel_ec50, absolute_ec50, einf, rse, z_prime_factor, plate_cv, flag)
    
    meanrow <- as.data.frame(lapply(this_table, get_mean_if_numeric))
    meanrow$exp_date <- "Mean:"
    semrow <- as.data.frame(lapply(this_table, get_sem_if_numeric))
    semrow$exp_date <- "SEM:"
    this_table <- rbind(this_table, meanrow, semrow)
    
    names(this_table) <- c("Experiment Date", "Relative IC50", "Absolute IC50", "EInf", "RSE", "Z' Factor", "Plate %CV", "Flag")
    
    this_kable <- kable(as.data.frame(this_table), caption = this_compound, digits = 4, format = "html", table.attr = "style='width:30%;'", valign = TRUE) %>%
      kable_styling(font = 10, full_width = FALSE)
    
    tables_list[[this_compound]] <- this_kable
  }
  
  print_tables <- kables(list(tables_list))
  
  bp_df <- bp_df %>% 
    filter(doesItModel == TRUE)
  
  bp_df_qc <- bp_df_qc %>% 
    filter(doesItModel == TRUE)
  
  #this is above the part where we filter out experiments that don't model
  #so that we can have the points for those experiments, but don't need to ddraw a line for them
  # bp_df_points <- bp_df %>%
  #   unnest(cols = c(data))
  # 
  # bp_df_qc_points <- bp_df_qc %>%
  #   unnest(cols = c(data))
  
  #doing the points this way instead
  #grabbing them from the all_data df instead of unnesting the drms df
  #so that we can include untreated and STA conditions
  bp_df_points <- all_data %>%
    #get all compounds that were in the experiments for this drug
    filter(file_path %in% bp_df$file_path & plate_id %in% bp_df$plate_id) %>%
    #make a column for whether or not this comdition had cafs
    #if the compound is not named [DRUG] CAF-CM, then it was conditioned in CRC media
    #don't need the color labels to have the drug name in them
    mutate(caf_cond = case_when(grepl("CAF-CM", compound_name) ~ "CAF-CM",
                                !grepl("CAF-CM", compound_name) ~ "CRC media")) %>%
    #make a caf_cond column
    # mutate(compound_cond = str_replace(compound_name, compound, ""),
    #        compound_cond = case_when(grepl("CAF-CM", compound_cond) ~ compound_cond,
    #                             TRUE ~ paste(compound_cond, "CRC media")),
    #        compound_cond = trimws(str_replace(compound_cond, "Untreated", "untreated")))
    mutate(compound_cond = str_replace(compound_name, " CAF-CM", ""),
           compound_cond = trimws(str_replace(compound_cond, "Untreated|0|DMSO Media|PBS|Media", "untreated"))) %>%
    mutate(compound_dose = case_when(compound_dose == 0 ~ min(compound_dose[compound_dose != 0])/2,
                                     TRUE ~ compound_dose))
  
  bp_df_qc_points <-  all_data %>%
    #get all compounds that were in the experiments for this drug
    filter(file_path %in% bp_df_qc$file_path & plate_id %in% bp_df_qc$plate_id) %>%
    #make a column for whether or not this comdition had cafs
    #if the compound is not named [DRUG] CAF-CM, then it was conditioned in CRC media
    #don't need the color labels to have the drug name in them
    mutate(caf_cond = case_when(grepl("CAF-CM", compound_name) ~ "CAF-CM",
                                !grepl("CAF-CM", compound_name) ~ "CRC media")) %>%
    #make a caf_cond column
    # mutate(compound_cond = str_replace(compound_name, compound, ""),
    #        compound_cond = case_when(grepl("CAF-CM", compound_cond) ~ compound_cond,
    #                             TRUE ~ paste(compound_cond, "CRC media")),
    #        compound_cond = trimws(str_replace(compound_cond, "Untreated", "untreated")))
    mutate(compound_cond = str_replace(compound_name, " CAF-CM", ""),
           compound_cond = trimws(str_replace(compound_cond, "Untreated|0|DMSO Media|PBS|Media", "untreated"))) %>%
    mutate(compound_dose = case_when(compound_dose == 0 ~ min(compound_dose[compound_dose != 0])/2,
                                     TRUE ~ compound_dose))

  overlay_all <- ggplot() +
    ylim(c(-50, 500)) +
    xlim(c(min(bp_df$compound_dose)/10, max(bp_df$compound_dose*2))) +
    geom_point(data = bp_df_points, aes(x = compound_dose, y = rel_signal, 
                                        color = as.factor(caf_cond),
                                        shape = as.factor(compound_cond)), 
               alpha = .5, size = 2) +
    theme_bw(base_size = 16) + 
    scale_color_manual(values = col_list) +
    #this commented line can make the x axis labels be all the doses that are there
    # scale_x_log10(labels = as.character(sort(unique(bp_df_points$compound_dose))), breaks = sort(round(unique(bp_df_points$compound_dose), 2))) +
    scale_x_log10() +
    ylab("% Response") +
    labs(colour="CAFs", shape = "Compound", subtitle = "All DRCs") +
    xlab(paste("Concentration (", units, ")", sep = "")) +
    ggtitle(paste("PDTO", compound, "CAFs vs No CAFs"))
  
  for (i in 1:nrow(bp_df)) {
    this_df_i <- bp_df[i,]$data[[1]] %>%
      ungroup()
    this_model <- bp_df[i,]$model[[1]]
    this_ec50 <- as.numeric(bp_df[i, "absolute_ec50"])
    this_lower_ci <- bp_df[i, "abs_lower_ci"][[1]]
    this_date <- bp_df[i, "exp_date"][[1]]
    this_compound_name <- bp_df[i, "caf_cond"][[1]]
    this_color = col_list[[as.character(this_compound_name)]]
    #make sure the lower ci limit is not negative so you can graph it on the log scale
    this_lower_ci <- case_when(this_lower_ci <= 0  ~ min(this_ec50, min(bp_df_points$compound_dose)),
                               TRUE ~ this_lower_ci)
    this_upper_ci <- bp_df[i, "abs_upper_ci"][[1]]
    
    curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data))) %>%
      ungroup()
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_all <- overlay_all +
      geom_line(data = curve.data, aes(x = x, y = y), color = this_color, linewidth = 1) +
      geom_vline(xintercept = this_ec50, color = this_color, alpha = .5, linewidth = 1) +
      annotate("rect", fill = this_color, alpha = 0.3, xmin = this_lower_ci, xmax = this_upper_ci, ymin = -Inf, ymax = Inf)
  }
  
  overlay_qc <- ggplot() +
    ylim(c(-50, 180)) +
    xlim(c(min(bp_df$compound_dose)/10, max(bp_df$compound_dose*2))) +
    geom_point(data = bp_df_qc_points, aes(x = compound_dose, y = rel_signal, 
                                        color = as.factor(caf_cond),
                                        shape = as.factor(compound_cond)), 
               alpha = .5, size = 2) +    theme_bw(base_size = 16) + 
    scale_color_manual(values = col_list) +
    # scale_x_log10(labels = as.character(sort(unique(bp_df_qc_points$compound_dose))), breaks = sort(round(unique(bp_df_qc_points$compound_dose), 2))) +
    scale_x_log10() +
    ylab("% Response") +
    labs(colour="CAFs", shape = "Compound", subtitle = "Passed QC") +
    xlab(paste("Concentration (", units, ")", sep = "")) +
    ggtitle(paste("PDTO", compound, "CAFs vs No CAFs"))
  
  for (i in 1:nrow(bp_df_qc)) {
    this_df_i <- bp_df_qc[i,]$data[[1]] %>%
      ungroup()
    this_model <- bp_df_qc[i,]$model[[1]]
    this_ec50 <- as.numeric(bp_df_qc[i, "absolute_ec50"])
    this_lower_ci <- bp_df_qc[i, "abs_lower_ci"][[1]]
    this_date <- bp_df_qc[i, "exp_date"][[1]]
    this_compound_name <- bp_df_qc[i, "caf_cond"][[1]]
    this_color = col_list[[as.character(this_compound_name)]]
    #make sure the lower ci limit is not negative so you can graph it on the log scale
    this_lower_ci <- case_when(this_lower_ci <= 0 ~ min(this_ec50, min(bp_df_qc_points$compound_dose)),
                               TRUE ~ this_lower_ci)
    this_upper_ci <- bp_df_qc[i, "abs_upper_ci"][[1]]
    
    curve_data <- PR(this_model, xVec = lseq(min(this_df_i$compound_dose), max(this_df_i$compound_dose), 100))
    
    curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data))) %>%
      ungroup()
    #l2 sets the top value at 1, so i am multiplying y by 100 here
    if (length(this_model$coefficients) == 2) {
      curve.data <- curve.data %>%
        mutate(y = y*100)
    }
    overlay_qc <- overlay_qc +
      geom_line(data = curve.data, aes(x = x, y = y), color = this_color, linewidth = 1) +
      geom_vline(xintercept = this_ec50, color = this_color, alpha = .5, linewidth = 1) +
      annotate("rect", fill = this_color, alpha = 0.3, xmin = this_lower_ci, xmax = this_upper_ci, ymin = -Inf, ymax = Inf)
  }
  

  return_list <- list("bp_all" = ic50s_bp_all,
                   "bp_qc" = ic50s_bp_qc,
                   "test_all" = t_all,
                   "test_qc" = t_qc,
                   "overlay_all" = overlay_all,
                   "overlay_qc" = overlay_qc,
                   "supp_tables" = print_tables)
  
  return(return_list)
  
}

z_prime_plot <- function(this_plate, this_cell) {
  this_df <- df %>%
    filter(plate == this_plate & cell_line == this_cell) %>%
    mutate(med_compound_name = paste(old_compound_name, compound_dose))
  
  this_z <- unique(this_df$z_prime_factor)
  
  bp <- ggplot(data = this_df, aes(x = med_compound_name, y = signal)) +
    geom_boxplot() + 
    geom_quasirandom(alpha = .7) +
    theme_bw() +
    annotate("text", x = 1, y = max(this_df$signal), label = paste("Z'Factor:", round(this_z, 4)), hjust = 0, family = "serif") +
    ggtitle(paste("Plate", this_plate, this_cell)) +
    xlab("Condition") +
    ylab("Signal") +
    theme(text = element_text(family = "serif"))
  
  
  return(bp)
}

g_coordinate_analysis <- function(this_compound, g_df) {
  this_df <- g_df %>%
    filter(compound == this_compound) %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    summarise(g_coordinate = mean(g_coordinate, na.rm = TRUE)) %>%
    mutate(condition = case_when(concentration_u_m == 0 ~ "Untreated",
                                 TRUE ~ paste(concentration_u_m, "uM", sep = "")),
           cond_for_test = paste(media_type, concentration_u_m)) %>%
    arrange(media_type, concentration_u_m)
             # paste(media_type, concentration_u_m))
  #make condition a factor with levels
  this_df <- this_df %>%
    mutate(condition = factor(condition, levels = unique(this_df$condition)))
  
  bp <- ggplot(data = this_df, aes(x = condition, y = g_coordinate)) +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7) +
    theme_bw() +
    ggtitle(this_compound) +
    xlab("Concentration") +
    ylab("G-Coordinate") +
    ylim(c(0, 1))
  
  #ok anova time
  #step 1 levene's test for equal variance
  test_results <- this_df %>%
    group_by(media_type) %>%
    #get results of levene tests
    do(levene_res = leveneTest(g_coordinate ~ as.factor(concentration_u_m), .),
       bartlett_res = bartlett.test(g_coordinate ~ as.factor(concentration_u_m), .),
       #get results of anova tests
       aov_res = aov(g_coordinate ~ as.factor(concentration_u_m), .),
       welch_res = oneway.test(g_coordinate ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       game_howell = games_howell_test(g_coordinate ~ concentration_u_m, data = .))
  
  #now do tukey's
  tukeys <- lapply(test_results$aov_res, TukeyHSD)
  #grab the p values out of here....
  #well... p values for the levene test, the whole table for the anova test
  levene_test_ps <- c()
  aov_tables <- c()
  tukeys_tables <- c()
  bartlett_test_ps <- c()
  welch_ps <- c()
  g_h_results <- c()
  
  for (i in 1:nrow(test_results)) {
    this_media <- test_results$media_type[[i]]
    levene_test_ps[[this_media]] <- test_results$levene_res[[i]]$`Pr(>F)`[[1]]
    
    bartlett_test_ps[[this_media]] <- test_results$bartlett_res[[i]]$p.value
    
    aov_df <- as.data.frame(tidy(test_results$aov_res[[i]]))
    aov_tables[[this_media]] <- aov_df
    
    welch_ps[[this_media]] <- test_results$welch_res[[i]]$p.value
    
    tukeys_tables[[this_media]] <- as.data.frame(tukeys[[i]]$`as.factor(concentration_u_m)`) %>%
      clean_names()
    # %>%
    #   filter(p_adj < .05)
    
    g_h_results[[this_media]] <- as.data.frame(test_results$game_howell[[i]])
    # %>%
    #   filter(p.adj < .05)
  }
  
  return_list = list("boxplot" = bp,
                     "levene_test_ps" = levene_test_ps,
                     "bartlett_test_ps" = bartlett_test_ps,
                     "aov_tables" = aov_tables,
                     "welch_ps" = welch_ps,
                     "tukeys_tables" = tukeys_tables,
                     "g_h_results" = g_h_results)
  
  return(return_list)
}

read_flim_data <- function(file_path) {
  #get all the sheet names from that excel file
  sheets <- excel_sheets(path = file_path)
  #take out the summary sheet from this list
  #cuz that sheet is f'ed up!! hard to read don't even look at it!
  sheets <- sheets[!grepl("Summary|Sheet1", sheets, ignore.case = TRUE)]
  raw <- lapply(sheets, read_excel, path = file_path)
  #now we just have to add the drug names to each dataframe in the "raw" list
  for (i in 1:length(raw)) {
    raw[[i]]["Compound"] <- sheets[[i]]
    #get columns that are all na
      #(if there are any)
    nacols <- sapply(raw[[i]], function(x) all(is.na(x)))
    just_nacol_names <- names(nacols[nacols>0])
    #remove rows where g_coordinate = "omit"
    raw[[i]] <- raw[[i]] %>%
      dplyr::select(-all_of(just_nacol_names)) %>%
      clean_names() %>%
      filter(g_coordinate != "omit") %>%
      mutate(g_coordinate = as.numeric(g_coordinate),
             fraction_bound = as.numeric(fraction_bound),
             s_coordinate = as.numeric(s_coordinate))

    
  }
  #now push all the dataframes in this list together so it's just one big dataframe
  raw_df <- bind_rows(raw)
  
  return(raw_df)
}

flim_analysis <- function(this_compound, df) {
  this_df <- df %>%
    filter(compound == this_compound) %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    dplyr::summarise(fraction_bound = mean(fraction_bound, na.rm = TRUE),
                     g_coordinate = mean(g_coordinate, na.rm = TRUE),
                     s_coordinate = mean(s_coordinate, na.rm = TRUE)) %>%
    mutate(condition = case_when(concentration_u_m == 0 ~ "Untreated",
                                 TRUE ~ paste(concentration_u_m, "uM", sep = "")),
           cond_for_test = paste(media_type, concentration_u_m)) %>%
    arrange(concentration_u_m, media_type)
  comparisons <- combn(unique(this_df$condition), 2)
  comparisons2 <- list()
  for (j in 1:ncol(comparisons)) {
    comparisons2[[j]] <- c(comparisons[1, j], comparisons[2, j])
  }
  
  # paste(media_type, concentration_u_m))
  #make condition a factor with levels
  this_df <- this_df %>%
    mutate(condition = factor(condition, levels = unique(this_df$condition)))
  
  bp_g <- ggplot(data = this_df, aes(x = condition, y = g_coordinate)) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    theme_bw() +
    ggtitle(paste(this_compound, "G-Coordinate")) +
    xlab("Concentration") +
    ylab("G-Coordinate") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))

  bp_s <- ggplot(data = this_df, aes(x = condition, y = s_coordinate)) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    theme_bw() +
    ggtitle(paste(this_compound, "S-Coordinate")) +
    xlab("Concentration") +
    ylab("S-Coordinate") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))
  
  bp_fb <- ggplot(data = this_df, aes(x = condition, y = fraction_bound)) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    theme_bw() +
    ggtitle(paste(this_compound, "Fraction Bound")) +
    xlab("Concentration") +
    ylab("Fraction Bound") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3)) +
    stat_compare_means(comparison = comparisons2, label = "p.signif")
  
  #ok anova time
  #step 1 levene's test for equal variance
  test_results <- this_df %>%
    group_by(media_type) %>%
    #get results of levene tests
      #originally started collecting levene results, but 
        #bartlett's is better for normal data, so I use that moving forward
    do(levene_res_g = leveneTest(g_coordinate ~ as.factor(concentration_u_m), .),
       levene_res_s = leveneTest(s_coordinate ~ as.factor(concentration_u_m), .),
       levene_res_fb = leveneTest(fraction_bound ~ as.factor(concentration_u_m), .),
       bartlett_res_g = bartlett.test(g_coordinate ~ as.factor(concentration_u_m), .),
       bartlett_res_s = bartlett.test(s_coordinate ~ as.factor(concentration_u_m), .),
       bartlett_res_fb = bartlett.test(fraction_bound ~ as.factor(concentration_u_m), .),
       #get results of anova tests
       aov_res_g = aov(g_coordinate ~ as.factor(concentration_u_m), .),
       aov_res_s = aov(s_coordinate ~ as.factor(concentration_u_m), .),
       aov_res_fb = aov(fraction_bound ~ as.factor(concentration_u_m), .),
       welch_res_g = oneway.test(g_coordinate ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       welch_res_s = oneway.test(s_coordinate ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       welch_res_fb = oneway.test(fraction_bound ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       game_howell_g = games_howell_test(g_coordinate ~ concentration_u_m, data = .),
       game_howell_s = games_howell_test(s_coordinate ~ concentration_u_m, data = .),
       game_howell_fb = games_howell_test(fraction_bound ~ concentration_u_m, data = .))
  
  #now do tukey's
  tukeys_g <- lapply(test_results$aov_res_g, TukeyHSD)
  tukeys_s <- lapply(test_results$aov_res_s, TukeyHSD)
  tukeys_fb <- lapply(test_results$aov_res_fb, TukeyHSD)
  
  #grab the p values out of here....
  #well... p values for the levene test, the whole table for the anova test
  test_results_output <- data.frame(matrix(ncol = 3, nrow = 0))
  names(test_results_output) <- c("condition", "bartlett", "aov")
  
  #we also want the anova
    #pvalues within their own list
  #so that we can print the post hoc tests if those p vals are less than .05 (significant)
  # aov_ps <- c()
  #jk no we don't we can grab them from the output table very easily!
  #also need the post hoc results in their own list to print as needed
  tukeys_tables <- c()
  g_h_results <- c()
  for (i in 1:nrow(test_results)) {
    output_i <- nrow(test_results_output) + 1
    #G COORDINATES FIRST!!
    this_media <- test_results$media_type[[i]]
    this_condition <- paste(this_media, "G-coordinate")
    test_results_output[output_i, "condition"] <- this_condition

    bartlett_result <- test_results$bartlett_res_g[[i]]$p.value
    test_results_output[output_i, "bartlett"] <- bartlett_result
    
    #if bartlett's test results are significant, use welch's anova
      #instead of the normal one
    #and the game-howell test for post hoc
    if (bartlett_result < .05) {
      welchs_p <- test_results$welch_res_g[[i]]$p.value
      # aov_ps[[this_condition]] <- welchs_p
      test_results_output[output_i, "aov"] <- welchs_p
    } else {
      trad_aov_p <- summary(test_results$aov_res_g[[i]])[[1]][["Pr(>F)"]][1]
      # aov_ps[[this_condition]] <- trad_aov_p
      test_results_output[output_i, "aov"] <- trad_aov_p
    }
    
    g_h_results[[this_condition]] <- as.data.frame(test_results$game_howell_g[[i]]) %>%
      filter(p.adj < .05)
    
    tukeys_tables[[this_condition]] <- as.data.frame(tukeys_g[[i]]$`as.factor(concentration_u_m)`) %>%
      clean_names() %>%
      filter(p_adj < .05)
    
    #now do s_coordinate
    this_condition <- paste(this_media, "S Coordinate")
    bartlett_result <- test_results$bartlett_res_s[[i]]$p.value
    welchs_p <- test_results$welch_res_s[[i]]$p.value
    trad_aov_p <- summary(test_results$aov_res_s[[i]])[[1]][["Pr(>F)"]][1]
    g_h_results[[this_condition]] <- as.data.frame(test_results$game_howell_s[[i]]) %>%
      filter(p.adj < .05)
    tukeys_tables[[this_condition]] <- as.data.frame(tukeys_s[[i]]$`as.factor(concentration_u_m)`) %>%
      clean_names() %>%
      filter(p_adj < .05)
    
    output_i <- output_i + 1
    
    test_results_output[output_i, "condition"] <- this_condition
    
    test_results_output[output_i, "bartlett"] <- bartlett_result
    
    #if bartlett's test results are significant, use welch's anova
    #instead of the normal one
    #and the game-howell test for post hoc
    if (bartlett_result < .05) {
      # aov_ps[[this_condition]] <- welchs_p
      test_results_output[output_i, "aov"] <- welchs_p
    } else {
      # aov_ps[[this_condition]] <- trad_aov_p
      test_results_output[output_i, "aov"] <- trad_aov_p
    }
    
    #get the fb stats before you increment i
      #this was from an old way I did it...
      #where i thought i could use the same i to loop through the input test_results
      #and to set the row for the output table
      #obviously that didn't work
      #i could go back and put these in a place where it would be easier to read this
    #.......... but i'm not doing that rn i'm on deadline
    this_condition <- paste(this_media, "Fraction Bound")
    bartlett_result <- test_results$bartlett_res_fb[[i]]$p.value
    welchs_p <- test_results$welch_res_fb[[i]]$p.value
    trad_aov_p <- summary(test_results$aov_res_fb[[i]])[[1]][["Pr(>F)"]][1]
    g_h_results[[this_condition]] <- as.data.frame(test_results$game_howell_fb[[i]]) %>%
      filter(p.adj < .05)
    tukeys_tables[[this_condition]] <- as.data.frame(tukeys_fb[[i]]$`as.factor(concentration_u_m)`) %>%
      clean_names() %>%
      filter(p_adj < .05)
    
    output_i <- output_i + 1
    
    test_results_output[output_i, "condition"] <- this_condition
    
    test_results_output[output_i, "bartlett"] <- bartlett_result
    
    #if bartlett's test results are significant, use welch's anova
    #instead of the normal one
    #and the game-howell test for post hoc
    if (bartlett_result < .05) {
      # aov_ps[[this_condition]] <- welchs_p
      test_results_output[output_i, "aov"] <- welchs_p
    } else {
      # aov_ps[[this_condition]] <- trad_aov_p
      test_results_output[output_i, "aov"] <- trad_aov_p
    }
    
  }
  
  return_list = list("g_boxplot" = bp_g,
                     "fb_boxplot" = bp_fb,
                     "s_boxplot" = bp_s,
                     "stat_test_table" = test_results_output,
                     # "aov_ps" = aov_ps,
                     "tukeys_tables" = tukeys_tables,
                     "g_h_results" = g_h_results)
  
  return(return_list)
}

flim_analysis_t <- function(this_compound, df) {
  this_df <- df %>%
    filter(compound == this_compound) %>%
    #filter for just the highest concentration and no treatment
    filter(concentration_u_m == 0 | concentration_u_m == max(concentration_u_m, na.rm = TRUE)) %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    dplyr::summarise(fraction_bound = mean(fraction_bound, na.rm = TRUE),
                     g_coordinate = mean(g_coordinate, na.rm = TRUE),
                     s_coordinate = mean(s_coordinate, na.rm = TRUE)) %>%
    mutate(condition = case_when(concentration_u_m == 0 ~ "Untreated",
                                 TRUE ~ paste(concentration_u_m, "uM", sep = "")),
           cond_for_test = paste(media_type, concentration_u_m)) %>%
    arrange(concentration_u_m, media_type)
  
  #make condition a factor with levels
  this_df <- this_df %>%
    mutate(condition = factor(condition, levels = unique(this_df$condition)))
  
  
  bp_g <- ggplot(data = this_df, aes(x = condition, y = g_coordinate)) +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +
    theme_bw() +
    ggtitle(paste(this_compound, "G-Coordinate")) +
    labs(subtitle = "Highest Concentration vs. No Treatment") +
    xlab("Concentration") +
    ylab("G-Coordinate") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))
  
  bp_s <- ggplot(data = this_df, aes(x = condition, y = s_coordinate)) +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +    theme_bw() +
    ggtitle(paste(this_compound, "S-Coordinate")) +
    labs(subtitle = "Highest Concentration vs. No Treatment") +
    xlab("Concentration") +
    ylab("S-Coordinate") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))
  
  bp_fb <- ggplot(data = this_df, aes(x = condition, y = fraction_bound)) +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7, aes(color = as.factor(experiment_date))) +
    scale_color_viridis_d() +
    labs(color = "Experiment Date") +    theme_bw() +
    ggtitle(paste(this_compound, "Fraction Bound")) +
    labs(subtitle = "Highest Concentration vs. No Treatment") +
    xlab("Concentration") +
    ylab("Fraction Bound") +
    ylim(c(0, 1)) +
    scale_x_discrete(guide = guide_axis(n.dodge=3))
  
  three_vars <- c("g_coordinate", "s_coordinate", "fraction_bound")
 
  test_results <- this_df %>%
    group_by(media_type) %>%
    #get results of levene tests
    #originally started collecting levene results, but 
    #bartlett's is better for normal data, so I use that moving forward
    do(bartlett_g_p = bartlett.test(g_coordinate ~ as.factor(concentration_u_m), .)$p.value,
       bartlett_s_p = bartlett.test(s_coordinate ~ as.factor(concentration_u_m), .)$p.value,
       bartlett_fb_p = bartlett.test(fraction_bound ~ as.factor(concentration_u_m), .)$p.value)

  #take the bartlett's and t test results out of their lists
  test_results[,grep("_p", names(test_results))] <- lapply(test_results[,grep("_p", names(test_results))], as.numeric)
  
  t_g <- lapply(test_results$media_type, function(x) t.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "g_coordinate"], 
                                                            this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "g_coordinate"])$p.value)
  
  t_s <- lapply(test_results$media_type, function(x) t.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "s_coordinate"], 
                                                            this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "s_coordinate"])$p.value)
  
  fb_s <- lapply(test_results$media_type, function(x) t.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "fraction_bound"], 
                                                            this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "fraction_bound"])$p.value)
  
  wilcox_g <- lapply(test_results$media_type, function(x) wilcox.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "g_coordinate"][[1]], 
                                                            this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "g_coordinate"][[1]])$p.value)
  
  wilcox_s <- lapply(test_results$media_type, function(x) wilcox.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "s_coordinate"][[1]], 
                                                            this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "s_coordinate"][[1]])$p.value)
  
  wilcox_fb <- lapply(test_results$media_type, function(x) wilcox.test(this_df[this_df$concentration_u_m == 0 & this_df$media_type == x, "fraction_bound"][[1]], 
                                                             this_df[this_df$concentration_u_m == max(this_df$concentration_u_m, na.rm = TRUE) & this_df$media_type == x, "fraction_bound"][[1]])$p.value)
  
  test_results[c("g_ttest_p", "s_ttest_p", "fb_ttest_p", "g_wilcox_p", "s_wilcox_p", "fb_wilcox_p")] <-
    data.frame(unlist(t_g), unlist(t_s), unlist(fb_s), unlist(wilcox_g), unlist(wilcox_s), unlist(wilcox_fb))

  test_results <- test_results %>%
    mutate(g_ttest_p = case_when(bartlett_g_p < .05 ~ g_wilcox_p,
                                 TRUE ~ g_ttest_p),
           s_ttest_p = case_when(bartlett_s_p < .05 ~ s_wilcox_p,
                                 TRUE ~ s_ttest_p),
           fb_ttest_p = case_when(bartlett_fb_p < .05 ~ fb_wilcox_p,
                                  TRUE ~ fb_wilcox_p)) %>%
    dplyr::select(media_type, g_ttest_p, s_ttest_p, fb_ttest_p)
  
  test_results$g_ttest_p <- p.adjust(test_results$g_ttest_p)
  test_results$s_ttest_p <- p.adjust(test_results$s_ttest_p)
  test_results$fb_ttest_p <- p.adjust(test_results$fb_ttest_p)
  
  names(test_results) <- c("Media Type", "G-Coordinate t-Test Results", "S-Coordinate t-Test Results", "Fraction Bound t-Test Results")
  
  return_list = list("g_boxplot" = bp_g,
                     "s_boxplot" = bp_s,
                     "fb_boxplot" = bp_fb,
                     "t_test_table" = test_results)
  
  
}

fb_analysis <- function(this_compound, df) {
  this_df <- df %>%
    filter(compound == this_compound) %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    dplyr::summarise(fraction_bound = mean(fraction_bound, na.rm = TRUE)) %>%
    mutate(condition = case_when(concentration_u_m == 0 ~ "Untreated",
                                 TRUE ~ paste(concentration_u_m, "uM", sep = "")),
           cond_for_test = paste(media_type, concentration_u_m)) %>%
    arrange(media_type, concentration_u_m)
  # paste(media_type, concentration_u_m))
  #make condition a factor with levels
  this_df <- this_df %>%
    mutate(condition = factor(condition, levels = unique(this_df$condition)))
  
  bp <- ggplot(data = this_df, aes(x = condition, y = fraction_bound)) +
    geom_boxplot() + 
    facet_wrap(~media_type) +
    geom_quasirandom(alpha = .7) +
    theme_bw() +
    ggtitle(this_compound) +
    xlab("Concentration") +
    ylab("Fraction Bound") +
    ylim(c(0, 1))
  
  #ok anova time
  #step 1 levene's test for equal variance
  test_results <- this_df %>%
    group_by(media_type) %>%
    #get results of levene tests
    do(levene_res = leveneTest(fraction_bound ~ as.factor(concentration_u_m), .),
       bartlett_res = bartlett.test(fraction_bound ~ as.factor(concentration_u_m), .),
       #get results of anova tests
       aov_res = aov(fraction_bound ~ as.factor(concentration_u_m), .),
       welch_res = oneway.test(fraction_bound ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       game_howell = games_howell_test(fraction_bound ~ concentration_u_m, data = .))
  
  #now do tukey's
  tukeys <- lapply(test_results$aov_res, TukeyHSD)
  #grab the p values out of here....
  #well... p values for the levene test, the whole table for the anova test
  levene_test_ps <- c()
  aov_tables <- c()
  tukeys_tables <- c()
  bartlett_test_ps <- c()
  welch_ps <- c()
  g_h_results <- c()
  
  for (i in 1:nrow(test_results)) {
    this_media <- test_results$media_type[[i]]
    levene_test_ps[[this_media]] <- test_results$levene_res[[i]]$`Pr(>F)`[[1]]
    
    bartlett_test_ps[[this_media]] <- test_results$bartlett_res[[i]]$p.value
    
    aov_df <- as.data.frame(tidy(test_results$aov_res[[i]]))
    aov_tables[[this_media]] <- aov_df
    
    welch_ps[[this_media]] <- test_results$welch_res[[i]]$p.value
    
    tukeys_tables[[this_media]] <- as.data.frame(tukeys[[i]]$`as.factor(concentration_u_m)`) %>%
      clean_names() %>%
      filter(p_adj < .05)
    
    g_h_results[[this_media]] <- as.data.frame(test_results$game_howell[[i]]) %>%
      filter(p.adj < .05)
  }
  
  return_list = list("boxplot" = bp,
                     "levene_test_ps" = levene_test_ps,
                     "bartlett_test_ps" = bartlett_test_ps,
                     "aov_tables" = aov_tables,
                     "welch_ps" = welch_ps,
                     "tukeys_tables" = tukeys_tables,
                     "g_h_results" = g_h_results)
  
  return(return_list)
}


flim_drc_analysis <- function(this_compound, df) {
  this_df <- df %>%
    filter(compound == this_compound) %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    dplyr::summarise(fraction_bound = mean(fraction_bound, na.rm = TRUE),
                     g_coordinate = mean(g_coordinate, na.rm = TRUE)) %>%
    mutate(cond_for_test = paste(media_type, concentration_u_m)) %>%
    group_by(experiment_date, media_type) %>%
    nest() %>%
    mutate(n_doses = map_int(data, function(data) length(unique(data$concentration_u_m)))) %>%
    #filter out experiments with 2 or fewer doses
    filter(n_doses > 2) %>%
    #can this experiment be modelled with a 4parameter log logistic dose response curve?
    mutate(does_it_model = map_lgl(data, function(data) getDRM_fb(data)),
           does_it_model_g = map_lgl(data, function(data) getDRM_g(data)),
           model_fb = map(data, function(df) drm(fraction_bound ~ concentration_u_m, fct = LL.4(names = c("hill", "min_value", 'max_value', "ec_50")), data = df, control = drmc(errorm = FALSE))),
           rse_fb = map_dbl(model_fb, function(model) summary(model)$rseMat[[1]]),
           rel_ec50_fb = map_dbl(model_fb, function(model) model$coefficients[[4]]),
           model_g = map(data, function(df) drm(g_coordinate ~ concentration_u_m, fct = LL.4(names = c("hill", "min_value", 'max_value', "ec_50")), data = df, control = drmc(errorm = FALSE))),
           rse_g = map_dbl(model_g, function(model) summary(model)$rseMat[[1]]),
           rel_ec50_g = map_dbl(model_g, function(model) model$coefficients[[4]])) %>%
           #can this experiment be modelled with any number of parameter log logistic drc?
           # doesItModel_ll = map_lgl(data, function(data) getDRM_fb_ll(data)))
           #none of these ran any better with a different number of parameters, so I'm just using 4-parameter
    arrange(media_type, experiment_date)
  
  this_df_points <- this_df %>%
    unnest(cols = c(data))
  this_df_points <- this_df_points %>%
    mutate(concentration_u_m = case_when(concentration_u_m == 0 ~ min(this_df_points$concentration_u_m[this_df_points$concentration_u_m!= 0])/2,
                                         TRUE ~ concentration_u_m))
  #get a list of the colors viridis will generate for each exp_date
  #i make it end at .8 so it doesn't make one of the colors yellow
  #"it's so great for colorblind people" can colorblind people see yellow dots??? idt so
  col_list <- viridis(length(unique(this_df$media_type)), end = .8)
  names(col_list) <- unique(this_df$media_type)
  
  overlay_plot_fb <- ggplot() +
    geom_point(data = this_df_points, aes(x = concentration_u_m, y = fraction_bound, color = as.factor(media_type)), alpha = .5) +
    theme_bw() + 
    # ylim(c(-50, 150)) +
    ggtitle(paste(this_compound, "US Organoids Fraction Bound")) +
    scale_color_viridis_d(end = .8) +
    scale_x_log10() +
    # scale_x_log10(labels = as.character(sort(unique(this_df_points$concentration_u_m))), breaks = sort(unique(this_df_points$concentration_u_m))) +
    ylab("Fraction Bound") +
    labs(colour="Media Type") +
    xlab(paste(this_compound, "uM"))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    this_media <- as.character(this_df[i, "media_type"])
    this_color <- col_list[this_media]
    this_df_i <- this_df[i,]$data[[1]]
    does_it_model <- this_df[i, "does_it_model"][[1]]
    this_model <- this_df[i, "model_fb"][[1]][[1]]
    #if you can use a log logistic drc, do that
    if (does_it_model == TRUE) {
      # this_model <- drm(fraction_bound ~ concentration_u_m, data = this_df_i, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))

      curve_data <- PR(this_model, xVec = seq(min(this_df_points$concentration_u_m), max(this_df_points$concentration_u_m), .1))
      
      curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
      
      overlay_plot_fb <- overlay_plot_fb +
        geom_line(data = curve.data, aes(x = x, y = y), color = this_color)
      
    } else {
      #if you can't use a log logistic drc (because it didn't model),
        #.... use a linear model instead
      overlay_plot_fb <- overlay_plot_fb +
        geom_smooth(method = "lm", formula = this_df_i$fraction_bound ~ this_df_i$concentration_u_m)
    }

  }
  
  #GET OVERLAY PLOT FOR G COORDINATE
  
  overlay_plot_g <- ggplot() +
    geom_point(data = this_df_points, aes(x = concentration_u_m, y = g_coordinate, color = as.factor(media_type)), alpha = .5) +
    theme_bw() + 
    # ylim(c(-50, 150)) +
    ggtitle(paste(this_compound, "US Organoids G-Coordinate")) +
    scale_color_viridis_d(end = .8) +
    scale_x_log10() +
    # scale_x_log10(labels = as.character(sort(unique(this_df_points$concentration_u_m))), breaks = sort(unique(this_df_points$concentration_u_m))) +
    ylab("G-Coordinate") +
    labs(colour="Media Type") +
    xlab(paste(this_compound, "uM"))
  # library(ggplot2) ggplot(df,aes(x,y))+geom_point()+scale_x_continuous(labels=as.character(x),breaks=x)
  
  #add overlay plots in a loop
  #would be very cool to do this in apply, but idk how to do that off the top of my head and have it return a single plot
  for (i in 1:nrow(this_df)) {
    this_media <- as.character(this_df[i, "media_type"])
    this_color <- col_list[this_media]
    this_df_i <- this_df[i,]$data[[1]]
    does_it_model <- this_df[i, "does_it_model_g"][[1]]
    this_model <- this_df[i, "model_g"][[1]][[1]]
    
    #if you can use a log logistic drc, do that
    if (does_it_model == TRUE) {
      # this_model <- drm(g_coordinate ~ concentration_u_m, data = this_df_i, fct = LL.4(names = c("hill", "einf", 'max', "ec_50")))
      
      curve_data <- PR(this_model, xVec = seq(min(this_df_points$concentration_u_m), max(this_df_points$concentration_u_m), .1))
      
      curve.data <- data.frame(y = as.vector(unlist(curve_data)), x = as.numeric(names(curve_data)))
      
      overlay_plot_g <- overlay_plot_g +
        geom_line(data = curve.data, aes(x = x, y = y), color = this_color)
      
    } else {
      #if you can't use a log logistic drc (because it didn't model),
      #.... use a linear model instead
      overlay_plot_g <- overlay_plot_g +
        geom_smooth(method = "lm", formula = this_df_i$g_coordinate ~ this_df_i$concentration_u_m)
    }
    
  }
  
  fb_table <- this_df %>% 
    dplyr::select(experiment_date, media_type, rel_ec50_fb, rse_fb)
  
  names(fb_table) <- c("Exp Date", "Media", "Relative EC50", "RSE")
  
  g_table <- this_df %>% 
    dplyr::select(experiment_date, media_type, rel_ec50_g, rse_g)
  
  names(g_table) <- c("Exp Date", "Media", "Relative EC50", "RSE")
  
  return_list <- list("overlay_plot_fb" = overlay_plot_fb,
                      "overlay_plot_g" = overlay_plot_g,
                      "fb_table" = fb_table,
                      "g_table" = g_table)
  
}

flim_posthoc <- function(condition, bartlett, aov, aov_pval, aov_fstat, aov_df, g_h_results, tukeys_tables, batch_corr = FALSE) {
  #if bartlett's test is significant, use games-howell's post hoc test
  if (bartlett < .05) {
    cat("###", paste(condition, "Post Hoc Tests"))
    
    cat("\n")
    cat("\n")
    
    print(kable(g_h_results[[condition]],
                digits = 4) %>%
      kable_styling(full_width = F))
    
    cat("\n")
    cat("\n")
    
    cat("Bartlett's test found a significant variance between conditions,
                so Games-Howell's post hoc test was used. This table lists the conditions that
                were found to be significantly different.")
    
    cat("\n")
    cat("\n")
    #print to excel sheet
    if (batch_corr == TRUE) {
      write.xlsx(g_h_results[[condition]], here("data_output", "flim_analysis_output.xlsx"), 
                 sheet = paste(i, condition, "Batch Corr. GH"), append = TRUE, row.names = FALSE)
    }
    
    
    
  } else {
    #(if bartlett's test is not significant, you can use tukey's post hoc test)
    cat(paste("###", condition, "Post Hoc Tests"))
    
    cat("\n")
    cat("\n")
    
    print(kable(tukeys_tables[[condition]],
                digits = 4) %>%
      kable_styling(full_width = F))
    
    cat("\n")
    cat("\n")
    
    cat("Bartlett's test did not find a significant variance between conditions,
                so Tukey's post hoc test was used. This table lists the conditions that
                were found to be significantly different.")
    
    cat("\n")
    cat("\n")

    #print to excel sheet
    if (batch_corr == TRUE) {
      write.xlsx(tukeys_tables[[condition]], here("data_output", "flim_analysis_output.xlsx"), 
                 sheet = paste(i, condition, "Batch Corr. Tukeys"), append = TRUE, row.names = FALSE)
    }
    
    
  }
}

#this is used within the flim_dist_analysis function
dist_t_tester <- function(this_conc, this_df_avgd) {
  df_for_t <- this_df_avgd %>%
    filter(concentration_u_m == this_conc)

  return_t <- t.test(df_for_t[df_for_t$media_type == "CAF-CM", "dist"], 
                     df_for_t[df_for_t$media_type == "CRC media", "dist"])
  
  return_p <- return_t$p.value
  
  return(return_p)
}

flim_dist_analysis <- function(this_compound, df) {
  this_df <- df %>%
    filter(compound == this_compound)
  
  #first, make scatter plots
  this_df_caf <- this_df %>% 
    filter(media_type == "CAF-CM") %>%
    group_by(experiment_date) %>%
    filter(n_dose > 1) %>%
    ungroup()
  
  this_df_crc <- this_df %>%
    filter(media_type == "CRC media") %>%
    group_by(experiment_date) %>%
    filter(n_dose > 1) %>%
    ungroup()
  
  splot_caf <- ggplot(data = this_df_caf, aes(x = g_coordinate, y = s_coordinate, color = as.factor(concentration_u_m))) +
    ggtitle(paste(this_compound, "CAF-CM")) +
    facet_wrap(~experiment_date, nrow = 3, ncol = 3) +
    theme_bw() +
    geom_point(alpha = .6, size = 3) +
    scale_color_viridis_d(end = .9) +
    labs(x = "G", y = "S", color = "Dose (uM)") +
    xlim(c(min(this_df$g_coordinate, na.rm = TRUE), max(this_df$g_coordinate, na.rm = TRUE))) +
    ylim(c(min(this_df$s_coordinate, na.rm = TRUE), max(this_df$s_coordinate, na.rm = TRUE)))
  
  splot_crc <- ggplot(data = this_df_crc, aes(x = g_coordinate, y = s_coordinate, color = as.factor(concentration_u_m))) +
    ggtitle(paste(this_compound, "CRC media")) +
    facet_wrap(~experiment_date, nrow = 3, ncol = 3) +
    theme_bw() +
    geom_point(alpha = .6, size = 3) +
    scale_color_viridis_d(end = .9) +
    labs(x = "G", y = "S", color = "Dose (uM)") +
    xlim(c(min(this_df$g_coordinate, na.rm = TRUE), max(this_df$g_coordinate, na.rm = TRUE))) +
    ylim(c(min(this_df$s_coordinate, na.rm = TRUE), max(this_df$s_coordinate, na.rm = TRUE)))
  
  
  #second, we want to run a hypothesis test to compare the 
    #euclidean distance between control and treatment conditions
    #and compare that between caf/crc
  #we already have the distance in the this_df data frame
  
  #this will be a t test between caf and crc conditions
    #within each concentration
    #so we'll have to adjust the p values
  
  #before we even START doing that we gotta average all the organoids!!
  this_df_avgd <- this_df %>%
    group_by(experiment_date, concentration_u_m, media_type) %>%
    dplyr::summarise(dist = median(dist, na.rm = TRUE),
                     g_0um = median(g_0um, na.rm = TRUE),
                     s_0um = median(s_0um, na.rm = TRUE)) %>%
    #don't want any 0 concentrations in our df for the t tests and tables
    filter(concentration_u_m > 0) %>%
    filter(!is.na(dist))
  
  t_results <- this_df_avgd %>% 
    #need more than one row for each concentration and media type
    group_by(concentration_u_m, media_type) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    group_by(concentration_u_m) %>%
    filter(n_distinct(media_type) > 1) %>%
    do(bartlett_ps = bartlett.test(dist ~ media_type, .)$p.value)
  t_results$bartlett_ps <- as.numeric(t_results$bartlett_ps)

  #might be cool to add a sanity check plot at some point in here?
  t_tests <- as.numeric(lapply(t_results$concentration_u_m, dist_t_tester, this_df_avgd))
  wilcox_ps <- as.numeric(lapply(t_results$concentration_u_m, 
                     function(this_conc) wilcox.test(dist ~ media_type, this_df_avgd[this_df_avgd$concentration_u_m == this_conc,])$p.value))
  t_results["t_ps"] <- t_tests
  t_results["wilcox_ps"] <- wilcox_ps
  t_results <- t_results %>%
    mutate(t_ps = case_when(bartlett_ps < .05 ~ wilcox_ps,
                            TRUE ~ t_ps)) %>%
    dplyr::select(-wilcox_ps)
  t_results["p_adj"] <- p.adjust(t_results$t_ps)
  names(t_results) <- c("Dose (uM)", "Bartlett P-val", "T-test P-val", "Adj P")
  
  #ok now make some cutie cute cute cute output tables
  #we're gonna wanna use this df this_df_avgd
  this_df_avgd <- this_df_avgd %>%
    dplyr::select(experiment_date, concentration_u_m, media_type, dist) %>%
    arrange(concentration_u_m, experiment_date)
  
  this_df_avgd_crc <- this_df_avgd %>%
    filter(media_type == "CRC media") %>%
    dplyr::select(-media_type)
    
  names(this_df_avgd_crc) <- c("Experiment Date", "Dose (uM)", "Distance from Median No Treatment Point")
  
  this_df_avgd_caf <- this_df_avgd %>%
    filter(media_type == "CAF-CM") %>%
    dplyr::select(-media_type)
  
  names(this_df_avgd_caf) <- c("Experiment Date", "Dose (uM)", "Distance from Median No Treatment Point")
  
  
  return_list <- list("caf_scatter" = splot_caf,
                      "crc_scatter" = splot_crc,
                      "t_results" = t_results,
                      "crc_table" = this_df_avgd_crc,
                      "caf_table" = this_df_avgd_caf)
}

get_anova_results <- function(this_df, batch_corr = FALSE) {
  
  #ok anova time
  #step 1 levene's test for equal variance
  test_results <- this_df %>%
    group_by(media_type, concentration_u_m) %>%
    mutate(n_rep = n()) %>%
    ungroup() %>%
    filter(n_rep > 1) %>%
    group_by(media_type) %>%
    #get results of levene tests
    #originally started collecting levene results, but 
    #bartlett's is better for normal data, so I use that moving forward
    do(levene_res_fb = leveneTest(fraction_bound ~ as.factor(concentration_u_m), .),
       bartlett_res_fb = bartlett.test(fraction_bound ~ as.factor(concentration_u_m), .),
       #get results of anova tests
       aov_res_fb = aov(fraction_bound ~ as.factor(concentration_u_m), .),
       welch_res_fb = oneway.test(fraction_bound ~ as.factor(concentration_u_m), data = ., var.equal = FALSE),
       game_howell_fb = games_howell_test(fraction_bound ~ concentration_u_m, data = .))
  
  check_norm <- performance::check_normality(aov(fraction_bound ~ as.factor(concentration_u_m), this_df))
  #now do tukey's
  tukeys_fb <- lapply(test_results$aov_res_fb, TukeyHSD)
  
  #grab the p values out of here....
  #well... p values for the levene test, the whole table for the anova test
  test_results_output <- data.frame(matrix(ncol = 3, nrow = 0))
  names(test_results_output) <- c("condition", "bartlett", "aov")
  
  #we also want the anova
  #pvalues within their own list
  #so that we can print the post hoc tests if those p vals are less than .05 (significant)
  # aov_ps <- c()
  #jk no we don't we can grab them from the output table very easily!
  #also need the post hoc results in their own list to print as needed
  tukeys_tables <- c()
  g_h_results <- c()
  for (j in 1:nrow(test_results)) {
    output_i <- nrow(test_results_output) + 1
    this_media <- test_results$media_type[[j]]
    this_condition <- paste(this_media, "FB")
    test_results_output[output_i, "condition"] <- this_condition
    
    bartlett_result <- test_results$bartlett_res_fb[[j]]$p.value
    test_results_output[output_i, "bartlett"] <- bartlett_result
    
    #if bartlett's test results are significant, use welch's anova
    #instead of the normal one
    #and the game-howell test for post hoc
    if (bartlett_result < .05) {
      welchs_p <- test_results$welch_res_fb[[j]]$p.value
      welchs_f <- test_results$welch_res_fb[[j]]$statistic[[1]]
      welchs_df <- test_results$welch_res_fb[[j]]$parameter[[1]]
      # aov_ps[[this_condition]] <- welchs_p
      test_results_output[output_i, "aov_pval"] <- welchs_p
      test_results_output[output_i, "aov_fstat"] <- welchs_f
      test_results_output[output_i, "aov_df"] <- welchs_df
      #write to excel output
      if (batch_corr == TRUE) {
        write.xlsx(test_results_output[output_i,], here("data_output", "flim_analysis_output.xlsx"), 
                   sheet = paste(i, this_condition, "Welch"), row.names = FALSE,
                   append = TRUE)
      }
      
    } else {
      trad_aov_p <- summary(test_results$aov_res_fb[[j]])[[1]][["Pr(>F)"]][1]
      trad_aov_f <- summary(test_results$aov_res_fb[[j]])[[1]][["F value"]][[1]]
      trad_aov_df <- summary(test_results$aov_res_fb[[j]])[[1]][["Df"]][[1]]
      # aov_ps[[this_condition]] <- trad_aov_p
      test_results_output[output_i, "aov_pval"] <- trad_aov_p
      test_results_output[output_i, "aov_fstat"] <- trad_aov_f
      test_results_output[output_i, "aov_df"] <- trad_aov_df
      #write to excel output
      if (batch_corr == TRUE) {
        write.xlsx(test_results_output[output_i,], here("data_output", "flim_analysis_output.xlsx"), 
                   sheet = paste(i, this_condition, "AOV"), row.names = FALSE,
                   append = TRUE)
      }
      
    }
    
    g_h_results[[this_condition]] <- as.data.frame(test_results$game_howell_fb[[j]])
    # %>%
    #   filter(p.adj < .05)
    
    tukeys_tables[[this_condition]] <- as.data.frame(tukeys_fb[[j]]$`as.factor(concentration_u_m)`) %>%
      clean_names()
    # %>%
    #   filter(p_adj < .05)
  }
  
  return_list <- list("check_norm" = check_norm,
                      "test_results_output" = test_results_output,
                      "g_h_results" = g_h_results,
                      "tukeys_tables" = tukeys_tables)
  
  return(return_list)
}
