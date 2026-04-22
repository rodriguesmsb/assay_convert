library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)

# Collapse duplicate wells for the same sample/dilution
# This keeps the dilution curve intact
collapse_samples <- function(df) {
  df %>%
    mutate(value_copy = value) %>% 
    group_by(sample,dilution, file_name, assay) %>%
    summarise(
      # If there are replicate wells, average their values
      value = mean(value, na.rm = TRUE),
      
      # compute CV across replicates for traceability
      cv = sd(value_copy, na.rm = TRUE)/mean(value_copy, na.rm = TRUE) * 100,
      
      # Keep a representative position string for traceability
      position = paste(position, collapse = "; "),
      .groups = "drop"
    )
}

# Function to convert Elisa plate data into long format
create_elisa_long <- function(values_df, map_df, file_path, assay_name) {
  
  # Standardize first column name
  names(values_df)[1] <- "plate_row"
  names(map_df)[1] <- "plate_row"
  
  # Build dilution lookup from row A in the map sheet
  dilution_lookup <- map_df %>%
    filter(plate_row == "A") %>%
    pivot_longer(
      cols = -plate_row,
      names_to = "plate_col",
      values_to = "dilution"
    ) %>%
    mutate(
      plate_col = as.character(plate_col)
    ) %>%
    select(plate_col, dilution)
  
  # Convert value sheet to long format
  values_long <- values_df %>%
    filter(plate_row != "A") %>%
    pivot_longer(
      cols = -plate_row,
      names_to = "plate_col",
      values_to = "value"
    ) %>%
    mutate(
      plate_col = as.character(plate_col),
      position = paste0(plate_row, plate_col)
    ) %>%
    select(position, plate_row, plate_col, value)
  
  # Convert map sheet to long format
  sample_long <- map_df %>%
    filter(plate_row != "A") %>%
    pivot_longer(
      cols = -plate_row,
      names_to = "plate_col",
      values_to = "sample"
    ) %>%
    mutate(
      plate_col = as.character(plate_col),
      position = paste0(plate_row, plate_col)
    ) %>%
    select(position, plate_row, plate_col, sample)
  
  # Join everything together and add file name + assay name
  final_long <- sample_long %>%
    left_join(values_long, by = c("position", "plate_row", "plate_col")) %>%
    left_join(dilution_lookup, by = "plate_col") %>%
    mutate(
      file_name = basename(file_path),
      assay = assay_name
    ) %>%
    select(sample, position, value, dilution, assay, file_name)
  
  # Remove only the final dilution suffix from the sample name
  # Example:
  # E80-1558-E1-CV-W00_100  -> E80-1558-E1-CV-W00
  final_long <- final_long %>%
    mutate(
      sample = str_remove(sample, "_[0-9]+$")
    )
  
  # Collapse replicate wells while preserving dilution
  final_long <- collapse_samples(final_long)
  
  return(final_long)
}


# create a function to process a single excel file
# Process a single Excel file end-to-end and return long-format data.
process_elisa_file <- function(file_path) {
  sheets <- excel_sheets(file_path)
  value_matches <- grep("_value$", sheets, value = TRUE)
  map_matches   <- grep("_map$",   sheets, value = TRUE)
  
  if (length(value_matches) != 1) {
    stop(sprintf("File '%s': expected one '_value' sheet, found %d.",
                 basename(file_path), length(value_matches)))
  }
  if (length(map_matches) != 1) {
    stop(sprintf("File '%s': expected one '_map' sheet, found %d.",
                 basename(file_path), length(map_matches)))
  }
  
  values_df <- read_excel(file_path, sheet = value_matches[1])
  map_df    <- read_excel(file_path, sheet = map_matches[1])
  
  if (ncol(values_df) != ncol(map_df) || nrow(values_df) != nrow(map_df)) {
    stop(sprintf("File '%s': value and map sheets have different dimensions.",
                 basename(file_path)))
  }
  
  create_elisa_long(
    values_df  = values_df,
    map_df     = map_df,
    file_path  = file_path,
    assay_name = sub("_value$", "", value_matches[1])
  )
}



# Helper function to compute trapezoidal AUC
compute_trapz_auc <- function(x, y) {
  
  # Keep only complete pairs
  keep <- complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]
  
  # At least 2 points are needed to compute an area
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  # Order points by x so the area is computed correctly
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  #convert y to log scale if not already
  if (any(x <= 0)) {
    x <- log(x + 1) # add 1 to avoid log(0)
  } else {
    x <- log(x)
  }
  
  # Trapezoidal rule
  auc <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  
  return(auc)
}

# Function to compute AUC for each sample
create_auc_df <- function(long_df) {
  
  auc_df <- long_df %>%
    mutate(
      dilution_num = parse_number(as.character(dilution)),
      value = as.numeric(value)
    ) %>%
    
    # Compute one AUC per sample
    group_by(sample, file_name, assay) %>%
    summarise(
      AUC = compute_trapz_auc(dilution_num, value),
      .groups = "drop"
    ) %>%
    
    # Keep requested columns
    select(sample, AUC, file_name, assay)
  
  return(auc_df)
}