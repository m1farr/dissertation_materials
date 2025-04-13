# convert all sheets from excel file into tibbles
read_datasets_as_tables <- function(folder, sheet_mapping, name_change,
                                    country_name_changes) {
  
  # Get list of files
  files <- list.files(path = folder, full.names = TRUE)
  
  # Loop through each file
  for (file in files) {
    # Get the base name without extension
    base_name <- file_path_sans_ext(basename(file))
    file_ext  <- file_ext(file)
    
    # Determine which sheet to read
    sheet_to_read <- sheet_mapping[[base_name]]
    
    
    if(file_ext == "xlsx"){
      
      # Read the Excel file (use specified sheet if available, else default to first)
      if (!is.null(sheet_to_read)) {
        
        # Skip unnecessary rows for Doing Business data
        if(base_name == "doing_business_data"){
          data <- read_excel(file, sheet = sheet_to_read, skip = 3) |> 
            clean_names() |> 
            rename(
              construction_procedures       = procedures_number_29,
              construction_score_procedures = score_procedures_number_30,
              construction_time             = time_days_31,
              construction_score_time       = score_time_days_32,
              electricity_procedures        = procedures_number_46,
              electricity_score_procedures  = score_procedures_number_47,
              electricity_time              = time_days_48,
              electricity_score_time        = score_time_days_49,
              property_procedures           = procedures_number_67,
              property_score_procedures     = score_procedures_number_68,
              property_time                 = time_days_69,
              property_score_time           = score_time_days_70,
              contract_time                 = time_days_171,
              contract_score_time           = score_time_days_172
            )
        } else if (base_name == "wjp_data") {
          data <- read_excel(file, sheet = sheet_to_read, col_names = FALSE) |> 
            filter(!is.na(`...1`))
        } else {
          data <- read_excel(file, sheet = sheet_to_read) |> 
            clean_names()
        }
      } else {
        
        # Skip unnecessary rows for Heritage data
        if(base_name == "heritage_data"){
          data <- read_excel(file, skip = 4) |> 
            clean_names()
        } else {
          data <- read_excel(file) |> 
            clean_names()
        }
      }
    } else if(file_ext == "csv") {
      data <- read.csv(file) |> 
        clean_names()
    }
    
    # Assign to variable in global environment
    assign(base_name, data, envir = .GlobalEnv)
  }
}


# change country names where they don't match between tables
change_names <- function(name, name_new, n_tbl){
  
  match <- n_tbl |> 
    filter(!is.na({{ name_new }}), {{ name_new }} == name) |> 
    pull(name_wdi)
  
  if (!is.na(match) && length(match) > 0) {
    return(match)
  } else {
    return(name)
  }
}


# Read and combine all of the sheets in an Excel file into one table

combine_excel_sheets <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  sheet_names <- setdiff(sheet_names, "Read me")
  
  combined_data <- map_dfr(sheet_names, ~ {
    read_excel(file_path, sheet = .x) |> 
      clean_names() |> 
      select(economy, x2017) |> 
      mutate(sheet = .x)
  })
  
  return(combined_data)
}

# Get country-level variable means for WDI variables

get_wdi_mean <- function(name, start_year, end_year, wdi, country_list) {
  # Create a symbol from the column name string
  var_sym <- sym(name)
  
  # Create a name for the output
  updated_name <- paste0("mean_", name)
  
  # Calculate the mean
  data <- wdi |> 
    filter(year >= start_year & year <= end_year) |> 
    group_by(country, country_code) |> 
    rename(code = country_code) |> 
    summarise(mean = mean(!!var_sym, na.rm = TRUE), .groups = "drop") |> 
    mutate_all(~ifelse(is.nan(.), NA, .)) |> 
    rename(!!var_sym := mean)
  
  data <- left_join(country_list, data)
  
  return(data)
}

# Get variable from Doing Business

get_db <- function(name, var, year, db_data, country_list){
  
  var_sym <- sym(var)
  name_sym <- sym(name)
  
  data <- db_data |> 
    select(country, db_year, !!var_sym) |> 
    filter(db_year == year) |> 
    select(-db_year) |> 
    rename(!!name_sym := !!var_sym)
  
  data <- left_join(country_list, data)
  
  return(data)
  
}
