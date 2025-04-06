# convert all sheets from excel file into tibbles
read_datasets_as_tables <- function(folder = "data", sheet_mapping = list()) {
  
  # Get list of Excel files
  files <- list.files(path = folder, pattern = "\\.xlsx?$", full.names = TRUE)
  
  # Loop through each file
  for (file in files) {
    # Get the base name without extension
    base_name <- tools::file_path_sans_ext(basename(file))
    
    # Determine which sheet to read
    sheet_to_read <- sheet_mapping[[base_name]]
    
    # Read the Excel file (use specified sheet if available, else default to first)
    if (!is.null(sheet_to_read)) {
      data <- read_excel(file, sheet = sheet_to_read)
    } else {
      data <- read_excel(file)
    }
    
    # Assign to variable in global environment
    assign(base_name, data, envir = .GlobalEnv)
  }
}


# change country names where they don't match between tables
change_names <- function(name, name_new, n_tbl){
  if(name %in% (n_tbl |> pull({{ name_new }}))){
    ret_val <- n_tbl |> filter( {{ name_new }} == name) |> pull(name_old)
  } else {
    ret_val <- name
  }
}