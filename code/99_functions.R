# convert all sheets from excel file into tibbles
read_workbook <- function(file){
  sheets <- excel_sheets(file)
  x <- lapply(sheets, function(X) read_excel(file, sheet = X)) |> 
    clean_names()
  names(x) <- sheets
  list2env(x, envir = .GlobalEnv)
  as_tibble(x)
}

# change country names where they don't match between tables
change_names <- function(name, n_tbl){
  if(name %in% n_tbl$name_old) {
    ret_val <- n_tbl |> filter(name_old == name) |> pull(name_new)
  } else {
    ret_val <- name
  }
}
