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
change_names <- function(name, name_new, n_tbl){
  if(name %in% (n_tbl |> pull({{ name_new }}))){
    ret_val <- n_tbl |> filter( {{ name_new }} == name) |> pull(name_old)
  } else {
    ret_val <- name
  }
}


# add appropriate missing country names back to table
