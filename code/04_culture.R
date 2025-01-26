source("code/00_dependencies.R")
source("code/99_functions.R")

# load country name change lookup table
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# load country name list for Table 4 (Culture)
table_04_countries <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 4") |> 
  select(country) |>
  distinct()

# Cultural Dimensions (Hofstede: pdi, idv, uai, & mas) -------------------------

dimensions_tbl <- read_excel("data/hofstede_dimensions.xls") |> 
  select(country:uai) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_hof, country_name_changes)))

updated_dimensions <- left_join(table_04_countries, dimensions_tbl)
write_xlsx(updated_dimensions, path = "output/updated_dimensions.xlsx")