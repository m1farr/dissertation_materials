source("code/00_dependencies.R")
source("code/99_functions.R")

# load country name change lookup table
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# load country name list for Table 4 (Culture)
table_02_countries <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 2") |> 
  select(country) |>
  distinct() |> 
  filter(row_number() < 160)

# Corruption (WGI) --------------------------------------------------------

corruption_tbl <- read_excel("data/wgi_dataset.xlsx") |> 
  select(countryname, year, indicator, estimate) |> 
  rename(country = countryname) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_wgi, country_name_changes))) |> 
  filter(indicator == "cc") |> 
  filter(year >= 2018 & year < 2023) |> 
  select(-indicator) |> 
  rename(corruption = estimate) |> 
  group_by(country) |> 
  mutate(corruption = as.numeric(corruption)) |> 
  mutate(corruption_avg = mean(corruption)) |> 
  select(country, corruption_avg) |> 
  distinct()

updated_corruption <- left_join(table_02_countries, corruption_tbl)
write_xlsx(updated_corruption, path = "output/updated_corruption.xlsx")
