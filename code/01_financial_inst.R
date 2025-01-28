source("code/00_dependencies.R")
source("code/99_functions.R")

# load country name change lookup table
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# load market cap country names from Table 1
table_01_countries <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 1") |> 
  select(country) |> 
  distinct() |> 
  filter(row_number() < 190)


# Private Credit to GDP Ratio (privo) -------------------------------------

privo <- read_excel("data/private_credit_data.xlsx") |> 
  clean_names() |> 
  select(country_name, x2018:x2022) |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, ".."))) |> 
  mutate(
    x2018 = as.numeric(x2018),
    x2019 = as.numeric(x2019),
    x2020 = as.numeric(x2020),
    x2021 = as.numeric(x2021),
    x2022 = as.numeric(x2022)
  )

privo_avg <- privo |> 
  rowwise() |> 
  mutate(privo = mean(c(x2018, x2019, x2020, x2021, x2022), na.rm = TRUE) / 100) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  select(country, privo)

updated_privo <- left_join(table_01_countries, privo_avg)
write_xlsx(updated_privo, path = "output/updated_privo.xlsx")  
