# GDP per capita ----------------------------------------------------------

gdp_pc <- read_excel("data/temp_gdp_pc_2020.xlsx") |> 
  clean_names() |> 
  rename(country = country_name, 
         gdp_pc_2020 = x2020_yr2020) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_wdi, country_name_changes))) |> 
  mutate(ln_gdp_2020 = log(gdp_pc_2020)) |> 
  select(country, ln_gdp_2020)

temp <- left_join(country_list, gdp_pc)