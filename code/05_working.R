# GDP per capita ----------------------------------------------------------
  mutate(ln_gdp_2020 = log(gdp_pc_2020)) |> 
  select(country, ln_gdp_2020)

temp <- left_join(country_list, gdp_pc)


# Mcap time series 2000-2022 ----------------------------------------------

ts_data <- wdi_mcap_data |> 
  select(country, x2000:x2022) |> 
  left_join(legal_origins) |> 
  relocate(legal_origin, .after = country) |> 
  group_by(legal_origin) |> 
  sapply(mean(x2000:x2022))