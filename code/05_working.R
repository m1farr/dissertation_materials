# GDP per capita ----------------------------------------------------------
  mutate(ln_gdp_2020 = log(gdp_pc_2020)) |> 
  select(country, ln_gdp_2020)

temp <- left_join(country_list, gdp_pc)


# Mcap time series 2000-2022 ----------------------------------------------


