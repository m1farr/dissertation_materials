source("code/00_dependencies.R")
source("code/99_functions.R")

# load country name change lookup table
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# load market cap country names from Table 1
table_01_countries <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 1") |> 
  select(country) |> 
  distinct() |> 
  filter(row_number() < 190)


# Market Cap --------------------------------------------------------------

legal_origins <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 1") |> 
  select(country, legor_uk:legor_so) |> 
  distinct() |> 
  filter(row_number() < 190) |> 
  mutate(
    legal_origin = case_when(
      legor_uk == 1 ~ "English",
      legor_fr == 1 ~ "French",
      legor_ge == 1 ~ "German",
      legor_sc == 1 ~ "Scandinavian",
      legor_so == 1 ~ "Socialist"
    )) |> 
  select(country, legal_origin)

# load market cap data 2000-2022 from WDI
wdi_mcap_data <- read_excel("data/wdi_mcap_raw.xlsx") |> 
  clean_names() |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, "..")))

recent_mcap <- wdi_mcap_data |> 
  select(country, x2018:x2022) |> 
  mutate(
    x2018 = as.numeric(x2018),
    x2019 = as.numeric(x2019),
    x2020 = as.numeric(x2020),
    x2021 = as.numeric(x2021),
    x2022 = as.numeric(x2022)
  )

updated_mcap <- left_join(table_01_countries, recent_mcap) |> 
  rowwise() |> 
  mutate(mcap = mean(c(x2018, x2019, x2020, x2021, x2022), na.rm = TRUE)) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  select(country, mcap)

write_xlsx(updated_mcap, path = "output/updated_mcap.xlsx")


# Time series 2000-2022 ---------------------------------------------------

ts_data <- wdi_mcap_data |> 
  select(country, x2000:x2022) |> 
  left_join(legal_origins) |> 
  relocate(legal_origin, .after = country) |> 
  group_by(legal_origin) |> 
  sapply(mean(x2000:x2022))

# mutate map takingaverage per year of mcap 

# Ln of average number of domestic firms per capita, 2018-2022 ------------

firms_per_capita <- read_excel("data/wdi_firms_per_capita.xlsx") |> 
  clean_names() |> 
  rename(country = country_name) |> 
  select(country, series_name, x2018:x2022) |> 
  filter(row_number() < 436) |> 
  mutate(across(everything(), ~ na_if(.x, ".."))) |> 
  mutate(
    series_name = str_replace_all(series_name, "Listed domestic companies, total", "firms"),
    series_name = str_replace_all(series_name, "Population, total", "pop")
  ) |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    names_prefix = "x"
  ) |> 
  pivot_wider(
    names_from = series_name,
    values_from = value
  ) |> 
  mutate(
    firms = as.numeric(firms),
    pop = as.numeric(pop)
  ) |> 
  select(-`NA`) |> 
  mutate(
    firms_per_capita = (firms / pop) * 100000
  )

ln_firms_per_capita <- firms_per_capita |> 
  group_by(country) |> 
  summarize(average_firms_pc = mean(firms_per_capita, na.rm = TRUE)) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  mutate(ln_average_firms_pc = log(average_firms_pc)) |> 
  select(-average_firms_pc)

updated_firms <- left_join(table_01_countries, ln_firms_per_capita)

write_xlsx(updated_firms, path = "output/updated_firms.xlsx")
