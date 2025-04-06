source("code/01_load_files.R")

# Anti-self-dealing index -------------------------------------------------

updated_anti_sd <- doing_business |> 
  select(country, db_year, score_protecting_minority_investors_db15_20_methodology) |> 
  filter(db_year == 2020) |> 
  select(-db_year) |> 
  rename(anti_sd = score_protecting_minority_investors_db15_20_methodology)

final_anti_sd <- left_join(country_list, updated_anti_sd)


# Disclosure --------------------------------------------------------------

disclosure <- doing_business |> 
  select(country,db_year, extent_of_disclosure_index_0_10) |> 
  filter(db_year == 2020) |> 
  select(-db_year) |> 
  rename(disclose = extent_of_disclosure_index_0_10)

updated_disclose <- left_join(country_list, disclosure)


# Creditor Rights 2020 for Table 1 --------------------------------------------

cr_2020 <- doing_business |> 
  select(country, db_year, strength_of_insolvency_framework_index_0_16) |> 
  filter(db_year == 2020) |> 
  select(-db_year) |> 
  rename(cr_2020 = strength_of_insolvency_framework_index_0_16)

updated_cr_2020 <- left_join(country_list, cr_2020)


# Case A Efficiency -------------------------------------------------------

case_a_efficiency <- doing_business |> 
  select(country, db_year, score_enforcing_contracts_db17_20_methodology, 
         score_resolving_insolvency) |> 
  rename(enforce_contracts = score_enforcing_contracts_db17_20_methodology, 
         resolve_insolvency = score_resolving_insolvency) |> 
  filter(db_year == 2020) |> 
  mutate(case_a_efficiency = (enforce_contracts + resolve_insolvency)/2) |> 
  select(country, case_a_efficiency)

updated_case_a <- left_join(country_list, case_a_efficiency)


# Government ownership of banks -------------------------------------------

boatw_data <- read_excel("data/boatw_dataset.xlsx") |> 
  clean_names()

gbbp_20 <- boatw_data |> 
  select(iso, year, country, soe1_db) |> 
  filter(year == 2020) |> 
  mutate(country = str_to_title(country)) |> 
  rename(country_name = country,
         code         = iso)

updated_gbbp <- left_join(country_list_codes, gbbp_20) |> 
  select(country, soe1_db)


# Market Cap --------------------------------------------------------------

# load market cap data 2000-2022 from WDI
wdi_mcap_data <- read_excel("data/wdi_mcap_raw.xlsx") |> 
  clean_names() |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, "..")))

recent_mcap <- wdi_mcap_data |> 
  select(country, x2018:x2022) |> 
  mutate(across(x2018:x2022, ~ as.numeric(.x)))

updated_mcap <- left_join(country_list, recent_mcap) |> 
  rowwise() |> 
  mutate(mcap = mean(c(x2018, x2019, x2020, x2021, x2022), na.rm = TRUE)) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  select(country, mcap)


# Mcap time series 2000-2022 ----------------------------------------------

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
    cols         = starts_with("x"),
    names_to     = "year",
    names_prefix = "x"
  ) |> 
  pivot_wider(
    names_from  = series_name,
    values_from = value
  ) |> 
  mutate(
    firms = as.numeric(firms),
    pop   = as.numeric(pop)
  ) |> 
  select(-`NA`) |> 
  mutate(
    firms_per_capita = (firms / pop) * 100000
  )


# Firms per capita --------------------------------------------------------

ln_firms_per_capita <- firms_per_capita |> 
  group_by(country) |> 
  summarize(average_firms_pc = mean(firms_per_capita, na.rm = TRUE)) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  mutate(ln_average_firms_pc = log(average_firms_pc)) |> 
  select(-average_firms_pc)

updated_firms <- left_join(country_list, ln_firms_per_capita)


# Ownership concentration -------------------------------------------------

oecd_ownership <- read_excel("data/oecd_transcribed.xlsx", 
                             sheet = "Ownership (company) by investor") |> 
  clean_names() |> 
  select(country, top_3_investors) |> 
  mutate(top_3_investors = top_3_investors/100) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_oecd, country_name_changes)))

updated_concentr <- left_join(country_list, oecd_ownership)


# Median block premia -----------------------------------------------------

orbis_data <- read_excel("data/orbis_ma_data.xlsx", sheet = "Results") |> 
  clean_names()

complete_orbis <- orbis_data |> 
  filter(deal_status == "Completed") |> 
  mutate(across(target_stock_price_prior_to_announcement_usd:offer_price_usd, 
                ~ na_if(.x, "n.a."))) |> 
  filter(!is.na(target_stock_price_prior_to_announcement_usd) 
         & !is.na(offer_price_usd)) |>
  mutate(offer_price_usd = as.numeric(offer_price_usd),
         target_stock_price_prior_to_announcement_usd = as.numeric(target_stock_price_prior_to_announcement_usd),
         final_stake_percent = as.numeric(final_stake_percent)) |> 
  mutate(bp = 
           (offer_price_usd - target_stock_price_prior_to_announcement_usd)/
           target_stock_price_prior_to_announcement_usd * (final_stake_percent/100)) |> 
  rename(country = target_country)

bp_med <- complete_orbis |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_orbis, country_name_changes))) |> 
  group_by(country) |> 
  mutate(bp_med = median(bp)) |> 
  select(country, bp_med) |> 
  distinct()

bp_med_tbl <- complete_orbis |> 
  group_by(country) |> 
  summarise(
    Mean         = mean(bp, na.rm = TRUE),
    Median       = median(bp, na.rm = TRUE),
    SD           = sd(bp, na.rm = TRUE),
    Minimum      = min(bp, na.rm = TRUE),
    Maximum      = max(bp, na.rm = TRUE),
    Observations = n(),
    PositiveObs  = sum(bp > 0, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
  filter(Observations > 1)

updated_bp_med <- bp_med_tbl |> 
  select(country, Median) |> 
  rename(bp_med = Median)

updated_bp_med <- left_join(country_list, updated_bp_med)


# Private Credit to GDP Ratio (privo) -------------------------------------

privo <- read_excel("data/private_credit_data.xlsx") |> 
  clean_names() |> 
  select(country_name, x2018:x2022) |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, ".."))) |> 
  mutate(across(x2018:x2022, ~ as.numeric(.x)))

privo_avg <- privo |> 
  rowwise() |> 
  mutate(privo = mean(c(x2018, x2019, x2020, x2021, x2022), na.rm = TRUE) / 100) |> 
  mutate_all(~ifelse(is.nan(.), NA, .)) |> 
  select(country, privo)

updated_privo <- left_join(country_list, privo_avg)


# Interest rate spread ----------------------------------------------------

interest_spread <- read_excel("data/wdi_spread_data.xlsx") |> 
  clean_names() |> 
  select(country_name, x1999:x2023) |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, ".."))) |> 
  mutate(across(x1999:x2023, ~ as.numeric(.x))) |> 
  rowwise() |> 
  mutate(mean_spread = mean(c_across(x1999:x2023), na.rm = TRUE)) |> 
  ungroup() |> 
  select(country, mean_spread) |> 
  filter(!is.na(mean_spread))

updated_spread <- left_join(country_list, interest_spread)


# Ln Steps (Starting Business) --------------------------------------------

ln_steps <- doing_business |> 
  select(country, db_year, procedures_men_number, procedures_women_number) |> 
  mutate(
    procedures_men_number = as.numeric(procedures_men_number),
    procedures_women_number = as.numeric(procedures_women_number)
  ) |> 
  mutate(ln_steps = (procedures_men_number + procedures_women_number)/2) |> 
  mutate(ln_steps = round(ln_steps, digits = 1)) |> 
  filter(db_year == 2020) |> 
  select(country, ln_steps)

updated_ln_steps <- left_join(country_list, ln_steps)

# Military conscription ---------------------------------------------------

conscription_tbl <- read_excel("data/m3_conscription_data.xlsx", na = "NA") |> 
  select(year, countryname_standard, com_mil_serv) |> 
  filter(year == 2020) |> 
  filter(!is.na(com_mil_serv)) |> 
  select(-year) |> 
  rename(country = countryname_standard)

updated_conscription <- left_join(country_list, conscription_tbl)

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

updated_corruption <- left_join(country_list, corruption_tbl)


# Unofficial Employment ---------------------------------------------------

unofficial_size_dge <- read_excel("data/informal_economy_database.xlsx", sheet = "DGE_p") |> 
  clean_names() |> 
  select(economy, x2000) |> 
  rename(country = economy,
         dge_2000 = x2000)

unofficial_size_mimic <- read_excel("data/informal_economy_database.xlsx", sheet = "MIMIC_p") |> 
  clean_names() |> 
  select(economy, x2000) |> 
  rename(country = economy,
         mimic_2000 = x2000)

unofficial_size_orig <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 2") |> 
  select(country, code, employmentunofficial) |> 
  filter(row_number() < 160) |> 
  mutate(across(everything(), ~ na_if(.x, "."))) |> 
  filter(!is.na(employmentunofficial))

temp <- left_join(unofficial_size_orig, unofficial_size_dge) |> 
  left_join(unofficial_size_mimic)


# Property rights ---------------------------------------------------------

ef_index <- read_excel("data/ef_index_data.xlsx", skip = 1) |> 
  clean_names() |> 
  select(country, property_rights) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_ef, country_name_changes))) |> 
  mutate(across(everything(), ~ na_if(.x, "N/A")))

updated_property <- left_join(country_list, ef_index)


# Creditor Rights 2020 for Table 4 ----------------------------------------

cr_2010 <- doing_business |> 
  select(country, db_year, strength_of_insolvency_framework_index_0_16) |> 
  filter(db_year == 2010) |> 
  select(-db_year) |> 
  rename(cr_2010 = strength_of_insolvency_framework_index_0_16)

table_04_cr <- left_join(country_list, cr_2020) |> 
  left_join(cr_2010)


# Cultural Dimensions (Hofstede: pdi, idv, uai, & mas) -------------------------

dimensions_tbl <- read_excel("data/hofstede_dimensions.xls") |> 
  select(country:uai) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_hof, country_name_changes)))

updated_dimensions <- left_join(country_list, dimensions_tbl)


# Union Density (union_dens)----------------------------------------------------

new_union_dens <- read_excel("data/union_dens_pulled.xlsx") |> 
  clean_names() |> 
  select(ref_area_label, time, obs_value) |> 
  rename(country = ref_area_label) |> 
  mutate(obs_value = obs_value/100) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_ud, country_name_changes))) |> 
  filter(time == "2018")

updated_union_dens <- left_join(country_list, new_union_dens) |> 
  select(-time)


# I used this plot to visualize the amount of countries with union density data grouped
# by year in order to select the most appropriate year for my variable. 

# union_dens_bar_plot <- update_union_dens |> 
#   group_by(time) |> 
#   select(-obs_value) |> 
#   ggplot(aes(time)) +
#   geom_bar()
