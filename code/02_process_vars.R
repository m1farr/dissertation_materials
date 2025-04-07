source("code/01_load_data.R")

# Process variables from World Development Indicators ---------------------

# Variables include market cap, ln firms per capita, private credit ratio,
# interest rate spread, and GDP per capita (PPP-adjusted)

updated_spread <- get_wdi_mean("intr_spread", 1999, 2023, wdi, country_list)
updated_privo <- get_wdi_mean("privo", 2018, 2022, wdi, country_list)
updated_mcap <- get_wdi_mean("mcap", 2018, 2022, wdi, country_list)
updated_firms <- get_wdi_mean("ln_firms_pc", 2018, 2022, wdi, country_list)
# get_wdi_mean("gdp_pc")


# Process variables from World Bank's Doing Business ----------------------

# Variables include anti-self-dealing index (anti_sd), disclosure, creditor
# rights (cr_2020 and cr_2010), case a efficiency, ln steps

updated_anti_sd <- get_db("anti_sd", 
                          "score_protecting_minority_investors_db15_20_methodology",
                          2020,
                          doing_business_data,
                          country_list)

updated_disclose <- get_db("disclose", 
                           "extent_of_disclosure_index_0_10", 
                           2020,
                           doing_business_data,
                           country_list)

updated_cr_2020 <- get_db("cr_2020", 
                          "strength_of_insolvency_framework_index_0_16",
                          2020,
                          doing_business_data,
                          country_list)

updated_cr_2010 <- get_db("cr_2010",
                          "strength_of_insolvency_framework_index_0_16",
                          2010,
                          doing_business_data,
                          country_list)


# Case A Efficiency -------------------------------------------------------

case_a_efficiency <- doing_business_data |> 
  select(economy, db_year, score_enforcing_contracts_db17_20_methodology, 
         score_resolving_insolvency) |> 
  rename(country = economy,
         enforce_contracts = score_enforcing_contracts_db17_20_methodology, 
         resolve_insolvency = score_resolving_insolvency) |> 
  filter(db_year == 2020) |> 
  mutate(case_a_efficiency = (enforce_contracts + resolve_insolvency)/2) |> 
  select(country, case_a_efficiency)


# Ln Steps (Starting Business) --------------------------------------------

ln_steps <- doing_business_data |> 
  select(economy, db_year, procedures_men_number, procedures_women_number) |> 
  rename(country = economy) |> 
  mutate(
    procedures_men_number   = as.numeric(procedures_men_number),
    procedures_women_number = as.numeric(procedures_women_number)
  ) |> 
  mutate(ln_steps = (procedures_men_number + procedures_women_number)/2) |> 
  mutate(ln_steps = round(ln_steps, digits = 1)) |> 
  filter(db_year == 2020) |> 
  select(country, ln_steps)


# Government ownership of banks -------------------------------------------

gbbp_20 <- boatw_data |> 
  select(iso, year, country, soe1_db) |> 
  filter(year == 2020) |> 
  mutate(country = str_to_title(country)) |> 
  rename(code    = iso,
         gbbp_20 = soe1_db) |> 
  select(-year)
  
updated_gbbp <- left_join(country_list, gbbp_20)


# Ownership concentration -------------------------------------------------

oecd_ownership <- oecd_data |> 
  select(country, top_3_investors) |> 
  mutate(top_3_investors = top_3_investors/100) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_oecd, country_name_changes)))
  
updated_concentr <- left_join(country_list, oecd_ownership)


# Median block premia -----------------------------------------------------

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


# Military conscription ---------------------------------------------------

conscription_tbl <- m3_data |> 
  select(year, countryname_standard, com_mil_serv) |> 
  mutate(com_mil_serv = na_if(com_mil_serv, "NA")) |> 
  filter(year == 2020) |> 
  filter(!is.na(com_mil_serv)) |> 
  select(-year) |> 
  rename(country = countryname_standard)

updated_havdft <- left_join(country_list, conscription_tbl)


# Corruption (WGI) --------------------------------------------------------

corruption_tbl <- wgi_dataset |> 
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

ef_index <- heritage_data |> 
  select(country, property_rights) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_ef, country_name_changes))) |> 
  mutate(across(everything(), ~ na_if(.x, "N/A")))

updated_pty_rights <- left_join(country_list, ef_index)

# Cultural Dimensions (Hofstede: pdi, idv, uai, & mas) -------------------------

dimensions_tbl <- hofstede_data |> 
  select(country:uai) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_hof, country_name_changes)))

updated_dimensions <- left_join(country_list, dimensions_tbl)


# Union Density (union_dens)----------------------------------------------------

new_union_dens <- union_dens_pulled |> 
  select(ref_area_label, time, obs_value) |> 
  rename(country = ref_area_label) |> 
  mutate(obs_value = obs_value/100) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_ud, country_name_changes))) |> 
  filter(time == "2018")

updated_union_dens <- left_join(country_list, new_union_dens) |> 
  select(-time)
