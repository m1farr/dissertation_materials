source("code/01_load_data.R")

# Process variables from World Development Indicators ---------------------

# Variables include market cap, ln firms per capita, private credit ratio,
# interest rate spread, and GDP per capita (PPP-adjusted)

updated_spread <- get_wdi_mean("intr_spread", 1998, 2022, wdi, country_list)
updated_privo <- get_wdi_mean("privo", 2018, 2022, wdi, country_list)
updated_mcap <- get_wdi_mean("mcap", 2018, 2022, wdi, country_list)
updated_lfrmle <- get_wdi_mean("lfrmle", 2018, 2022, wdi, country_list)
updated_avg_unem <- get_wdi_mean("unem", 2013, 2022, wdi, country_list)
updated_firms <- get_wdi_mean("firms_pc", 2018, 2022, wdi, country_list) |> 
  mutate(ln_firms_pc = log(firms_pc)) |> 
  select(-firms_pc)



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

updated_formalism <- get_db("formalism_c",
                            "quality_of_judicial_processes_index_0_18_db17_20_methodology",
                            2020,
                            doing_business_data,
                            country_list)

updated_time <- get_db("time_c",
                       "contract_time",
                       2020,
                       doing_business_data,
                       country_list)

updated_enforce <- get_db("enforce",
                          "score_enforcing_contracts_db17_20_methodology",
                          2020,
                          doing_business_data,
                          country_list)


# Case A Efficiency -------------------------------------------------------

case_a_efficiency <- doing_business_data |> 
  select(country, db_year, score_enforcing_contracts_db17_20_methodology, 
         score_resolving_insolvency) |> 
  rename(enforce_contracts = score_enforcing_contracts_db17_20_methodology, 
         resolve_insolvency = score_resolving_insolvency) |> 
  filter(db_year == 2020) |> 
  mutate(case_a_efficiency = (enforce_contracts + resolve_insolvency)/2) |> 
  select(country, case_a_efficiency)

updated_case_a <- left_join(country_list, case_a_efficiency)


# Ln Steps (Starting Business) --------------------------------------------

ln_steps <- doing_business_data |> 
  select(country, db_year, procedures_men_number, procedures_women_number) |> 
  mutate(
    procedures_men_number   = as.numeric(procedures_men_number),
    procedures_women_number = as.numeric(procedures_women_number)
  ) |> 
  mutate(ln_steps = (procedures_men_number + procedures_women_number)/2) |> 
  mutate(ln_steps = round(ln_steps, digits = 1)) |> 
  filter(db_year == 2020) |> 
  select(country, ln_steps)

updated_ln_steps <- left_join(country_list, ln_steps)


# Index of Labor ----------------------------------------------------------

index_labor <- oecd_epl_data |> 
  select(country, version, time_period, obs_value, measure_2) |> 
  filter(version == "VERSION4" & time_period == 2019) |> 
  pivot_wider(
    names_from = measure_2,
    values_from = obs_value
  ) |> 
  clean_names() |> 
  mutate(across(collective_dismissals:temporary_contracts, ~ as.numeric(.x))) |> 
  mutate(index_labor = rowMeans(across(collective_dismissals:temporary_contracts), na.rm = TRUE)) |> 
  select(country, index_labor)

updated_index_labor <- left_join(country_list, index_labor)


# Government ownership of press -------------------------------------------

press_share_state <- vindoc_data |> 
  select(country, country_text_id, year, v2medstateprint_ord) |> 
  filter(year == 2020) |> 
  rename(press_state = v2medstateprint_ord) |> 
  select(country, press_state)

updated_press_state <- left_join(country_list, press_share_state)


# Government ownership of banks -------------------------------------------

gbbp_20 <- boatw_data |> 
  select(iso, year, country, soe1_db) |> 
  filter(year == 2020) |> 
  rename(code    = iso,
         gbbp_20 = soe1_db) |> 
  select(-year)

updated_gbbp <- left_join(country_list, gbbp_20)


# Ownership concentration -------------------------------------------------

oecd_ownership <- oecd_oc_data |> 
  select(country, top_3_investors) |> 
  mutate(top_3_investors = top_3_investors/100) |> 
  rename(concentr = top_3_investors)

updated_concentr <- left_join(country_list, oecd_ownership)


# Median block premia -----------------------------------------------------

bp_med <- complete_orbis |> 
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
  select(year, country, com_mil_serv) |> 
  mutate(com_mil_serv = na_if(com_mil_serv, "NA")) |> 
  filter(year == 2020) |> 
  filter(!is.na(com_mil_serv)) |> 
  select(-year) |> 
  rename(havdft = com_mil_serv)

updated_havdft <- left_join(country_list, conscription_tbl)


# Corruption (WGI) --------------------------------------------------------

corruption_tbl <- wgi_data |> 
  select(country, year, indicator, estimate) |> 
  filter(indicator == "cc") |> 
  filter(year >= 2018 & year <= 2022) |> 
  select(-indicator) |> 
  rename(corruption = estimate) |> 
  group_by(country) |> 
  mutate(corruption = as.numeric(corruption)) |> 
  mutate(corrupt = mean(corruption)) |> 
  select(country, corrupt) |> 
  distinct()

updated_corrupt <- left_join(country_list, corruption_tbl)


# Unofficial Employment ---------------------------------------------------

unof_emp <- unofficial_economy |> 
  pivot_wider(
    names_from = sheet,
    values_from = x2017
  ) |> 
  clean_names() |> 
  mutate(
    employmentunofficial = rowMeans(across(dge_p:wvs), na.rm = TRUE),
    count = rowSums(!is.na(across(dge_p:wvs)))
  ) |> 
  select(country, employmentunofficial, count) |> 
  filter(!is.na(employmentunofficial)) |> 
  filter(count > 2)

updated_unof_emp <- left_join(country_list, unof_emp) |> 
  select(-count)



# Rate of male unemployment, 20-24 ----------------------------------------

rat_mal2024 <- ilostat_male_data |> 
  select(country, time, obs_value) |> 
  filter(time >= 2013 & time <= 2022) |> 
  group_by(country) |> 
  mutate(rat_mal2024 = mean(obs_value)) |> 
  select(country, rat_mal2024) |> 
  distinct()

updated_rat_mal2024 <- left_join(country_list, rat_mal2024)

# Tenure and Case law -------------------------------------------------------

updated_case_law <- left_join(country_list, higher_court_data) |> 
  select(-tenure)

updated_tenure <- left_join(country_list, higher_court_data) |> 
  select(-caselaw)


# Property rights ---------------------------------------------------------

ef_index <- heritage_data |> 
  select(country, property_rights) |> 
  mutate(across(everything(), ~ na_if(.x, "N/A")))

updated_pty_rights <- left_join(country_list, ef_index)

# Judicial independence ---------------------------------------------------

wjp_clean <- wjp_clean |> 
  select(country, x1_2_government_powers_are_effectively_limited_by_the_judiciary) |> 
  rename(judindependence = x1_2_government_powers_are_effectively_limited_by_the_judiciary)

updated_judindependence <- left_join(country_list, wjp_clean)

# Cultural Dimensions (Hofstede: pdi, idv, uai, & mas) -------------------------

dimensions_tbl <- hofstede_data |> 
  select(country:uai)

updated_dimensions <- left_join(country_list, dimensions_tbl)


# Union Density (union_dens)----------------------------------------------------

new_union_dens <- ilostat_union_data |> 
  select(country, time, obs_value) |> 
  mutate(obs_value = obs_value/100) |> 
  filter(time == "2018") |> 
  rename(union_dens = obs_value)

updated_union_dens <- left_join(country_list, new_union_dens) |> 
  select(-time)

# Autocracy  -------------------------------------------------------------------

autocracy_tbl <- vdem_data |> 
  select(country, country_text_id, year, v2x_regime, v2lgbicam) |> 
  filter(year >= 1990 & year <= 2020) |> 
  mutate(autocracy = case_when(
    v2x_regime %in% c(0, 1) & v2lgbicam == 0 ~ 2,
    v2x_regime %in% c(0, 1) & v2lgbicam > 0  ~ 1,
    v2x_regime >= 2                          ~ 0
  )) |> 
  group_by(country) |> 
  mutate(autocracy_mean = mean(autocracy, na.rm = TRUE)) |> 
  select(country, autocracy_mean) |> 
  rename(autocracy = autocracy_mean) |> 
  distinct()

updated_autocracy <- left_join(country_list, autocracy_tbl)

# Left Power --------------------------------------------------------------

left_power <- dpi_data |> 
  select(country, year, execrlc, congress_rlc) |> 
  mutate(year = year(year)) |> 
  filter(year > 1995) |> 
  mutate(left_power = case_when(
    execrlc >= 2 & congress_rlc >= 2 ~ 1,
    execrlc < 2 | congress_rlc < 2 ~ 0,
    is.na(execrlc) == TRUE & congress_rlc >= 2 ~ NA_real_,
    execrlc >= 2 & is.na(congress_rlc) == TRUE ~ NA_real_
  )) |> 
  group_by(country) |> 
  summarise(
    years_with_data = sum(!is.na(left_power)),
    years_left_power = sum(left_power == 1, na.rm = TRUE),
    pct_left_power = years_left_power / years_with_data
  ) |>
  ungroup() |> 
  filter(years_with_data == 25) |> 
  select(country, pct_left_power) |> 
  rename(left_power = pct_left_power)

updated_left_power <- left_join(country_list, left_power)


# Proportionality ---------------------------------------------------------

proportionality <- dpi_data |> 
  select(country, year, pr, pluralty, housesys) |> 
  mutate(
    pr = case_when(pr == -999 ~ NA_real_,
                   TRUE ~ pr),
    pluralty = case_when(pluralty == -999 ~ NA_real_,
                         TRUE ~ pr),
    housesys = case_when(
      housesys == "PR" ~ 0,
      housesys == "Plurality" ~ 1
    ),
    year = year(year)
  ) |> 
  mutate(score = 2 + pr - pluralty - housesys) |> 
  filter(year > 1995) |> 
  group_by(country) |> 
  summarise(
    years_with_data = sum(!is.na(score)),
    avg_score = mean(score, na.rm = TRUE)
  ) |> 
  filter(!is.na(avg_score)) |> 
  filter(years_with_data > 10) |> 
  select(country, avg_score) |> 
  rename(proportionality = avg_score)

updated_proportionality <- left_join(country_list, proportionality)

# Catholic ----------------------------------------------------------------

# Follows the methodology of Djankov et al 2007, which only considers 9 religious
# groups: Atheist (here, any non-religious), Buddhist, Catholic, Hindu,
# Indigenous, Jewish, Muslim, Orthodox Christian, and Protestant

rcs_pop <- rcs_data |> 
  filter(year == 2010) |> 
  select(nrepc, budpc, catpc, hinpc, indpc, jewpc, muspc, ortpc, prtpc)

catholic <- rcs_data |> 
  filter(year == 2010) |> 
  select(iso3, year, nrepc, budpc, catpc, hinpc, indpc, jewpc, muspc, ortpc, prtpc) |> 
  mutate(max_column = apply(rcs_pop, 1, function(row) colnames(rcs_pop)[which.max(row)])) |> 
  mutate(catholic = case_when(
    max_column == "catpc" ~ 1,
    .default = 0
  )) |> 
  select(iso3, catholic) |> 
  rename(code = iso3) |> 
  mutate(code = str_replace_all(code, "ROU", "ROM"),
         code = str_replace_all(code, "COD", "ZAR"))

updated_catholic <- left_join(country_list, catholic)

# WVS ---------------------------------------------------------------------

# variables: obedience, independence, family, trust
# Var Q1: get % of respondents who agree that family life is v important
# Var Q8: get % of respondents who agree that child independence is important
# Var Q17: get % of respondents who agree that child obedience is important
# Var Q57: get % of respondents who agree that strangers (people) can
# generally be trusted

wvs_data <- wvs_wave_7_data |> 
  clean_names() |> 
  select(b_country_alpha, q1, q8, q17, q57) |> 
  rename(
    code = b_country_alpha,
    family = q1,
    independence = q8,
    obedience = q17,
    trust = q57
  ) |> 
  group_by(code) |> 
  summarise(
    family = (sum(family == 1) / n()),
    independence = (sum(independence == 1) / n()),
    obedience = (sum(obedience == 1) / n()),
    trust = (sum(trust == 1) / n())
  ) |> 
  mutate(code = str_replace_all(code, "ROU", "ROM"))

updated_wvs <- left_join(country_list, wvs_data)


# GDP ---------------------------------------------------------------------

gdp_tbl <- wdi |> 
  select(country, year, gdp_pc) |> 
  pivot_wider(
    names_from = year,
    values_from = gdp_pc
  ) |> 
  select(country, `1990`:`2023`) |> 
  rename_with(
    .fn = ~ paste0("gdp_", .),
    .cols = matches("^\\d{4}$")
  ) |> 
  mutate(gdp_avg_9020 = rowMeans(across(gdp_1990:gdp_2020)),
         gdp_avg_9822 = rowMeans(across(gdp_1998:gdp_2022)),
         gdp_avg_1322 = rowMeans(across(gdp_2013:gdp_2022)),
         gdp_avg_1722 = rowMeans(across(gdp_2017:gdp_2022)),
         gdp_avg_1822 = rowMeans(across(gdp_2018:gdp_2022))
  ) |> 
  mutate(across(starts_with("gdp"), log)) |> 
  rename_with(~ paste0("ln_", .), starts_with("gdp"))

updated_gdp <- left_join(country_list, gdp_tbl)
