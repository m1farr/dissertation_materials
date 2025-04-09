source("code/00_dependencies.R")
source("code/99_functions.R")

# General data loading ----------------------------------------------------

# Create list of specific sheets to read
sheet_map <- list("data", "All Data", "Data", "Results")

# Name list to match specific sheet to specific workbook
names(sheet_map) <- c("boatw_data", "doing_business_data", "wdi_data", "orbis_data") 

# Read and load all files
read_datasets_as_tables("data", sheet_map)


# Legal origins data loading. ----------------------------------------------

# Pull legal origins data from original dataset (La Porta et al. 2008)
legal_origins <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 1") |> 
  filter(!is.na(legor_uk)) |> 
  select(country, code, legor_uk:legor_so) |> 
  distinct() |> 
  mutate(
    legal_origin = case_when(
      legor_uk == 1 ~ "English",
      legor_fr == 1 ~ "French",
      legor_ge == 1 ~ "German",
      legor_sc == 1 ~ "Scandinavian",
      legor_so == 1 ~ "Socialist"
    )) |>
  select(country, code, legal_origin)

# Account for 2006 separation of Serbia and Montenegro, both of which have
# French (civil law) legal origins
legal_origins <- legal_origins |> 
  bind_rows(
    legal_origins |> 
      filter(country == "Serbia and Montenegro") |> 
      mutate(
        country = "Serbia",
        code    = "SRB"),
    legal_origins |> 
      filter(country == "Serbia and Montenegro") |> 
      mutate(
        country = "Montenegro",
        code    = "MNE")
  ) |> 
  filter(country != "Serbia and Montenegro") |> 
  arrange(code) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_ecolo, country_name_changes)))

country_list <- legal_origins |> 
  select(country, code)


# WDI ---------------------------------------------------------------------

wdi <- wdi_data |> 
  rename(country = country_name) |> 
  mutate(across(everything(), ~ na_if(.x, ".."))) |> 
  rename_with(~ gsub("^x(\\d{4})_yr\\1$", "\\1", .x)) |> 
  mutate(across(`1974`:`2023`, ~ as.numeric(.x))) |> 
  select(-series_code) |> 
  mutate(
    series_name = str_replace_all(series_name, fixed("Market capitalization of listed domestic companies (% of GDP)"), "mcap"),
    series_name = str_replace_all(series_name, fixed("Firms per capita"), "firms_pc"),
    series_name = str_replace_all(series_name, fixed("Domestic credit to private sector (% of GDP)"), "privo"),
    series_name = str_replace_all(series_name, fixed("Interest rate spread (lending rate minus deposit rate, %)"), "intr_spread"),
    series_name = str_replace_all(series_name, fixed("GDP per capita, PPP (constant 2021 international $)"), "gdp_pc"),
    series_name = str_replace_all(series_name, fixed("Labor force participation rate, male (% of male population ages 15-64) (modeled ILO estimate)"), "lfrmle"),
    series_name = str_replace_all(series_name, fixed("Unemployment, total (% of total labor force) (modeled ILO estimate)"), "unem")
  ) |> 
  pivot_longer(
    cols         = `1974`:`2023`,
    names_to     = "year",
    names_prefix = "x"
  ) |> 
  pivot_wider(
    names_from  = series_name,
    values_from = value
  )


# Orbis M&A ---------------------------------------------------------------

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
  rename(country = target_country) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_orbis, country_name_changes)))


# DPI ---------------------------------------------------------------------

dpi_data <- dpi_data |> 
  clean_names() |> 
  rename(country = countryname) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_dpi, country_name_changes))) |> 
  select(country, year, pr, pluralty, housesys, execrlc, gov1seat, gov1rlc,
         opp1seat, opp1rlc) |> 
  mutate(congress_rlc = case_when(
    gov1seat > opp1seat ~ gov1rlc,
    gov1seat < opp1seat ~ opp1rlc
  )) |> 
  mutate(
    execrlc = case_when(
      execrlc == "Right" ~ 1,
      execrlc == "Center" ~ 2,
      execrlc == "Left" ~ 3,
      execrlc == -999 ~ NA_real_
    )) |> 
  mutate(
    congress_rlc = case_when(
      congress_rlc == "Right" ~ 1,
      congress_rlc == "Center" ~ 2,
      congress_rlc == "Left" ~ 3,
      congress_rlc == -999 ~ NA_real_
    )
  )


# Country name changes ----------------------------------------------------

boatw_data <- boatw_data |> 
  mutate(country = str_to_title(country)) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_boatw, country_name_changes)))

# error
doing_business_data <- doing_business_data |> 
  rename(country = economy) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_db, country_name_changes)))

heritage_data <- heritage_data |> 
  mutate(country = map_chr(country, ~change_names(.x, name_ef, country_name_changes)))

hofstede_data <- hofstede_data |> 
  mutate(country = map_chr(country, ~change_names(.x, name_hof, country_name_changes)))

m3_data <- m3_data |> 
  rename(country = countryname_standard) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_m3, country_name_changes)))

oecd_data <- oecd_data |> 
  mutate(country = map_chr(country, ~change_names(.x, name_oecd, country_name_changes)))

union_dens_pulled <- union_dens_pulled |> 
  rename(country = ref_area_label) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_ud, country_name_changes)))

wgi_data <- wgi_data |> 
  rename(country = countryname) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_wgi, country_name_changes)))

vdem_data <- vdem_data |> 
  rename(country = country_name) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_vdem, country_name_changes)))
  
