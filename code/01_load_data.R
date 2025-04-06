source("code/00_dependencies.R")
source("code/99_functions.R")

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
  arrange(code)

country_list <- legal_origins |> 
  select(country, code)


# General data loading ----------------------------------------------------

read_datasets_as_tables()

# Doing business ---------------------------------------------------------
doing_business <- read_excel("data/doing_business_corrected.xlsx", sheet = "All Data", skip = 3) |> 
  clean_names() |> 
  rename(
    construction_procedures       = procedures_number_29,
    construction_score_procedures = score_procedures_number_30,
    construction_time             = time_days_31,
    construction_score_time       = score_time_days_32,
    electricity_procedures        = procedures_number_46,
    electricity_score_procedures  = score_procedures_number_47,
    electricity_time              = time_days_48,
    electricity_score_time        = score_time_days_49,
    property_procedures           = procedures_number_67,
    property_score_procedures     = score_procedures_number_68,
    property_time                 = time_days_69,
    property_score_time           = score_time_days_70,
    contract_time                 = time_days_171,
    contract_score_time           = score_time_days_172
  ) |> 
  filter(!is.na(economy)) |> 
  select(economy, db_year, score_protecting_minority_investors_db15_20_methodology,
         extent_of_disclosure_index_0_10, strength_of_insolvency_framework_index_0_16,
         score_enforcing_contracts_db17_20_methodology, score_resolving_insolvency,
         procedures_men_number, procedures_women_number) |> 
  rename(country = economy) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_db, country_name_changes)))