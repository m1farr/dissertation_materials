source("code/02_process_vars.R")

# Median block premia summary statistics ----------------------------------

ft <- flextable(bp_med_tbl) |> 
  autofit()
doc <- read_docx()            
doc <- body_add_flextable(doc, ft)
print(doc, target = "output/bp_table.docx")

# Table 1 Panel A ----------------------------------------------

table_01_data <- updated_anti_sd |> 
  left_join(updated_disclose) |> 
  left_join(updated_cr_2020) |> 
  left_join(updated_case_a) |> 
  left_join(updated_gbbp) |> 
  left_join(updated_mcap) |>
  left_join(updated_firms) |> 
  left_join(updated_concentr) |> 
  left_join(updated_bp_med) |> 
  left_join(updated_privo) |> 
  left_join(updated_spread) |> 
  left_join(updated_gdp) |> 
  mutate(across(anti_sd:ln_gdp_avg_1822, ~ as.numeric(.))) |> 
  left_join(legal_origins) |> 
  filter(legal_origin != "Socialist")

# Define dependent variables
dependent_vars_1a <- c("anti_sd", "disclose", "cr_2020", "case_a_efficiency", "gbbp_20")

# Run regressions for each dependent variable
models_1a <- map(dependent_vars_1a, ~ lm(as.formula(paste(.x, "~ legal_origin + ln_gdp_2020")), data = table_01_data))

# Name the models based on dependent variables
names(models_1a) <- dependent_vars_1a

modelsummary(models_1a,
             title = "Financial Institutions and Legal Origins",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             coef_map = c("legal_originFrench" = "French Legal Origin",
                          "legal_originGerman" = "German Legal Origin",
                          "legal_originScandinavian" = "Scandinavian Legal Origin",
                          "ln_gdp_2020" = "Ln (GDP per capita)",
                          "(Intercept)" = "Constant"),
             output = "output/finance_panel_a.docx")



# Table 1 Panel B ---------------------------------------------------------

# Panel B models
model_1i <- lm(mcap ~ anti_sd + ln_gdp_avg_1822, data = table_01_data)
model_1ii <- lm(ln_firms_pc ~ anti_sd + ln_gdp_avg_1822, data = table_01_data)
model_1iii <- lm(concentr ~ anti_sd + ln_gdp_2020, data = table_01_data)
model_1iv <- lm(bp_med ~ disclose + ln_gdp_2020, data = table_01_data)
model_1v <- lm(privo ~ cr_2020 + case_a_efficiency + ln_gdp_avg_1822, data = table_01_data)
model_1vi <- lm(intr_spread ~ gbbp_20 + ln_gdp_avg_9822, data = table_01_data)

# get model list
models_1b <- list(
  "Stock-market-to-GDP (2018-2022)" = model_1i,
  "Ln(Firms/Pop) (2018-2022)" = model_1ii,
  "Ownership Concentration" = model_1iii,
  "Control premium" = model_1iv,
  "Private-credit-to-GDP (2018-2022)" = model_1v,
  "Interest spread (1998-2022)" = model_1vi
)

modelsummary(models_1b,
             title = "Financial Institutions and Capital Markets Development",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/finance_panel_b.docx")


# Table 2 Panel A ---------------------------------------------------------

table_02_data <- updated_ln_steps |> 
  left_join(updated_index_labor) |> 
  left_join(updated_press_state) |> 
  left_join(updated_havdft) |> 
  left_join(updated_corrupt) |> 
  left_join(updated_unof_emp) |> 
  left_join(updated_lfrmle) |> 
  left_join(updated_avg_unem) |> 
  left_join(updated_rat_mal2024) |> 
  left_join(updated_gdp) |> 
  mutate(across(ln_steps:ln_gdp_avg_1822, ~ as.numeric(.))) |> 
  left_join(legal_origins) |> 
  filter(legal_origin != "Socialist")

dependent_vars_2a <- c("ln_steps", "index_labor", "press_state", "havdft")

models_2a <- map(dependent_vars_2a, 
                 ~ lm(as.formula(paste(.x, "~ legal_origin + ln_gdp_2020")), 
                 data = table_02_data))

names(models_2a) <- dependent_vars_2a

modelsummary(models_2a,
             title = "Government Regulation and Legal Origins",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/gov_panel_a.docx")


# Table 2 Panel B ---------------------------------------------------------

# Panel B models
model_2i <- lm(corrupt ~ ln_steps + ln_gdp_avg_1822, data = table_02_data)
model_2ii <- lm(employmentunofficial ~ ln_steps + ln_gdp_2017, data = table_02_data)
model_2iii <- lm(lfrmle ~ index_labor + ln_gdp_avg_1822, data = table_02_data)
model_2iv <- lm(unem ~ index_labor + ln_gdp_avg_1322, data = table_02_data)
model_2v <- lm(rat_mal2024 ~ index_labor + ln_gdp_avg_1322, data = table_02_data)

models_2b <- list(
  "Corruption Index" = model_2i,
  "Employment Unofficial Economy" = model_2ii,
  "Labor Participation Male" = model_2iii,
  "Unemployment Rate" = model_2iv,
  "Unemployment Rate for Men Aged 20-24" = model_2v
)

modelsummary(models_2b,
             title = "Government Regulation, Corruption, Unofficial Economy, 
             and Labor Market Outcomes",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/gov_panel_b.docx")


# Table 3  Panel A --------------------------------------------------------

table_03_data <- updated_formalism |> 
  left_join(updated_tenure) |> 
  left_join(updated_case_law) |> 
  left_join(updated_time) |> 
  left_join(updated_enforce) |> 
  left_join(updated_pty_rights) |> 
  left_join(updated_gdp) |> 
  mutate(across(formalism_c:ln_gdp_avg_1822, ~ as.numeric(.))) |> 
  left_join(legal_origins) |> 
  filter(legal_origin != "Socialist")

dependent_vars_3a <- c("formalism_c", "tenure", "caselaw")

models_3a <- map(dependent_vars_3a, 
                 ~ lm(as.formula(paste(.x, "~ legal_origin + ln_gdp_2020")), 
                 data = table_03_data))

names(models_3a) <- dependent_vars_3a

modelsummary(models_3a,
             title = "Legal Origin and Judicial Institutions",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/judicial_panel_a.docx")


# Table 3 Panel B ---------------------------------------------------------

# Panel B models
model_3i <- lm(time_c ~ formalism_c + ln_gdp_2020, data = table_03_data)
model_3ii <- lm(enforce ~ formalism_c + ln_gdp_2020, data = table_03_data)
model_3iii <- lm(property_rights ~ tenure + ln_gdp_2022, data = table_03_data)
model_3iv <- lm(caselaw ~ tenure + ln_gdp_2022, data = table_03_data)

models_3b <- list(
  "Time to Enforce Contract" = model_3i,
  "Contract Enforcement" = model_3ii,
  "Property Rights (2022)" = model_3iii,
  "Property Rights (2022)" = model_3iv
)

modelsummary(models_3b,
             title = "Judicial Institutions and Outcomes",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/judicial_panel_b.docx")


# Table 4 -----------------------------------------------------------------

table_04_data <- updated_catholic |> 
  left_join(updated_cr_2010) |> 
  left_join(updated_cr_2020) |> 
  left_join(updated_dimensions) |> 
  left_join(updated_wvs) |> 
  left_join(updated_gdp) |> 
  mutate(across(catholic:ln_gdp_avg_1822, ~ as.numeric(.))) |> 
  left_join(legal_origins) |> 
  filter(legal_origin != "Socialist")

# Define independent variable lists
hofstede_vars <- c("pdi", "idv", "mas", "uai")
wvs_vars <- c("family", "obedience", "independence", "trust")

# Run regressions for each dependent variable
hofstede_models <- map(hofstede_vars, ~lm(as.formula(paste("cr_2010 ~", .x,
                                                           "+ legal_origin +
                                                           ln_gdp_2010")),
                                          data = table_04_data))


wvs_models <- map(wvs_vars, ~lm(as.formula(paste("cr_2020 ~", .x, 
                                                 "+ legal_origin +
                                                 ln_gdp_avg_1722")),
                                data = table_04_data))

# Name the models based on dependent variables
names(hofstede_models) <- hofstede_vars
names(wvs_models) <- wvs_vars

model_4i <- lm(cr_2010 ~ catholic + legal_origin + ln_gdp_2010, data = table_04_data)

models_4 <- c(
  list("Catholic" = model_4i),
  hofstede_models,
  wvs_models
)

modelsummary(models_4,
             title = "Creditor Rights, Culture, and Legal Origin",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/culture_table.docx")




# Table 5 Panel A ----------------------------------------------------------

table_05_data <- updated_anti_sd |> 
  left_join(updated_disclose) |> 
  left_join(updated_cr_2020) |> 
  left_join(updated_case_a) |> 
  left_join(updated_gbbp) |> 
  left_join(updated_ln_steps) |>
  left_join(updated_index_labor) |> 
  left_join(updated_press_state) |> 
  left_join(updated_havdft) |> 
  left_join(updated_formalism) |> 
  left_join(updated_judindependence) |> 
  left_join(updated_tenure) |> 
  left_join(updated_case_law) |> 
  left_join(updated_proportionality) |> 
  left_join(updated_left_power) |> 
  left_join(updated_union_dens) |> 
  mutate(across(anti_sd:union_dens, ~ as.numeric(.))) |> 
  left_join(legal_origins) |> 
  left_join(updated_gdp) |> 
  filter(legal_origin != "Socialist")

dependent_vars_5 <- c("anti_sd", "disclose", "cr_2020", "case_a_efficiency", 
                      "gbbp_20", "ln_steps", "index_labor", "press_state", 
                      "havdft", "formalism_c", "tenure", "caselaw")

models_5a_general <- map(
  dependent_vars_5, 
  ~lm(as.formula(paste(.x, "~ legal_origin + proportionality + ln_gdp_2020")),
      data = table_05_data)
  )

names(models_5a_general) <- dependent_vars_5

model_5ai <- lm(judindependence ~ legal_origin + proportionality + ln_gdp_2019, 
                data = table_05_data)

models_5a <- c(
  models_5a_general,
  list("judindependence" = model_5ai)
)

modelsummary(models_5a,
             title = "Legal Origin and Proportional Representation",
             stars = c('c' = 0.10, 'b' = 0.05, 'a' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/politics_panel_a.docx")


# Table 5 Panel B ---------------------------------------------------------

models_5b <- map(
  dependent_vars_5, 
  ~lm(as.formula(paste(.x, "~ legal_origin + left_power + ln_gdp_2020")),
      data = table_05_data)
)

names(models_5b) <- dependent_vars_5

modelsummary(models_5b,
             title = "Legal Origin and Power of the Left",
             stars = c('ᶜ' = 0.10, 'ᵇ' = 0.05, 'ᵃ' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/politics_panel_b.docx")


# Table 5 Panel C ---------------------------------------------------------

models_5c <- map(
  dependent_vars_5, 
  ~lm(as.formula(paste(.x, "~ legal_origin + union_dens + ln_gdp_2020")),
      data = table_05_data)
)

names(models_5c) <- dependent_vars_5

modelsummary(models_5c,
             title = "Legal Origin and Union Density",
             stars = c('ᶜ' = 0.10, 'ᵇ' = 0.05, 'ᵃ' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/politics_panel_c.docx")


# Table 6 -----------------------------------------------------------------

table_06_data <- table_05_data |> 
  left_join(updated_autocracy)

table_06_model_data <- table_06_data |> 
  filter(autocracy != 0)

models_6 <- map(
  dependent_vars_5, 
  ~lm(as.formula(paste(.x, "~ legal_origin + ln_gdp_2020")),
      data = table_06_model_data)
  )

names(models_6) <- dependent_vars_5

modelsummary(models_6,
             title = "Legal Origin in Countries with Autocratic Governments",
             stars = c('ᶜ' = 0.10, 'ᵇ' = 0.05, 'ᵃ' = 0.01),
             gof_map = c("nobs", "r.squared"),
             output = "output/autocracy_table.docx")

# Save Dataset ------------------------------------------------------------


tables_list <- list(
  "Table 01" = table_01_data,
  "Table 02" = table_02_data,
  "Table 03" = table_03_data,
  "Table 04" = table_04_data,
  "Table 05" = table_05_data,
  "Table 06" = table_06_data
)

# Write to Excel with each element as a sheet
write_xlsx(tables_list, path = "output/combined_tables.xlsx")


# Produce graphs ----------------------------------------------------------

# stock market cap to GDP ratio

poland_ts_data <- wdi |> 
  select(country, year, mcap) |> 
  filter(year >= 2000 & country == "Poland") |> 
  rename(legal_origin = country,
         avg_mcap = mcap)

ts_data <- wdi |> 
  select(country, year, mcap) |> 
  filter(year >= 2000) |> 
  left_join(legal_origins) |> 
  group_by(legal_origin, year) |> 
  summarise(avg_mcap = mean(mcap, na.rm = TRUE)) |> 
  filter(legal_origin %in% c("English", "French", "German", "Scandinavian")) |> 
  ungroup() |> 
  bind_rows(poland_ts_data) |> 
  mutate(year = as.numeric(year))

mcap_ts <- ggplot(ts_data, aes(x = year, y = avg_mcap, color = legal_origin, group = legal_origin)) +
  geom_line() +
  labs(
    title = "Average Market Cap Over Time by Legal Origin",
    x = "Year",
    y = "Average Market Cap",
    color = "Key"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman")
  ) + 
  scale_x_continuous(
    breaks = seq(2000, 2022, by = 5)
  )

ggsave("output/mcap_ts.png", plot = mcap_ts, width = 8, height = 6)


# Poland/civil law comparative table --------------------------------------

summary_tbl <- table_05_data |> 
  mutate(ln_gdp_growth = ln_gdp_2020 / ln_gdp_2000) |> 
  select(country:legal_origin, ln_gdp_growth)

pol_summary <- summary_tbl |> 
  filter(country == "Poland") |> 
  select(-legal_origin) |> 
  rename(legal_origin = country) |> 
  select(-code)

origins_summary <- summary_tbl |> 
  group_by(legal_origin) |> 
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{.col}")
  )

final_tbl <- bind_rows(pol_summary, origins_summary) |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  row_to_names(row_number = 1) |> 
  rename(indicator = legal_origin) |> 
  filter(!is.na(Poland))

write_xlsx(final_tbl, path = "output/comparative_table.xlsx")
