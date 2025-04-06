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
  left_join(temp) |> 
  left_join(legal_origins)

# Define dependent variables
dependent_vars <- c("anti_sd", "disclose", "cr_2020", "case_a_efficiency", "soe1_db")

# Run regressions for each dependent variable
models <- map(dependent_vars, ~ lm(as.formula(paste(.x, "~ legal_origin + ln_gdp_2020")), data = table_01_data))

# Name the models based on dependent variables
names(models) <- dependent_vars

modelsummary(models,
             title = "Financial Institutions and Capital Markets Development",
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             gof_map = c("nobs", "r.squared"),
             coef_map = c("legal_originFrench" = "French Legal Origin",
                          "legal_originGerman" = "German Legal Origin",
                          "legal_originScandinavian" = "Scandinavian Legal Origin",
                          "ln_gdp_2020" = "Ln (GDP per capita)",
                          "(Intercept)" = "Constant"),
             output = "finance_panel_a.docx")

# Run separate regressions for each legal origin
mod_anti_sd <- lm(ln_gdp_2020 ~ anti_sd, table_01_data)


# Extract models
models_list <- setNames(models$model, models$legal_origin)

# Display models in one table
modelsummary(models_list, title = "Regression Results by Legal Origin")