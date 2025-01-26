source("code/00_dependencies.R")
source("code/99_functions.R")

# load country name change lookup table
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# load country name list for Table 5 (Politics)
table_05_countries <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 5") |> 
  select(country) |> 
  distinct()

# Union Density (union_dens)----------------------------------------------------

new_union_dens <- read_excel("data/union_dens_pulled.xlsx") |> 
  clean_names() |> 
  select(ref_area_label, time, obs_value) |> 
  rename(country = ref_area_label) |> 
  mutate(obs_value = obs_value/100) |> 
  mutate(country = map_chr(country, ~change_names(.x, name_new_ud, country_name_changes))) |> 
  filter(time == "2018")

updated_union_dens <- left_join(table_05_countries, new_union_dens) |> 
  select(-time)

write_xlsx(updated_union_dens, path = "output/updated_union_dens.xlsx")

# I used this plot to visualize the amount of countries with union density data grouped
# by year in order to select the most appropriate year for my variable. 

# union_dens_bar_plot <- update_union_dens |> 
#   group_by(time) |> 
#   select(-obs_value) |> 
#   ggplot(aes(time)) +
#   geom_bar()


