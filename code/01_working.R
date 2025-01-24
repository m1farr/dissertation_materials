source("code/00_dependencies.R")
source("code/99_functions.R")


# load all data
read_workbook("data/legal_origins_data.xlsx")
country_name_changes <- read_excel("data/country_name_changes.xlsx")

# temp <- read.csv("data/rcs_demographics_data.csv")


# Union Density -----------------------------------------------------------

union_dens_data <- read_excel("data/legal_origins_data.xlsx", sheet = "Table 5") |> 
  select(country, union_dens) |> 
  mutate(country = map_chr(country, ~change_names(.x, country_name_changes))
         )

new_union_dens <- read_excel("data/union_dens_pulled.xlsx") |> 
  clean_names() |> 
  select(ref_area_label, time, obs_value) |> 
  rename(country = ref_area_label) |> 
  mutate(obs_value = obs_value/100)

update_union_dens <- inner_join(union_dens_data, new_union_dens, relationship = "many-to-many")|> 
  filter(time == "2018") |> 
  distinct() |> 
  rename(
    union_dens_1997 = union_dens,
    union_dens_2018 = obs_value
  ) |> 
  select(-time)

union_dens_format <- left_join(union_dens_data, update_union_dens, by = "country") |> 
  select(-union_dens) |> 
  distinct()

write_xlsx(union_dens_format, path = "output/union_dens_format.xlsx")

# I used this plot to visualize the amount of countries with union density data grouped
# by year in order to select the most appropriate year for my variable. 

# union_dens_bar_plot <- update_union_dens |> 
#   group_by(time) |> 
#   select(-obs_value) |> 
#   ggplot(aes(time)) +
#   geom_bar()
