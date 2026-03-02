# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_3_stabilizing_it.R")

source(here("code/2_3_stabilizing_work_pop.R"))

country_to_model <- "IT"
country_to_model_str <- "IT"

birth_rate_it <- 0.007   
# rate of 0.7%


# selecting country from Eurostat pop data ----------------------------------------------------------------
advanceable_b_it <- eurostat_ages_clean %>%
  filter(year >= 2000,
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

## histogram - current FR age distribution -----------------------------------------
advanceable_b_it %>%
  filter(year == 2025) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "darkblue", color = "transparent") +
  labs(
    title = paste0(country_to_model_str, " Population Distribution by Age (2025)"),
    subtitle = "Eurostat",
    x = "Age",
    y = "Population"
  ) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(text = element_text(family="Times New Roman"))

ggsave(here("outputs/2_3_it_age_composition_2025.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



## Function usage - Advancement Simple ----------------------------------------------------
years_to_project <- 17
start_year <- max(advanceable_b_it$year)
projection_closed_borders_it <- advanceable_b_it %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  new <- advance_year_simple(projection_closed_borders_it, start_year + i - 1)
  projection_closed_borders_it <- bind_rows(projection_closed_borders_it, new)
}

wpop_closedborders_it <- projection_closed_borders_it %>%
  filter(age_numeric >= 18, 
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

# next: apply natality rates to project into 2100
# then: apply annual mortality rates




# Function usage - Advancement Natality ----------------------------------------------------
advanceable_n_it <- advanceable_b_it # making copy of prev df
years_to_project <- 75
start_year <- max(advanceable_n_it$year)

projection_closed_borders_it <- advanceable_n_it %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_natality(
    projection_closed_borders_it,
    t = next_year,
    birth_rate = birth_rate_it
  )
  
  projection_closed_borders_it <- bind_rows(
    projection_closed_borders_it,
    new
  )
}





## natality working pop histogram -----------------------------------------
wpop_closedborders_it <- projection_closed_borders_it %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

wpop_closedborders_it %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = paste0(country_to_model_str, " Forecasted WorkPop, no Migration [Eurostat]"),
    subtitle = "Natality Rate 0.7%, WPopAge = [18-64], No Mortality in WPop",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(text = element_text(family="Times New Roman"))


ggsave(here("outputs/2_3_it_wpop_closedborders_nat.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



# mortality selecting FR -------------------------------------
mortality_it <- mortality_clean %>%
  filter(year >= 2000,
         year <= 2023, # 2024 is all NA empties
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

mortality_wpop_it <- mortality_it %>%
  filter(age_numeric >= 18,
         age_numeric <= 64)



## histogram - FR WPop mortality by age 2023 -----------------------------------------
mortality_wpop_it %>%
  filter(year == 2023) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "navyblue") +
  labs(
    title = paste0(country_to_model_str, " Mortality in WPop, by Age (2023)"),
    subtitle = "Eurostat Data [2025], WPop = [18-64]",
    x = "Age",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e3, "K")) + # 1e6 means removing 6 zeros from scale
  theme(text = element_text(family="Times New Roman"))

ggsave(here("outputs/2_3_it_mortality_composition_wpop_2023.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)




## Calculating mortality rates --------------------------------------------------------
# to calculate the rate of deaths,
# we need to combine mortality counts with population counts

# population in 2023
calcs_pop_2023_it <- advanceable_n_it %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         pop = Value) # renaming this column for later calc

# deaths in 2023
calcs_deaths_2023_it <- mortality_it %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         deaths = Value) # renaming this column for later calc

# mortality rate by age
mortality_rates_2023_it <- calcs_deaths_2023_it %>%
  left_join(calcs_pop_2023_it, by = "age_numeric") %>%
  mutate(mortality_rate = deaths / pop) %>%
  select(age_numeric, mortality_rate)




## usage mortality function ----------------------------------------
years_to_project <- 75
start_year <- max(advanceable_n_it$year)

projection_mortality_it <- advanceable_n_it %>%
  select(year, age_numeric, Value)


for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_mortality(
    projection_mortality_it,
    t = next_year,
    birth_rate = birth_rate_it,
    mortality_table = mortality_rates_2023_it
  )
  
  projection_mortality_it <- bind_rows(
    projection_mortality_it,
    new
  )
}


# wpop projection: all (cb, n, m) projection -----------------------------------------

projection_mortality_it %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = paste0(country_to_model_str, " Forecasted WorkPop, no Migration [Eurostat]"),
    subtitle = "Natality Rate 0.7%, WPopAge = [18-64], Historical Mortality",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(text = element_text(family="Times New Roman"))

ggsave(here("outputs/2_3_it_wpop_closedborders_mort.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)





### current vs. wpop all (cb, n, m) projection -----------------------------------------

# taking 2025 working pop and extending to 2100 
# to graph it as our benchmark
projection_wpop_gap_it <- advanceable_n_it %>%
  select(year, age_numeric, Value) %>%
  filter(year == 2025) %>%
  mutate(Projection = "WPop2025") %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") 

projection_wpop_gap_it <- tibble(
  year = 2025:2100,
  work_pop = projection_wpop_gap_it$work_pop,
  Projection = "WPop2025") %>%
  select(year, Projection, work_pop)


# then massaging the no migration projection
projection_wpop_cbnm_it <- projection_mortality_it %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  rename(NoMigration = "work_pop")

# joining the current stock and no migration projections
projection_wpop_integral_it <- projection_wpop_cbnm_it %>%
  left_join(projection_wpop_gap_it %>%
              select(year, work_pop) %>%
              rename(WPop2025 = "work_pop"),
            by = "year") %>%
  filter(year >= 2025)

# and finally creating a third series for the pre 2025 true numbers
projection_wpop_observed_it <- projection_wpop_cbnm_it %>%
  filter(year <= 2025) %>%
  rename(Observed = "NoMigration")


# Graphing Migration Gap (Integral) -----------------------------------
ggplot() +
  # integral
  geom_ribbon(
    data = projection_wpop_integral_it,
    aes(x = year,
        ymin = pmin(NoMigration, WPop2025),
        ymax = pmax(NoMigration, WPop2025)),
    fill = "grey70",
    alpha = 0.5) +
  
  # stock at 2025 wpop
  geom_line(
    data = projection_wpop_gap_it %>%
      filter(year>= 2025),
    aes(x = year, y = work_pop, color = "WPop2025"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # nomigration
  geom_line(
    data = projection_wpop_cbnm_it %>%
      filter(year>= 2025),
    aes(x = year, y = NoMigration, color = "NoMigration"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # true observed
  geom_line(
    data = projection_wpop_observed_it,
    aes(x = year, y = Observed, color = "Observed"),
    linewidth = 0.6) +
  
  labs(
    title = paste0(country_to_model_str, " Working-Age Population: Stabilization Gap"),
    subtitle = "Eurostat Current [2025] WPop Stock vs. WPop Decay under No Migration",
    caption = "Working Age = 18–64, Natality 0.7%, 2023 Mortality Schedule",
    x = "Year",
    y = "Population"
  ) +
  
  scale_color_manual(values = c("Observed" = "#377eb8",
                                "NoMigration" = "black",
                                "WPop2025" = "#1b9e77"),
                     breaks = c("Observed", "WPop2025", "NoMigration"))+
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(text = element_text(family="Times New Roman"))


ggsave(here("outputs/2_3_it_wpop_integral_cbnm.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)








# THINGS TO NOTE---------------------------------------------------
# 1) when calculating birth rates for each t+1, you are using a rate and applying
# eg 0.9%
# NEED TO UPDATE WITH FR RATE
# 1.a) this rate is eye-balled off of the DE natality chart, 
# and not based on a further calculation or regression
# 1.b) this rate is being applied to the entire population stock, 
# in which you are currently killing letting people age to 100 and then
# killing them. for a more scientific approach, you would need to 
# adjust this 'tapering-off' of elderly population using observed deaths (as u did with natality)

