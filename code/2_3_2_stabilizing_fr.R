# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_2_stabilizing_fr.R")

source(here("code/2_3_stabilizing_work_pop.R"))



# selecting country from Eurostat pop data ----------------------------------------------------------------
country_to_model = "FR"
advanceable_b_fr <- eurostat_ages_clean %>%
  filter(year >= 2000,
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

## histogram - current FR age distribution -----------------------------------------
advanceable_b_fr %>%
  filter(year == 2025) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "darkblue", color = "transparent") +
  labs(
    title = "FR Population Distribution by Age (2025)",
    subtitle = "Eurostat",
    x = "Age",
    y = "Population"
  ) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_age_composition_2025_FR.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



## Function usage - Advancement Simple ----------------------------------------------------
years_to_project <- 17
start_year <- max(advanceable_b_fr$year)
projection_closed_borders_fr <- advanceable_b_fr %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  new <- advance_year_simple(projection_closed_borders_fr, start_year + i - 1)
  projection_closed_borders_fr <- bind_rows(projection_closed_borders_fr, new)
}

wpop_closedborders_fr <- projection_closed_borders_fr %>%
  filter(age_numeric >= 18, 
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

# next: apply natality rates to project into 2100
# then: apply annual mortality rates




# Function usage - Advancement Natality ----------------------------------------------------
advanceable_n_fr <- advanceable_b_fr # making copy of prev df
years_to_project <- 75
start_year <- max(advanceable_n_fr$year)

birth_rate_fr <- 0.012   # example: 12 births per 1000 population
# rate of 1.2%
projection_closed_borders_fr <- advanceable_n_fr %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_natality(
    projection_closed_borders_fr,
    t = next_year,
    birth_rate = birth_rate_fr
  )
  
  projection_closed_borders_fr <- bind_rows(
    projection_closed_borders_fr,
    new
  )
}





## natality working pop histogram -----------------------------------------
wpop_closedborders_fr <- projection_closed_borders_fr %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

wpop_closedborders_fr %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = "FR Forecasted WorkPop, no Migration [Eurostat]",
    subtitle = "Natality Rate 0.9%, WPopAge = [18-64], No Mortality in WPop",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_wpop_closedborders_nat_fr.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



# mortality selecting FR -------------------------------------
country_to_model = "FR"
mortality_fr <- mortality_clean %>%
  filter(year >= 2000,
         year <= 2023, # 2024 is all NA empties
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

mortality_wpop_fr <- mortality_fr %>%
  filter(age_numeric >= 18,
         age_numeric <= 64)



## histogram - FR WPop mortality by age 2023 -----------------------------------------
mortality_wpop_fr %>%
  filter(year == 2023) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "navyblue") +
  labs(
    title = "FR Mortality in WPop, by Age (2023)",
    subtitle = "Eurostat Data [2025], WPop = [18-64]",
    x = "Age",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e3, "K")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_mortality_composition_wpop_2023_FR.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)




## Calculating mortality rates --------------------------------------------------------
# to calculate the rate of deaths,
# we need to combine mortality counts with population counts

# population in 2023
calcs_pop_2023_fr <- advanceable_n_fr %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         pop = Value) # renaming this column for later calc

# deaths in 2023
calcs_deaths_2023_fr <- mortality_fr %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         deaths = Value) # renaming this column for later calc

# mortality rate by age
mortality_rates_2023_fr <- calcs_deaths_2023_fr %>%
  left_join(calcs_pop_2023_fr, by = "age_numeric") %>%
  mutate(mortality_rate = deaths / pop) %>%
  select(age_numeric, mortality_rate)




## usage mortality function ----------------------------------------
years_to_project <- 75
start_year <- max(advanceable_n_fr$year)

projection_mortality_fr <- advanceable_n_fr %>%
  select(year, age_numeric, Value)


for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_mortality(
    projection_mortality_fr,
    t = next_year,
    birth_rate = birth_rate_fr,
    mortality_table = mortality_rates_2023_fr
  )
  
  projection_mortality_fr <- bind_rows(
    projection_mortality_fr,
    new
  )
}


# wpop projection: all (cb, n, m) projection -----------------------------------------

projection_mortality_fr %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = "FR Forecasted WorkPop, no Migration [Eurostat]",
    subtitle = "Natality Rate 0.9%, WPopAge = [18-64], Historical Mortality",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_wpop_closedborders_mort_fr.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)





### current vs. wpop all (cb, n, m) projection -----------------------------------------

# taking 2025 working pop and extending to 2100 
# to graph it as our benchmark
projection_wpop_gap_fr <- advanceable_n_fr %>%
  select(year, age_numeric, Value) %>%
  filter(year == 2025) %>%
  mutate(Projection = "WPop2025") %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") 

projection_wpop_gap_fr <- tibble(
  year = 2025:2100,
  work_pop = projection_wpop_gap_fr$work_pop,
  Projection = "WPop2025") %>%
  select(year, Projection, work_pop)


# then massaging the no migration projection
projection_wpop_cbnm_fr <- projection_mortality_fr %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  rename(NoMigration = "work_pop")

# joining the current stock and no migration projections
projection_wpop_integral_fr <- projection_wpop_cbnm_fr %>%
  left_join(projection_wpop_gap_fr %>%
              select(year, work_pop) %>%
              rename(WPop2025 = "work_pop"),
            by = "year") %>%
  filter(year >= 2025)

# and finally creating a third series for the pre 2025 true numbers
projection_wpop_observed_fr <- projection_wpop_cbnm_fr %>%
  filter(year <= 2025) %>%
  rename(Observed = "NoMigration")


# Graphing Migration Gap (Integral) -----------------------------------
ggplot() +
  # integral
  geom_ribbon(
    data = projection_wpop_integral_fr,
    aes(x = year,
        ymin = pmin(NoMigration, WPop2025),
        ymax = pmax(NoMigration, WPop2025)),
    fill = "grey70",
    alpha = 0.5) +
  
  # stock at 2025 wpop
  geom_line(
    data = projection_wpop_gap_fr %>%
      filter(year>= 2025),
    aes(x = year, y = work_pop, color = "WPop2025"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # nomigration
  geom_line(
    data = projection_wpop_cbnm_fr %>%
      filter(year>= 2025),
    aes(x = year, y = NoMigration, color = "NoMigration"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # true observed
  geom_line(
    data = projection_wpop_observed_fr,
    aes(x = year, y = Observed, color = "Observed"),
    linewidth = 0.6) +
  
  labs(
    title = "FR Working-Age Population: Stabilization Gap",
    subtitle = "Eurostat Current [2025] WPop Stock vs. WPop Decay under No Migration",
    caption = "Working Age = 18–64, Natality 0.9%, 2023 Mortality Schedule",
    x = "Year",
    y = "Population"
  ) +
  
  scale_color_manual(values = c("Observed" = "#377eb8",
                                "NoMigration" = "black",
                                "WPop2025" = "#1b9e77"),
                     breaks = c("Observed", "WPop2025", "NoMigration"))+
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M"))


ggsave(here("outputs/2_3_wpop_integral_cbnm_fr.png"),
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




















