# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_1_stabilizing_de.R")

source(here("code/2_3_stabilizing_work_pop.R"))



# selecting country from Eurostat pop data ----------------------------------------------------------------
country_to_model = "DE_TOT"
advanceable_b_de <- eurostat_ages_clean %>%
  filter(year >= 2000,
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

## histogram - current DE age distribution -----------------------------------------
advanceable_b_de %>%
  filter(year == 2025) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "darkblue", color = "transparent") +
  labs(
    title = "DE Population Distribution by Age (2025)",
    subtitle = "Eurostat",
    x = "Age",
    y = "Population"
  ) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_age_composition_2025_DE.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



## Function usage - Advancement Simple ----------------------------------------------------
years_to_project <- 17
start_year <- max(advanceable_b_de$year)
projection_closed_borders_de <- advanceable_b_de %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  new <- advance_year_simple(projection_closed_borders_de, start_year + i - 1)
  projection_closed_borders_de <- bind_rows(projection_closed_borders_de, new)
}

wpop_closedborders_de <- projection_closed_borders_de %>%
  filter(age_numeric >= 18, 
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

# next: apply natality rates to project into 2100
# then: apply annual mortality rates




# Function usage - Advancement Natality ----------------------------------------------------
advanceable_n_de <- advanceable_b_de # making copy of prev df
years_to_project <- 75
start_year <- max(advanceable_n_de$year)

birth_rate_de <- 0.009   # example: 9 births per 1000 population
# rate of 0.9%
projection_closed_borders_de <- advanceable_n_de %>%
  select(year, age_numeric, Value)

for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_natality(
    projection_closed_borders_de,
    t = next_year,
    birth_rate = birth_rate_de
  )
  
  projection_closed_borders_de <- bind_rows(
    projection_closed_borders_de,
    new
  )
}





## natality working pop histogram -----------------------------------------
wpop_closedborders_de <- projection_closed_borders_de %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value), .groups="drop")

wpop_closedborders_de %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = "DE Forecasted WorkPop, no Migration [Eurostat]",
    subtitle = "Natality Rate 0.9%, WPopAge = [18-64], No Mortality in WPop",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_wpop_closedborders_nat_de.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



# mortality selecting DE -------------------------------------
country_to_model = "DE_TOT"
mortality_de <- mortality_clean %>%
  filter(year >= 2000,
         year <= 2023, # 2024 is all NA empties
         gender == "T",
         Location == country_to_model,
         !(age_bracket %in% c("Y_OPEN",
                              "UNK",
                              "TOTAL",
                              "Y_LT1"))) %>% 
  mutate(age_numeric = as.integer(str_extract(age_bracket, "\\d+")))

mortality_wpop_de <- mortality_de %>%
  filter(age_numeric >= 18,
         age_numeric <= 64)



## histogram - DE WPop mortality by age 2023 -----------------------------------------
mortality_wpop_de %>%
  filter(year == 2023) %>%
  ggplot(aes(x = age_numeric, y = Value)) +
  geom_col(fill = "navyblue") +
  labs(
    title = "DE Mortality in WPop, by Age (2023)",
    subtitle = "Eurostat Data [2025], WPop = [18-64]",
    x = "Age",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e3, "K")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_mortality_composition_wpop_2023_DE.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)




## Calculating mortality rates --------------------------------------------------------
# to calculate the rate of deaths,
# we need to combine mortality counts with population counts

# population in 2023
calcs_pop_2023_de <- advanceable_n_de %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         pop = Value) # renaming this column for later calc

# deaths in 2023
calcs_deaths_2023_de <- mortality_de %>%
  filter(year == 2023) %>%
  select(age_numeric, 
         deaths = Value) # renaming this column for later calc

# mortality rate by age
mortality_rates_2023_de <- calcs_deaths_2023_de %>%
  left_join(calcs_pop_2023_de, by = "age_numeric") %>%
  mutate(mortality_rate = deaths / pop) %>%
  select(age_numeric, mortality_rate)




## usage mortality function ----------------------------------------
years_to_project <- 75
start_year <- max(advanceable_n_de$year)

birth_rate_de <- 0.009   # 0.9%

projection_mortality_de <- advanceable_n_de %>%
  select(year, age_numeric, Value)


for(i in 1:years_to_project){
  
  next_year <- start_year + i - 1
  
  new <- advance_year_mortality(
    projection_mortality_de,
    t = next_year,
    birth_rate = birth_rate_de,
    mortality_table = mortality_rates_2023_de
  )
  
  projection_mortality_de <- bind_rows(
    projection_mortality_de,
    new
  )
}


# wpop projection: all (cb, n, m) projection -----------------------------------------

projection_mortality_de %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  ggplot(aes(x = year, y = work_pop)) +
  geom_line() +
  labs(
    title = "DE Forecasted WorkPop, no Migration [Eurostat]",
    subtitle = "Natality Rate 0.9%, WPopAge = [18-64], Historical Mortality",
    x = "Year",
    y = "Population"
  ) +
  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_wpop_closedborders_mort_de.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)





### current vs. wpop all (cb, n, m) projection -----------------------------------------

# taking 2025 working pop and extending to 2100 
# to graph it as our benchmark
projection_wpop_gap_de <- advanceable_n_de %>%
  select(year, age_numeric, Value) %>%
  filter(year == 2025) %>%
  mutate(Projection = "WPop2025") %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") 

projection_wpop_gap_de <- tibble(
  year = 2025:2100,
  work_pop = projection_wpop_gap_de$work_pop,
  Projection = "WPop2025") %>%
  select(year, Projection, work_pop)


# then massaging the no migration projection
projection_wpop_cbnm_de <- projection_mortality_de %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  rename(NoMigration = "work_pop")

# joining the current stock and no migration projections
projection_wpop_integral_de <- projection_wpop_cbnm_de %>%
  left_join(projection_wpop_gap_de %>%
              select(year, work_pop) %>%
              rename(WPop2025 = "work_pop"),
            by = "year") %>%
  filter(year >= 2025)

# and finally creating a third series for the pre 2025 true numbers
projection_wpop_observed_de <- projection_wpop_cbnm_de %>%
  filter(year <= 2025) %>%
  rename(Observed = "NoMigration")


# Graphing Migration Gap (Integral) -----------------------------------
ggplot() +
  # integral
  geom_ribbon(
    data = projection_wpop_integral_de,
    aes(x = year,
        ymin = pmin(NoMigration, WPop2025),
        ymax = pmax(NoMigration, WPop2025)),
    fill = "grey70",
    alpha = 0.5) +
  
  # stock at 2025 wpop
  geom_line(
    data = projection_wpop_gap_de %>%
      filter(year>= 2025),
    aes(x = year, y = work_pop, color = "WPop2025"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # nomigration
  geom_line(
    data = projection_wpop_cbnm_de %>%
      filter(year>= 2025),
    aes(x = year, y = NoMigration, color = "NoMigration"),
    linewidth = 0.6,
    linetype = "dashed") +
  
  # true observed
  geom_line(
    data = projection_wpop_observed_de,
    aes(x = year, y = Observed, color = "Observed"),
    linewidth = 0.6) +
  
  labs(
    title = "DE Working-Age Population: Stabilization Gap",
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


ggsave(here("outputs/2_3_wpop_integral_cbnm_de.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)








# THINGS TO NOTE---------------------------------------------------
# 1) when calculating birth rates for each t+1, you are using a rate and applying
# eg 0.9%
# 1.a) this rate is eye-balled off of the DE natality chart, 
# and not based on a further calculation or regression
# 1.b) this rate is being applied to the entire population stock, 
# in which you are currently killing letting people age to 100 and then
# killing them. for a more scientific approach, you would need to 
# adjust this 'tapering-off' of elderly population using observed deaths (as u did with natality)

















# # graphing gap OLD ----------------------------------------------- 
# projection_mortality_de %>%
#   mutate(Projection = "NoMigration") %>%
#   filter(age_numeric >= 18,
#          age_numeric <= 64) %>%
#   group_by(year, Projection) %>%
#   summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
#   rbind(projection_wpop_gap_de) %>%
#   ggplot(aes(x = year, y = work_pop, color = Projection)) +
#   geom_line() +
#   labs(
#     title = "DE 2025 WorkPop vs. No-Migration WorkPop",
#     subtitle = "Eurostat, WPopAge = [18-64]",
#     caption = "Natality Rate 0.9%, 2023 Historical Mortality",
#     x = "Year",
#     y = "Population"
#   ) +
#   scale_color_manual(values = c(
#     "WPop2025" = "#1b9e77",
#     "NoMigration"      = "black")) +  
#   #  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
#   scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale
# 
# 
# ggsave(here("outputs/2_3_wpop_gap_cbnm_de.png"),
#        width = 9,height = 5,   # 2:1 ratio
#        units = "in", dpi = 300)
# 


# # OLD DE entries v exits ----------------------------------
# ## DE stable calcs
# stable_de_calcs_clean <- eurostat_ages_raw %>%
#   rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
#   filter(index %in% c(#"A,NR,Y17,F,DE_TOT",
#     "A,NR,Y18,F,DE_TOT",
#     #"A,NR,Y64,F,DE_TOT",
#     "A,NR,Y65,F,DE_TOT"
#   )) %>% 
#   pivot_longer(cols = -index, # means: pivot everything except this column
#                names_to = "year", # pivot year columns to new single column, 'year'
#                values_to = "pop") %>% # 'Values' = estimates
#   mutate(index = recode(index, 
#                         "A,NR,Y18,F,DE_TOT" = "18yr",
#                         "A,NR,Y65,F,DE_TOT" = "65yr"
#   )) %>%
#   mutate(year = as.integer(year)) %>% # make sure years are read as numbers
#   mutate(pop = parse_number(pop)) %>%   # converts "411641" or "411,641" or "411641 p" -> 411641
#   filter(year >= 2000) %>% 
#   rename(age = "index") %>%
#   select(age, year, pop) # re-order columns
# 
# 
# 
# stable_de_calcs_clean %>%
#   ggplot(aes(x = year, y = pop, color = age)) +
#   #geom_vline(xintercept = 2015, color = "darkred", linetype = "dashed", linewidth = 0.5) +
#   geom_line(linewidth = 0.5) +
#   scale_color_manual(values = c("18yr" = "#1C8C1F",
#                                 "65yr" = "#311380"),
#                      name = "Age") +
#   labs(
#     title = "Entrants and Exits of Labour Market - DE", 
#     subtitle = "Share of Population Aged 18yrs and 65yrs, Annually",
#     x = "Year",
#     y = "Population") +
#   #scale_y_continuous(labels = function(x) paste0(x/1e5, "M"),limits = c(0, 700000)) + 
#   theme(panel.background = element_rect(fill = 'white', color = 'white'), 
#         panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
#         panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
#         legend.position = "bottom",
#         text = element_text(family="Times New Roman"))
# 
# 
# 
# ggsave(here("outputs/2_3_basic_entrants_exits_de.png"),
#        width = 9,height = 5,   # 2:1 ratio
#        units = "in", dpi = 300)
# 
# 






