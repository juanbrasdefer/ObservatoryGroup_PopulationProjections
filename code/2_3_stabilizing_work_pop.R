# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_stabilizing_work_pop.R")








# Working Pop --------------------------------------------
# ok so the steps we need to take are:
# re-code index name convention
  # or rather, extract from it (stringr):
  # yr denomination - done
  # location - done
  # gender - done
  # ex: A,NR,Y11,T,FR

## Eurostat true data ----------------------------------------------------------------
eurostat_ages_raw <- read_tsv(here("data/eurostat_data/estat_demo_pjan.tsv"),
                             locale = locale(encoding = "UTF-8",
                                             decimal_mark = ",",
                                             grouping_mark = "."))

eurostat_ages_clean <- eurostat_ages_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  mutate(index_parts = str_split(index, ",")) %>% # splits into a character vector
  mutate(age_bracket = map_chr(index_parts, 3),
         gender      = map_chr(index_parts, 4),
         Location    = map_chr(index_parts, 5)) %>%
  select(-index_parts) %>%
  pivot_longer(cols = `1960`:`2025`,
               names_to = "year",
               values_to = "Value") %>%
  filter(Location %in% c("FR",
                         "DE_TOT",
                         "IT",
                         "ES",
                         "EU27_2020")) %>%
  mutate(year = as.integer(year)) %>%
  mutate(Value = na_if(Value, ":"), # convert ":" → NA
         Value = readr::parse_number(Value))   # parse numeric values




# EUROSTAT CLOSED BORDERS implementation -----------------------------
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


## function advancement simple---------------------------------- 
## function for advancing through age categories
# no natality
# no mortality
advance_year_simple <- function(df, t){
  df %>%
    filter(year == t) %>%
    transmute(
      age_numeric = age_numeric + 1,
      year = t + 1,
      Value = Value)
}

### usage ---------------------------------------------------
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
# Natality Rates --------------------------------------------------------
## Eurostat Birth Counts ----------------------------------------------------------------
# importing Eurostat observed birth counts
# it is in the form of either monthly or total births
natality_raw <- read_tsv(here("data/eurostat_data/estat_demo_fmonth.tsv"),
                         locale = locale(encoding = "UTF-8",
                                         decimal_mark = ",",
                                         grouping_mark = "."))


## cleaning
natality_clean <- natality_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  filter(index %in% c("A,NR,TOTAL,EU27_2020", # EU total (using 2020 members, which excludes UK)
                      "A,NR,TOTAL,DE_TOT", # DE total
                      "A,NR,TOTAL,FR", # FR total
                      "A,NR,TOTAL,IT", # IT total
                      "A,NR,TOTAL,ES"  # ES total
  )) %>% 
  mutate(index = recode(index, 
                        "A,NR,TOTAL,EU27_2020" = "EU27-(2020)",
                        "A,NR,TOTAL,DE_TOT" = "Germany", # DE total
                        "A,NR,TOTAL,FR" = "France",
                        "A,NR,TOTAL,IT" = "Italy",
                        "A,NR,TOTAL,ES" = "Spain"
  )) %>%
  pivot_longer(cols = -index, # means: pivot everything except this column
               names_to = "year", # pivot year columns to new single column, 'year'
               values_to = "births") %>% 
  mutate(year = as.integer(year)) %>% # make sure years are read as numbers
  filter(year >= 2000, # 2000 as beginning year
         year <= 2023  # 2024 some countries dont have data
  ) %>% 
  rename(Location = "index") %>%
  mutate(id = paste0(Location, "_", year), # add column for id
         births = parse_number(births)) %>% # remove 'flag' characters like the 'b' and 'p'
  select(Location, year, id, births) # re-order columns


## Calculating natality rates --------------------------------------------------------
# join with full population numbers to calculate natality RATES
# I THINK THE EU27 POP NUMBERS ARE INCORRECT (only 200k in 2024??)
natality_rates <- actuals_eurostat_clean_long %>%
  mutate(id = paste0(Location, "_", year)) %>% # add column for id
  rename(population = "Value") %>%
  filter(year <= 2024) %>% # EU27 does not have data for 2025
  mutate(population = parse_number(population)) %>% # remove flags
  select(-year, -Location, -Projection) %>% # removing redundancy since this info is in the join
  right_join(natality_clean, by = "id") %>%
  select(Location, year, population, births) %>%
  mutate(natality_rate = round(((births/population)*100), 2))



## FGIS Natality Rates graphing----------------------------------------------------------------

#fgis
natality_rates %>%
  filter(!Location %in% c("EU27-(2020)")) %>% # drop EU observations since we dont need them for graph
  ggplot(aes(x = year, y = natality_rate
             #, color = Projection
  )) +
  #geom_vline(xintercept = 2015, color = "darkred", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Natality Rates in Key EU Countries (2000-2024)", 
    subtitle = "Population and Births Eurostat Data [2025]",
    x = "Year",
    y = "Rate") +
  scale_y_continuous(limits = c(0, 1.5), breaks = 0:2) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        #legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/2_3_natalityrates_fgis.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


## function advancement natality ---------------------------------- 
advance_year_natality <- function(df, t, birth_rate){
  current <- df %>% 
    filter(year == t)
  
  # everyone ages forward
  aged <- current %>%
    filter(age_numeric < 100) %>%   # cap
    transmute(
      age_numeric = age_numeric + 1,
      year = t + 1,
      Value = Value
    )
  
  # new 1-year-olds added, using totalpop * birthrate
  births_count <- sum(current$Value) * birth_rate
  
  newborns <- tibble(
    year = t + 1,
    age_numeric = 1,
    Value = births_count
  )
  
  bind_rows(aged, newborns)
}

#### usage ----------------------------------------------------
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


### natality working pop histogram -----------------------------------------
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








# Mortality Rates --------------------------------------------------------
## Import Mortality ----------------------------------------------------------------
# importing Eurostat observed mortality counts
# it is in the form of either monthly or total births
mortality_raw <- read_tsv(here("data/eurostat_data/estat_demo_magec.tsv"),
                         locale = locale(encoding = "UTF-8",
                                         decimal_mark = ",",
                                         grouping_mark = "."))

mortality_clean <- mortality_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  mutate(index_parts = str_split(index, ",")) %>% # splits into a character vector
  mutate(gender      = map_chr(index_parts, 3),
         age_bracket = map_chr(index_parts, 4),
         Location    = map_chr(index_parts, 5)) %>%
  select(-index_parts) %>%
  filter(Location %in% c("FR",
                         "DE_TOT",
                         "IT",
                         "ES",
                         "EU27_2020")) %>%
  pivot_longer(cols = `1960`:`2024`,
               names_to = "year",
               values_to = "Value") %>%
  mutate(year = as.integer(year)) %>%
  mutate(Value = na_if(Value, ":"), # convert ":" → NA
         Value = readr::parse_number(Value))   # parse numeric values


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



## function advancement mortality ---------------------------------------

# we create one final function that takes all three
# things into account (under closed borders eurostat):
# 1) working pop entries/retirements
# 2) natality
# 3) working age mortality
# NOTE: THIS DOES NOT APPLY MORTALITY RATES TO 65+, 
# and instead brute-force cuts all life at 100

advance_year_mortality <- function(df, t, birth_rate, mortality_table){
  
  current <- df %>%
    filter(year == t)
  
  # join mortality
  current <- current %>%
    left_join(mortality_table, by = "age_numeric") %>%
    mutate(mortality_rate = ifelse(is.na(mortality_rate), 0, mortality_rate))
  
  # apply survival
  survivors <- current %>%
    mutate(Value = round((Value * (1 - mortality_rate)),0))
  
  # age forward (cap at 100)
  aged <- survivors %>%
    filter(age_numeric < 100) %>%
    transmute(
      age_numeric = age_numeric + 1,
      year = t + 1,
      Value = Value
    )
  
  # births (based on surviving population)
  births_count <- round((sum(survivors$Value, na.rm = TRUE) * birth_rate),0)
  
  newborns <- tibble(
    year = t + 1,
    age_numeric = 1,
    Value = births_count
  )
  
  bind_rows(aged, newborns)
}





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


### wpop all (cb, n, m) projection -----------------------------------------

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

projection_wpop_gap_de <- advanceable_n_de %>%
  select(year, age_numeric, Value) %>%
  filter(year == 2025) %>%
  mutate(Projection = "2025WPop") %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") 

projection_wpop_gap_de <- tibble(
  year = 2025:2100,
  work_pop = projection_wpop_gap_de$work_pop,
  Projection = "2025WPop") %>%
  select(year, Projection, work_pop)

projection_mortality_de %>%
  mutate(Projection = "NoMigration") %>%
  filter(age_numeric >= 18,
         age_numeric <= 64) %>%
  group_by(year, Projection) %>%
  summarise(work_pop = sum(Value, na.rm = TRUE), .groups="drop") %>%
  rbind(projection_wpop_gap_de) %>%
  ggplot(aes(x = year, y = work_pop, color = Projection)) +
  geom_line() +
  labs(
    title = "DE 2025 WorkPop vs. No-Migration WorkPop",
    subtitle = "Eurostat, WPopAge = [18-64]",
    caption = "Natality Rate 0.9%, 2023 Historical Mortality",
    x = "Year",
    y = "Population"
  ) +
  scale_color_manual(values = c(
    "2025WPop" = "#1b9e77",
    "NoMigration"      = "black")) +  
#  geom_vline(xintercept = 2025, color = "lightblue", linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) # 1e6 means removing 6 zeros from scale


ggsave(here("outputs/2_3_wpop_gap_cbnm_de.png"),
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


# DETOUR - EUROSTATPROJECTIONSEXIST -----------------------------------------------
temp <- read_tsv(here("data/eurostat_data/estat_proj_23np.tsv.gz"),
                 locale = locale(encoding = "UTF-8",
                                 decimal_mark = ",",
                                 grouping_mark = "."))



# OLD DE entries v exits ----------------------------------
## DE stable calcs
stable_de_calcs_clean <- eurostat_ages_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  filter(index %in% c(#"A,NR,Y17,F,DE_TOT",
    "A,NR,Y18,F,DE_TOT",
    #"A,NR,Y64,F,DE_TOT",
    "A,NR,Y65,F,DE_TOT"
  )) %>% 
  pivot_longer(cols = -index, # means: pivot everything except this column
               names_to = "year", # pivot year columns to new single column, 'year'
               values_to = "pop") %>% # 'Values' = estimates
  mutate(index = recode(index, 
                        "A,NR,Y18,F,DE_TOT" = "18yr",
                        "A,NR,Y65,F,DE_TOT" = "65yr"
  )) %>%
  mutate(year = as.integer(year)) %>% # make sure years are read as numbers
  mutate(pop = parse_number(pop)) %>%   # converts "411641" or "411,641" or "411641 p" -> 411641
  filter(year >= 2000) %>% 
  rename(age = "index") %>%
  select(age, year, pop) # re-order columns



stable_de_calcs_clean %>%
  ggplot(aes(x = year, y = pop, color = age)) +
  #geom_vline(xintercept = 2015, color = "darkred", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("18yr" = "#1C8C1F",
                                "65yr" = "#311380"),
                     name = "Age") +
  labs(
    title = "Entrants and Exits of Labour Market - DE", 
    subtitle = "Share of Population Aged 18yrs and 65yrs, Annually",
    x = "Year",
    y = "Population") +
  #scale_y_continuous(labels = function(x) paste0(x/1e5, "M"),limits = c(0, 700000)) + 
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))



ggsave(here("outputs/2_3_basic_entrants_exits_de.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


