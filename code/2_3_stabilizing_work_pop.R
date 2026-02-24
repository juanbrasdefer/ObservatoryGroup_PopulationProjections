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
natality_rates_europe <- actuals_eurostat_clean_long %>%
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
natality_rates_europe %>%
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









# DETOUR - EUROSTATPROJECTIONSEXIST -----------------------------------------------
# temp <- read_tsv(here("data/eurostat_data/estat_proj_23np.tsv.gz"),
#                  locale = locale(encoding = "UTF-8",
#                                  decimal_mark = ",",
#                                  grouping_mark = "."))




