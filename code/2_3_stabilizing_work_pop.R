# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_stabilizing_work_pop.R")



# Import Eurostat Natality ----------------------------------------------------------------
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
  filter(year >= 1994, # 1994 is the first year where all countries have a value
         year <= 2023  # 2024 some countries dont have data
         ) %>% 
  rename(Location = "index") %>%
  mutate(id = paste0(Location, "_", year), # add column for id
         births = parse_number(births)) %>% # remove 'flag' characters like the 'b' and 'p'
  select(Location, year, id, births) # re-order columns


# calculating natality --------------------------------------------------------

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



# graphing ----------------------------------------------------------------

# graph ----------------------------------------------------------------

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
    title = "Natality Rates in Key EU Countries", 
    subtitle = "Population and Births Eurostat Data",
    x = "Year",
    y = "Rate") +
  scale_y_continuous(limits = c(0, 1.5), breaks = 0:2) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        #legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/NatalityRates_fgis.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)

