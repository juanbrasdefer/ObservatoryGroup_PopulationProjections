# libraries -----------------------------------------------------

library(tidyverse)
library(here)

here::i_am("code/2_2_comparison_of_estimates.R")



# Import UN data --------------------------------------------------
archival_2015_median <- read_csv(here("data/UN_data/UN_archival_2015_median.csv")) %>%
  rename(Location = "Major area, region, country or area *") %>%
  mutate("Variant" = "Median_2015") %>%
  rename(Projection = "Variant")


archival_2015_m_EU <- archival_2015_median %>%
  filter(Location == "EUROPE")


archival_2015_m_EU_long <- archival_2015_m_EU %>%
  pivot_longer(
    cols = -c(Projection, Location),
    names_to = "year",
    values_to = "Value" # 'Value' = estimate
  ) %>%
  mutate(
    year = as.integer(year))


# Import Eurostat true data ----------------------------------------------------------------
actuals_eurostat <- read_tsv(here("data/eurostat_data/estat_demo_pjan.tsv"),
               locale = locale(encoding = "UTF-8",
                               decimal_mark = ",",
                               grouping_mark = "."))

## FGIS
actuals_eurostat_clean_long <- actuals_eurostat %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  filter(index %in% c("A,NR,TOTAL,M,EU27_2020", # Europe 27 members (2020, excludes UK)
                      "A,NR,TOTAL,T,DE_TOT", # germany total ages, total sexes, total country
                      "A,NR,TOTAL,T,FR", # france, total ages, total sexes
                      "A,NR,TOTAL,T,ES", # spain, total ages, total sexes
                      "A,NR,TOTAL,T,IT")) %>% # italy, total ages, total sexes
  pivot_longer(cols = -index, # means: pivot everything except this column
               names_to = "year", # pivot year columns to new single column, 'year'
               values_to = "Value") %>% # 'Values' = estimates
  mutate(index = recode(index, 
                        "A,NR,TOTAL,M,EU27_2020" = "EU27-(2020)",
                        "A,NR,TOTAL,T,DE_TOT" = "Germany", 
                        "A,NR,TOTAL,T,FR" = "France",
                        "A,NR,TOTAL,T,ES" = "Spain",
                        "A,NR,TOTAL,T,IT" = "Italy")) %>%
  mutate(year = as.integer(year)) %>% # make sure years are read as numbers
  filter(year >= 1991) %>% # 1991 is the first year where all countries have a value
                                # france had no values before 1991
  rename(Location = "index") %>%
  mutate(Projection = "Eurostat2025") %>% # add column to show what projection this is
  select(Location, year, Projection, Value) # re-order columns

# bind
comparison_UN_EurostatActuals_bound <- fgis_median %>%
  mutate(Projection = recode(Projection, "Median" = "UNMedian")) %>%
  rbind(actuals_eurostat_clean_long) %>%
  filter(!Location %in% c("EU27-(2020)")) %>% # and drop EU observations since we dont need them for graph
  mutate(Value = parse_number(Value)) %>% # remove 'flag' characters like the 'b' and 'p'
  filter(year <= 2050) %>% # let's get rid of too-far off projections
  filter(year >= 2005) # and start from 10 years before the projections of UN
  
# graph ----------------------------------------------------------------
#letsgoooooooo
fgisprojection <- comparison_UN_EurostatActuals_bound %>%
  ggplot(aes(x = year, y = Value, color = Projection)) +
  geom_vline(xintercept = 2015, color = "darkred", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  #geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Comparison - True vs. Forecasted Population", 
    subtitle = "Eurostat True Figures vs. UN 2015 Forecast",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "UNMedian" = "#1b9e77",
    "Eurostat2025"      = "#7570b3")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/Eurostat_UN_comparison.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)
# 
# 
# # 2019 point-in-time data ------------------------------------------------------
# archival_2019_median <- read_csv(here("data/UN_data/UN_archival_2019_median.csv")) %>%
#   rename(Location = "Region, subregion, country or area *") %>%
#   mutate("Variant" = "Median") %>%
#   rename(Projection = "Variant")
# 
# archival_2019_m_EU <- archival_2019_median %>%
#   filter(Location == "EUROPE")
# 
# 
# temp <- archival_2019_m_EU %>%
#   pivot_longer(
#     cols = -c(Projection, Location),
#     names_to = "year",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     year = as.integer(year))










# Crystal - Eurostat data ----------------------------------------------------------------
eurostat_baseline <- read_csv(here("data/eurostat_data/Eurostat Projections.csv"))

# renaming columns to match the naming conventions from UN data
eurostat_baseline <- eurostat_baseline %>%
  rename(Projection = "Type of projection",
         Location = "Geopolitical entity (reporting)",
         year = "TIME_PERIOD",
         Value = "OBS_VALUE")



# Crystal - Graphing Eurostat Projections ------------------------------------
eurostat_baseline %>%
  ggplot(aes(x = year, y = Value, color = Projection)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Eurostat Working Population Projections: France, Germany, Italy, Spain", 
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
      "Baseline projections" = "#1b9e77",
      "Sensitivity test: no migration"      = "#7570b3")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave(here("outputs/Eurostat_Projections.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)
