# libraries -----------------------------------------------------

library(tidyverse)
library(here)

here::i_am("code/2_2_comparison_of_estimates.R")


# Import Eurostat true data ----------------------------------------------------------------
actuals_eurostat <- read_tsv(here("data/eurostat_data/estat_demo_pjan.tsv"),
                             locale = locale(encoding = "UTF-8",
                                             decimal_mark = ",",
                                             grouping_mark = "."))

## extract only FGIS countries
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


# Import UN Historical (2015) data --------------------------------------------------
archival_2015_median <- read_csv(here("data/UN_data/UN_archival_2015_median.csv")) %>%
  rename(Location = "Major area, region, country or area *") %>%
  mutate("Variant" = "UNMedian2015") %>%
  rename(Projection = "Variant")


archival_2015_m_fgis <- archival_2015_median %>%
  filter(Location %in% c("France", "Germany", "Italy", "Spain"))


archival_2015_m_fgis_long <- archival_2015_m_fgis %>%
  pivot_longer(cols = -c(Projection, Location),
               names_to = "year",
               values_to = "Value" # 'Value' = estimate
               ) %>%
  mutate(year = as.integer(year)) %>%
  mutate(Value = str_remove_all(Value, " ")) %>%
  mutate(Value = as.integer(Value)) %>%
  mutate(Value = Value*1000) # raising UN numbers by a factor of 1000 so as 
  # to put them on the same scale as the Eurostat and UN 2024 numbers
  # NOTE: while technically this is methodologically unsound, because we are 
  # increasing Significant Figures (ie: coding it so that 57,243 appears as 57,243,000)
  # it is okay to do, because we are only using it for graphing
  # which anyway lacks the granularity to even see this change
  



  
# graph UN 2015 vs. Eurostat actuals ----------------------------------------------------------------

# bind
comparison_UN15_EurostatActuals_bound <- actuals_eurostat_clean_long %>% 
  filter(!Location %in% c("EU27-(2020)")) %>% # and drop 'EU' location since we're not graphing EU
  mutate(Value = parse_number(Value)) %>% # remove 'flag' characters like the 'b' and 'p'
  rbind(archival_2015_m_fgis_long) %>%
  filter(year <= 2050) %>% # let's get rid of too-far off projections
  filter(year >= 2005) # and start from 10 years before the projections of UN


#letsgoooooooo
fgisprojection <- comparison_UN15_EurostatActuals_bound %>%
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
    "Eurostat2025" = "#1b9e77",
    "UNMedian2015"      = "#7570b3")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e3 means removing 3 zeros from scale
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/Eurostat_UN15_comparison.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


# graph UN 2024 vs. Eurostat actuals ----------------------------------------------------------------


# bind
comparison_UN24_EurostatActuals_bound <- fgis_median %>% # fgis_median is created in another R file
  mutate(Projection = recode(Projection, "Median" = "UNMedian2024")) %>% # change values inside column
  rbind(actuals_eurostat_clean_long) %>%
  filter(!Location %in% c("EU27-(2020)")) %>% # and drop EU observations since we dont need them for graph
  mutate(Value = parse_number(Value)) %>% # remove 'flag' characters like the 'b' and 'p'
  filter(year <= 2050) %>% # let's get rid of too-far off projections
  filter(year >= 2005) # and start from 10 years before the projections of UN


#letsgoooooooo
fgisprojection <- comparison_UN24_EurostatActuals_bound %>%
  ggplot(aes(x = year, y = Value, color = Projection)) +
  #geom_vline(xintercept = 2024, color = "darkred", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  #geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Comparison - True vs. Forecasted Population", 
    subtitle = "Eurostat True Figures vs. UN 2024 Forecast",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "Eurostat2025" = "#1b9e77",
    "UNMedian2024"      = "darkred")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e6 means removing 6 zeros from scale
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/Eurostat_UN24_comparison.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


# UN Error calcs -------------------------------

temp_UN24 <- comparison_UN24_EurostatActuals_bound %>%
  mutate(loc_year_id = paste0(Location,"_",year)) %>%
  filter(Projection == "UNMedian2024") %>%
  rename(UNMedian2024 = "Value")

temp_Eurostat25 <- comparison_UN24_EurostatActuals_bound %>%
  mutate(loc_year_id = paste0(Location,"_",year)) %>%
  filter(Projection == "Eurostat2025") %>%
  rename(Eurostat2025 = "Value")

errors_UN24_Eurostat <- temp_Eurostat25 %>%
  select(-c(Location, year, Projection)) %>%
  left_join(temp_UN24, by = "loc_year_id") %>%
  mutate(UN_Error = round((Eurostat2025 - UNMedian2024),0)) %>%
  select(Location, 
         year, 
         Eurostat2025, 
         UNMedian2024,
         UN_Error)

# UN error graph ----------------------------------------------------------


#letsgoooooooo
fgis_errors <- errors_UN24_Eurostat %>%
  ggplot(aes(x = year, y = UN_Error)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "UN Population Estimate Error by Year, 2005-2024", 
    subtitle = "Error = (Eurostat True Figures) - (UN 2024 Forecast)",
    caption = "'Negative' Error means UN overestimated; Positive Error means UN underestimated true pop",
    x = "Year",
    y = "Error Size (Population)") +
  scale_color_manual(values = "darkred") +
  scale_y_continuous(labels = function(x) paste0(x/1e3, "K")) + # 1e6 means removing 6 zeros from scale
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/UN24_ErrorSize_fgis.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)

# graph all three together ----------------------------------------------------------------

# bind
comparison_allthree_bound <- comparison_UN24_EurostatActuals_bound %>% 
  rbind(comparison_UN15_EurostatActuals_bound) 

#letsgoooooooo
fgisprojection <- comparison_allthree_bound %>%
  ggplot(aes(x = year, y = Value, color = Projection)) +
  #geom_vline(xintercept = 2015, color = "darkred", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Comparison - True vs. Forecasted Population", 
    subtitle = "Eurostat True Figures vs. UN Forecasts (2015, 2024)",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "Eurostat2025" = "#1b9e77",
    "UNMedian2024"      = "darkred",
    "UNMedian2015"      = "#264d73")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) + # 1e3 means removing 3 zeros from scale
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here("outputs/Eurostat_UNBoth_comparison.png"),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)



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
