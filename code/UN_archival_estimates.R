# libraries -----------------------------------------------------

library(tidyverse)
library(here)

here::i_am("code/UN_archival_estimates.R")



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
eurostat_feb2024 <- read_tsv(here("data/eurostat_data/estat_demo_pjan.tsv"),
               locale = locale(encoding = "UTF-8",
                               decimal_mark = ",",
                               grouping_mark = "."))

## FGIS
eurostat_feb2024_fgis_long <- eurostat_feb2024 %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  filter(index %in% c("A,NR,TOTAL,T,DE_TOT", # germany total ages, total sexes, total country
                      "A,NR,TOTAL,T,FR", # france, total ages, total sexes
                      "A,NR,TOTAL,T,ES", # spain, total ages, total sexes
                      "A,NR,TOTAL,T,IT")) %>% # italy, total ages, total sexes
  pivot_longer(cols = -index, # means: pivot everything except this column
               names_to = "year", # pivot year columns to new single column, 'year'
               values_to = "Value") %>% # 'Values' = estimates
  mutate(index = recode(index, 
                        "A,NR,TOTAL,T,DE_TOT" = "Germany", 
                        "A,NR,TOTAL,T,FR" = "France",
                        "A,NR,TOTAL,T,ES" = "Spain",
                        "A,NR,TOTAL,T,IT" = "Italy")) %>%
  mutate(year = as.integer(year)) %>% # make sure years are read as numbers
  filter(year >= 1991) %>% # 1991 is the first year where all countries have a value
                                # france had no values before 1991
  rename(Location = "index") %>%
  mutate(Projection = "Eurostat2024", # add column to show what projection this is
         Value = parse_number(Value)) %>% # remove 'flag' characters like the 'b' and 'p'
  select(Location, year, Projection, Value) # re-order columns

# bind
comparison_UN_Eurostat2024_bound <- fgis_median %>%
  rename(year = "Time") %>%
  rbind(eurostat_feb2024_fgis_long)

# graph
#letsgoooooooo
fgisprojection <- comparison_UN_Eurostat2024_bound %>%
  ggplot(aes(x = year, y = Value, color = Projection)) +
  geom_line(linewidth = 0.5) +
  #geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "UN Population Projection - France, Germany, Italy, Spain", 
    subtitle = "Median Estimate and No-Migration Scenarios",
    x = "Year",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2))
theme(legend.position = "bottom")




# 2019 point-in-time data ------------------------------------------------------
archival_2019_median <- read_csv(here("data/UN_data/UN_archival_2019_median.csv")) %>%
  rename(Location = "Region, subregion, country or area *") %>%
  mutate("Variant" = "Median") %>%
  rename(Projection = "Variant")

archival_2019_m_EU <- archival_2019_median %>%
  filter(Location == "EUROPE")


temp <- archival_2019_m_EU %>%
  pivot_longer(
    cols = -c(Projection, Location),
    names_to = "year",
    values_to = "Value"
  ) %>%
  mutate(
    year = as.integer(year))

