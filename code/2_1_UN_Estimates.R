### UN - Demographics Trends and Immigration 

# Libraries -----------------------------------------------------------------
#install.packages("gridExtra")
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(tidyverse)
library(here)

# set directory
here::i_am("code/2_1_UN_Estimates.R")


# Asia -------------------------------------------------------------
## Import All Asia ---------------------------------------------------------
WPP24_asia_raw <- read_csv(here("data/UN_data/UN_WPP24_Asia.csv")) %>%
  select(Location,
         Iso3,
         Time,
         Variant,
         Value) %>%
  rename(CountryCode = "Iso3",
         year = "Time",
         Projection = "Variant")


## All Asia Graphs -----------------------------------------

# set variable to one of: "China", "India", "Japan"
country_to_graph <- "Japan"

WPP24_asia_raw %>%
  filter(Location == country_to_graph) %>%
  ggplot(aes(x = year, y = Value, color = Projection))+
    geom_line(linetype = 1, linewidth = 0.5) +
    #geom_point(size = 0.5) +
    labs(
      title = paste0("UN Population Projection - ",country_to_graph),
      subtitle = "Median, High, Low Projections and No-Migration Scenario",
      x = "Year",
      y = "Population") +
    scale_color_manual(values = c(
      "95% upper bound" = "lightgrey",
      "Median" = "#1b9e77",
      "95% lower bound" = "lightgrey",
      "Zero-migration"      = "black")) +
    scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
    theme(panel.background = element_rect(fill = 'white', color = 'white'), 
          panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid'),
          panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.5),
          text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/2_1_UN_WPP24_",country_to_graph,".png")))



# US -------------------------------------------------------------
## Import US WPP ---------------------------------------------------------
WPP24_US_raw <- read_csv(here("data/UN_data/UN_WPP24_US.csv")) %>%
  select(Location,
         Iso3,
         Time,
         Variant,
         Value) %>%
  rename(CountryCode = "Iso3",
         year = "Time",
         Projection = "Variant")

## Import US Migration Census numbers ----------------------------
uscensus_netmig_long <- read_csv(here("data/US_Census/NST-EST2025-ALLDATA.csv")) %>%
  filter(NAME == "United States") %>% # total country, not by state
  select(NAME,
         NETMIG2020,
         NETMIG2021,
         NETMIG2022,
         NETMIG2023,
         NETMIG2024,
         NETMIG2025) %>%
  pivot_longer(-NAME,
               names_to = "time_period",
               values_to = "net_migration")

# final Biden year
us_netmig_24 <- uscensus_netmig_long %>%
  filter(time_period == "NETMIG2024") %>%
  pull(net_migration)

# first Trump (2nd term) year
us_netmig_25 <- uscensus_netmig_long %>%
  filter(time_period == "NETMIG2025") %>%
  pull(net_migration)



## Calculating low and high scenarios ------------------------------
US_high <- WPP24_US_raw %>%
  filter(Projection == "Zero-migration") %>%
  arrange(year) %>%
  mutate(index = row_number()) %>%
  mutate(cumulative_mig = index * us_netmig_24) %>%
  mutate(Value = Value + cumulative_mig) %>%
  mutate(Projection = "High-migration") %>%
  select(-index,
         -cumulative_mig)

US_low <- WPP24_US_raw %>%
  filter(Projection == "Zero-migration") %>%
  arrange(year) %>%
  mutate(index = row_number()) %>%
  mutate(cumulative_mig = index * us_netmig_25) %>%
  mutate(Value = Value + cumulative_mig) %>%
  mutate(Projection = "Low-migration") %>%
  select(-index,
         -cumulative_mig)

WPP24_US_scenarios <- WPP24_US_raw %>%
  rbind(US_high) %>%
  rbind(US_low)

## US Graph -----------------------------------------

WPP24_US_scenarios %>%
  filter(!(Projection %in% c("95% upper bound",
                           "95% lower bound"))) %>%
  ggplot(aes(x = year, y = Value, color = Projection))+
  geom_line(linetype = 1, linewidth = 0.5) +
  #geom_point(size = 0.5) +
  labs(
    title = paste0("Population Projection - United States"),
    subtitle = "Low and High Migration Scenarios, UN Median Scenario",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "Median" = "#1b9e77",
    "Zero-migration"      = "black",
    "High-migration"      = "darkred",
    "Low-migration" = "orange")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid'),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.5),
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/2_1_UN_WPP24&LowHighMig_UnitedStates.png")))



# WPP ALL, new ---------------------------------------------

# Europe -------------------------------------------------------------
## UN EU27 raw data ---------------------------------------
WPP24_eur_pop_raw <- read_csv(here("data/UN_data/UN_WPP24_Eur27_Pop.csv")) %>%
  select(Location,
         Iso3,
         Time,
         Variant,
         Value) %>%
  rename(CountryCode = "Iso3",
         year = "Time",
         Projection = "Variant") 

WPP24_eur_pop_eu27 <- WPP24_eur_pop_raw %>%
  group_by(year, Projection) %>%
  summarise(Value = sum(Value)) %>%
  mutate(CountryCode = "EU27",
         Location = "European Union (27)")


un_wpp24_erroneous_eur_24val <- WPP24_eur_pop_eu27 %>%
  filter(year == 2024) %>%
  filter(Projection == "Median") %>%
  pull(Value)
  
## eurostat raw data --------------------------------------
eurostat_raw <- read_tsv(here("data/eurostat_data/estat_demo_pjan.tsv"),
                             locale = locale(encoding = "UTF-8",
                                             decimal_mark = ",",
                                             grouping_mark = "."))

## extract EUROPE
eurostat_eur_long <- eurostat_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  filter(index %in% c("A,NR,TOTAL,T,EU27_2020")) %>% 
  pivot_longer(cols = -index, # means: pivot everything except this column
               names_to = "year", # pivot year columns to new single column, 'year'
               values_to = "Value") %>% # 'Values' = estimates
  mutate(index = recode(index, "A,NR,TOTAL,M,EU27_2020" = "EU27")) %>%
  mutate(year = as.integer(year)) %>% # make sure years are read as numbers
  filter(year >= 1990,     # UN starting year
         year < 2025) %>%  # no 2025 data
  rename(Location = "index") %>%
  mutate(Value = parse_number(Value)) %>% # remove 'flag' characters from Eurostat numbers like the 'b' and 'p' 
  mutate(Projection = "Eurostat2025") %>% # add column to show what projection this is
  select(Location, year, Projection, Value) # re-order columns
  
eurostat_eur_24val <- eurostat_eur_long %>%
  filter(year == 2024) %>%
  pull(Value)

## UN Error on EU estimate -----------------------------------
UN_24_EU27_error <- un_wpp24_erroneous_eur_24val - eurostat_eur_24val
print(UN_24_EU27_error)
# literally off by less than 1 million
# law of large numbers i guess
# because their estimates are much worse when 
# looking at individual countries (such as fgis)


EU_jointproj_graphable <- WPP24_eur_pop_eu27 %>%
  filter(Projection %in% c("Median",
                           "Zero-migration"))
  





# OLD EU ---------------------------------------------------------
# EU -Total population
eu_median <- read_csv(here("data/UN_data/UN Population Data EU.csv")) %>% # data is country-level
  rename(year = "Time")
eu_median <- aggregate(Value ~ year, data = eu_median, FUN = sum) # so we aggregate to reach Euro level
eu_nomigration <- read_csv(here("data/UN_data/UN Population Data EU No Migration.csv")) %>%
  rename(year = "Time")
eu_nomigration <- aggregate(Value ~ year, data = eu_nomigration, FUN = sum) # same aggregation step
eu_median$Projection <- "Median"
eu_nomigration$Projection <- "No Migration"
eu_bound <- rbind(eu_median, eu_nomigration)

# EU Zoom In - France, Germany, Italy, Spain
fgis_median <- read_csv(here("data/UN_data/UN Population Data FGIS.csv")) %>%
  rename(year = "Time") %>%
  mutate(Projection = recode(Projection, "Median" = "UNMedian2024")) # change values inside column
  
fgis_nomigration <- read_csv(here("data/UN_data/UN Population Data FGIS No Migration.csv")) %>%
  rename(year = "Time")
fgis_bound <- rbind(fgis_median, fgis_nomigration)


# OLD US ----------------------------------------------------------
# US - High level, total population
us_median <- read_csv(here("data/UN_data/UN Population Data US.csv")) %>%
  rename(year = "Time")
us_nomigration <- read_csv(here("data/UN_data/UN Population Data US No Migration.csv")) %>%
  rename(year = "Time")
us_median$Projection <- "Median"
us_nomigration$Projection <- "No Migration"
us_bound <- rbind(us_median, us_nomigration)





# Graphing -----------------------------------------------------------
graph_UNprojection <- function(df, region_name){
  df %>%
    ggplot(aes(x = year, y = Value, color = Projection))+
    geom_line(linetype = 1, linewidth = 0.5) +
    #geom_point(size = 0.5) +
    labs(
      title = paste0("UN Population Projection - ",region_name),
      subtitle = "Median Estimate and No-Migration Scenarios",
      x = "Year",
      y = "Population") +
    scale_color_manual(values = c(
      "Median" = "#1b9e77",
      "No Migration"      = "lightgrey")) +
    scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
    theme(panel.background = element_rect(fill = 'white', color = 'white'), 
          panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid'),
          panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.5),
          text = element_text(family="Times New Roman"))
  ggsave(here(paste0("outputs/UN_projections_",region_name,".png")))
}

## Usage
india_bound %>%
  graph_UNprojection("India")

japan_bound %>%
  graph_UNprojection("Japan")

china_bound %>%
  graph_UNprojection("China")

eu_bound %>%
  graph_UNprojection("European Union")

us_bound %>%
  graph_UNprojection("United States")

fgisprojection <- ggplot(fgis_bound, aes(x = year, y = Value, color = Projection)) +
  geom_line(linewidth = 0.5) +
  #geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "UN Population Projection - France, Germany, Italy, Spain", 
    subtitle = "Median Estimate and No-Migration Scenarios",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "Median" = "#1b9e77",
    "Zero-migration"      = "lightgrey")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/UN_projections_FGIS.png")),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


