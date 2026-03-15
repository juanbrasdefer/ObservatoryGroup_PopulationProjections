# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/2_3_stabilizing_EURO_revisited.R")

# NEW STUFF - UN NATALITY AND MORTALITY RATES ---------------------------------

WPP24_EU27_nat_raw <- read_csv(here("data/UN_data/UN_WPP24_EU27_Natality.csv")) %>%
  rename(CountryCode = "Iso3",
         year = "Time",
         Projection = "Variant") 
WPP24_EU27_mort_raw <- read_csv(here("data/UN_data/UN_WPP24_EU27_Natality.csv")) %>%
  rename(CountryCode = "Iso3",
         year = "Time",
         Projection = "Variant") 
  

# DETOUR - EUROSTATPROJECTIONSEXIST -----------------------------------------------
# projections available to us are:
# BSL     - Baseline
# HMIGR   - High Migration  
# LFRT    - Low Fertility
# LMIGR   - Low Migration
# LMRT    - Low Mortality
# NMIGR   - No Migration
eurostat_official_projection_raw <- read_tsv(here("data/eurostat_data/estat_proj_23np.tsv.gz"),
                 locale = locale(encoding = "UTF-8",
                                 decimal_mark = ",",
                                 grouping_mark = "."))


eurostat_official_projection_clean <- eurostat_official_projection_raw %>%
  rename(index = 1) %>% # rename column "freq,unit,age,sex,geo\TIME_PERIOD" using its index, 1
  mutate(index_parts = str_split(index, ",")) %>% # splits into a character vector
  mutate(Projection  = map_chr(index_parts, 2),
         gender      = map_chr(index_parts, 3),
         age_bracket = map_chr(index_parts, 4),
         unknown_col = map_chr(index_parts, 5), # no idea what "PER" stands for, but all rows say "PER", so it gives us no new information; we drop it later
         Location    = map_chr(index_parts, 6)) %>%
  select(-index_parts) %>%
  pivot_longer(cols = `2022`:`2100`,
               names_to = "year",
               values_to = "Value") %>%
  filter(Location == "EU27_2020") %>%
  filter(gender == "T") %>%
  mutate(year = as.integer(year)) %>%
  select(-unknown_col)



# just to compare 2024 val against UN and eurostat other #s
eurostat_eurproj_value <- eurostat_official_projection_clean %>%
  filter(Projection == "BSL") %>%
  filter(age_bracket == "TOTAL") %>%
  filter(year == 2024) %>%
  pull(Value)

# BSL     - Baseline
# HMIGR   - High Migration  
# LFRT    - Low Fertility
# LMIGR   - Low Migration
# LMRT    - Low Mortality
# NMIGR   - No Migration


eurostat_official_projection_clean %>%
  filter(age_bracket == "TOTAL") %>%
  filter(!(Projection %in% c("LFRT",
                             "LMRT"))) %>%
  ggplot(aes(x = year, y = Value, color = Projection))+
  geom_line(linetype = 1, linewidth = 0.5) +
  labs(
    title = paste0("Eurostat Population Projections - EU27"),
    subtitle = "Baseline, HighMigration, LowMigration, NoMigration",
    x = "Year",
    y = "Population") +
  scale_color_manual(values = c(
    "BSL" = "#1b9e77",
    "NMIGR"      = "black",
    "HMIGR"      = "darkred",
    "LMIGR" = "orange")) +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid'),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.5),
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/2_3_new_EUROSTAT_projections.png")))

