# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/1_1_comparing_growth.R")

# GDP -----------------------------------------------------
## load gdp data -----------------------------------------------

wb_gdp_growth_raw <- read_csv(here("data/world_bank/worldbank_gdp_growth.csv"))

wb_gdp_growth_comparison <- wb_gdp_growth_raw %>%
  rename(CountryName = "Country Name",
         CountryCode = "Country Code",
         IndicatorN = "Indicator Name",
         IndicatorC = "Indicator Code") %>%
  filter(CountryCode %in% c("USA", 
                            "EUU")) %>%
  select(-c(IndicatorN,
            IndicatorC)) %>%
  pivot_longer(cols = -c(CountryName, CountryCode), # pivot everything except these two columns
               names_to = "year", # pivot all other columns to new column, 'year'
               values_to = "gdp_growth") %>% # values to new pct growth col
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2000,
         year <= 2024)




## graph - single country gdp ----------------------------------------

country_to_plot = "USA"

wb_gdp_growth_comparison %>%
  filter(CountryCode == country_to_plot) %>%
  ggplot(aes(x = year, y = gdp_growth)) +
  geom_point(size = 2, color = "navyblue", alpha = 0.7) +  # Simple scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = "#c4167c", se = FALSE,
              linetype = "dashed") +  # Regression line
  labs(
    title = paste0(country_to_plot, " - Annual GDP Growth"),
    subtitle = "2000 - 2024, World Bank Data",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    text = element_text(family="Times New Roman"))


ggsave(here(paste0("outputs/1_1_", country_to_plot,"_gdpgrowth.png")))







## graph - gdp comparison ------------------------------------------------
wb_gdp_growth_comparison %>%
  ggplot(aes(x = year, y = gdp_growth, color = CountryCode)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste0("Comparison - EU vs. US Annual GDP Growth"), 
    subtitle = "2000 - 2024, World Bank Data",
    x = "Year",
    y = "GDP Growth (%)") +
    scale_color_manual(values = c(
    EUU = "navyblue",
    USA      = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here(paste0("outputs/1_1_comparison_gdpgrowth.png")))


# POP -------------------------------------------------------------
## load population data -----------------------------------------------

wb_pop_growth_raw <- read_csv(here("data/world_bank/worldbank_population_growth.csv"))

wb_pop_growth_comparison <- wb_pop_growth_raw %>%
  rename(CountryName = "Country Name",
         CountryCode = "Country Code",
         IndicatorN = "Indicator Name",
         IndicatorC = "Indicator Code") %>%
  filter(CountryCode %in% c("USA", 
                            "EUU")) %>%
  select(-c(IndicatorN,
            IndicatorC)) %>%
  pivot_longer(cols = -c(CountryName, CountryCode), # pivot everything except these two columns
               names_to = "year", # pivot all other columns to new column, 'year'
               values_to = "pop_growth") %>% # values to new pct growth col
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2000,
         year <= 2024)



## graph - single country pop -------------------------------------------------

country_to_plot = "USA"

wb_pop_growth_comparison %>%
  filter(CountryCode == country_to_plot) %>%
  ggplot(aes(x = year, y = pop_growth)) +
  geom_point(size = 2, color = "navyblue", alpha = 0.7) +  # Simple scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = "#c4167c", se = FALSE,
              linetype = "dashed") +  # Regression line
  labs(
    title = paste0(country_to_plot, " - Annual Pop Growth"),
    subtitle = "2000 - 2024, World Bank Data",
    x = "Year",
    y = "Pop Growth (%)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    text = element_text(family="Times New Roman"))


ggsave(here(paste0("outputs/1_1_", country_to_plot,"_popgrowth.png")))



## graph - pop comparison ------------------------------------------------
wb_pop_growth_comparison %>%
  ggplot(aes(x = year, y = pop_growth, color = CountryCode)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste0("Comparison - EU vs. US Annual Pop Growth"), 
    subtitle = "2000 - 2024, World Bank Data",
    x = "Year",
    y = "Pop Growth (%)") +
  scale_color_manual(values = c(
    EUU = "navyblue",
    USA      = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here(paste0("outputs/1_1_comparison_popgrowth.png")))




# BOTH --------------------------------------------------------
wb_adjusting_growth <- wb_gdp_growth_comparison %>%
  left_join(wb_pop_growth_comparison,
            by = c("CountryCode", "CountryName", "year")) %>%
  mutate(gdp_percap_growth = gdp_growth - pop_growth)
  #mutate(gdp_percap_growth = log(1 + gdp_growth/100) - log(1 + pop_growth/100))




## graph - adjusted gdp comparison ------------------------------------------------
wb_adjusting_growth %>%
  #filter(year < 2020) %>%
  ggplot(aes(x = year, y = gdp_percap_growth, color = CountryCode)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste0("Comparison - EU vs. US Adjusted GDP Growth"), 
    subtitle = "GDP Growth per unit of Pop Growth",
    x = "Year",
    y = "Adjusted GDP Growth (%)") +
  scale_color_manual(values = c(
    EUU = "navyblue",
    USA      = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))


ggsave(here(paste0("outputs/1_1_comparison_percapgrowth.png")))

  

# GPT SUGGESTIONS ----------------------------------------------

## plot 1 ------------------------------------
wb_gdp_growth_comparison %>%
  ggplot(aes(x = year, y = gdp_growth, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU Aggregate GDP Growth",
    subtitle = "2000–2024",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_1_gpt_comparison_gdp_growth.png")))




## plot 2 --------------------------------------------

wb_adjusting_growth %>%
  ggplot(aes(x = year, y = gdp_percap_growth, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - Per Capita GDP Growth",
    subtitle = "[GDP Growth %] – [Population Growth %]",
    x = "Year",
    y = "Per Capita GDP Growth (%)"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_1_gpt_comparison_percapgdp_growth.png")))




## 3rd graph with all on one -----------------------------------
wb_plot_data <- wb_adjusting_growth %>%
  select(CountryCode, year, gdp_growth, gdp_percap_growth) %>%
  pivot_longer(cols = c(gdp_growth, gdp_percap_growth),
               names_to = "measure",
               values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     gdp_growth = "Aggregate GDP Growth",
                     gdp_percap_growth = "Per Capita GDP Growth"))



wb_plot_data %>%
  ggplot(aes(x = year,
             y = value,
             color = CountryCode,
             linetype = measure,
             linewidth = measure,
             alpha = measure)) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(EUU = "navyblue",
                                USA = "darkred")) +
  scale_linetype_manual(values = c(
    "Per Capita GDP Growth" = "solid",
    "Aggregate GDP Growth" = "dashed"
  )) +
  scale_alpha_manual(values = c(
    "Per Capita GDP Growth" = 1,
    "Aggregate GDP Growth" = 0.3
  )) +
  labs(
    title = "US vs EU – Aggregate vs Per Capita GDP Growth",
    subtitle = "Solid lines = Per Capita | Faded dashed lines = Aggregate",
    x = "Year",
    y = "Growth Rate (%)",
    linetype = "",
    alpha = ""
  ) +
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
    panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"))

ggsave(here(paste0("outputs/1_1_gpt_comparison_allgrowths.png")))



## averages table ----------------------------------------
wb_gdp_growth_comparison %>%
  group_by(CountryCode) %>%
  summarise(mean_gdp_growth = mean(gdp_growth, na.rm = TRUE))

wb_adjusting_growth %>%
  group_by(CountryCode) %>%
  summarise(mean_percap_growth = mean(gdp_percap_growth, na.rm = TRUE))

wb_adjusted_growth_table <- wb_adjusting_growth %>%
  group_by(CountryCode) %>%
  summarise(
    mean_gdp_growth = mean(gdp_growth, na.rm = TRUE),
    mean_percap_growth = mean(gdp_percap_growth, na.rm = TRUE))