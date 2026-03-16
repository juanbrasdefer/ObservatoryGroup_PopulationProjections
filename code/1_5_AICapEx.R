# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/1_5_AICapEx.R")

# BEA NIPA table data -----------------------------------------------
bea_contribution_pctchng_gdp_raw <- read_csv(here("data/BEA_NIPA/BEA_NIPA_Table_1_5_2_Contributions_to_Percent_Change_in_Real_GDP.csv"))


# 1. Reshape full table to long format
bea_contribution_long <- bea_contribution_pctchng_gdp_raw %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "quarter",
    values_to = "value"
  ) %>%
  mutate(quarter = recode(quarter,
                          "Q1_2023" = "2023-Q1",
                          "Q2_2023" = "2023-Q2",
                          "Q3_2023" = "2023-Q3",
                          "Q4_2023" = "2023-Q4",
                          "Q1_2024" = "2024-Q1",
                          "Q2_2024" = "2024-Q2",
                          "Q3_2024" = "2024-Q3",
                          "Q4_2024" = "2024-Q4",
                          "Q1_2025" = "2025-Q1",
                          "Q2_2025" = "2025-Q2",
                          "Q3_2025" = "2025-Q3",
                          "Q4_2025" = "2025-Q4"))


# 2. Extract GDP total growth (annualized %)
bea_gdp <- bea_contribution_long %>%
  filter(str_detect(item, "Gross domestic product")) %>%
  select(quarter, gdp_annual = value)


# 3. Extract AI-related contribution lines
# Adjust string matches if wording differs
ai_contrib <- bea_contribution_long %>%
  filter(str_detect(item, "Information processing equipment") |
           str_detect(item, "Software")) %>%
  group_by(quarter) %>%
  summarise(ai_contribution = sum(value, na.rm = TRUE),
            .groups = "drop")


# 4. Merge GDP and AI contributions
bea_gdp_full <- bea_gdp %>%
  left_join(ai_contrib, by = "quarter") %>%
  mutate(ai_contribution = replace_na(ai_contribution, 0))


# 5. Construct counterfactual annualized growth
# (subtract AI contribution from total)
bea_gdp_full <- bea_gdp_full %>%
  mutate(
    gdp_noai_annual = gdp_annual - ai_contribution
  )


# 6. Convert annualized rates to true quarterly rates
bea_gdp_full <- bea_gdp_full %>%
  mutate(
    growth_actual_q = (1 + gdp_annual/100)^(1/4) - 1,
    growth_noai_q     = (1 + gdp_noai_annual/100)^(1/4) - 1
  )

# 7. Construct level indices (start at 100)
bea_gdp_full <- bea_gdp_full %>%
  arrange(quarter) %>%
  mutate(
    gdp_actual_index = 100 * cumprod(1 + growth_actual_q),
    gdp_noai_index     = 100 * cumprod(1 + growth_noai_q)
  )

# Result: 
# gdp_actual_index  = observed GDP path
# gdp_noai_index    = no-AI-investment path

# plotting ---------------------------------

# Reshape to long format for plotting
gdp_full_long <- bea_gdp_full %>%
  select(quarter, 
         gdp_actual_index, 
         gdp_noai_index) %>%
  pivot_longer(
    cols = c(gdp_actual_index, gdp_noai_index),
    names_to = "series",
    values_to = "index"
  ) 

# Plot
gdp_full_long %>%
  ggplot(aes(x = quarter, y = index, group = series, color = series)) +
  geom_line(linewidth = 0.8) +
  labs(x = NULL,
       y = "GDP Index (Start = 100)",
       title = "US - Actual GDP vs. GDP without AI Investment"
       ) +
  scale_color_manual(values = c(gdp_actual_index = "darkgreen", 
                                gdp_noai_index = "orange")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
    panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
    text = element_text(family="Times New Roman"))

ggsave(here("outputs/1_5_NoAICapEx.png"))
