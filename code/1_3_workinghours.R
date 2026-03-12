# libraries --------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/1_3_workinghours.R")

# OECD full data -----------------------------------------------
oecd_productivity_raw <- read_csv(here("data/oecd/oecd_productivitymeasures.csv"))
# includes...
# Avg Hours Worked per Person Employed
# Labour Utilisation (hours worked per head of population)
# GDP per Hour Worked
# GDP per Person Employed
# GDP
# Employment
# GDP per Capita
# Avg Hours Worked per Person Employed
# Labour Utilisation (hours worked per head of population)
# Hours Worked for Total Employment


oecd_productivity_clean <- oecd_productivity_raw %>%
  rename(CountryCode = "REF_AREA",
         ref_area_long = "Reference area",
         description = "Measure",
         measure = "MEASURE",
         unit = "Unit of measure",
         year = "TIME_PERIOD",
         unit_multiplier = "Unit multiplier",
         obs_value = "OBS_VALUE",
         price_base_description = "Price base",
         base_period = "Base period") %>%
  filter(CountryCode %in% c("USA","EU27_2020" )) %>% #EU27_2020, USA
  mutate(CountryCode = recode(CountryCode, 
                              "EU27_2020" = "EUU")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 2000) %>%
  select(CountryCode, 
         ref_area_long,
         measure,
         year,
         unit_multiplier,
         obs_value,
         description,
         unit,
         PRICE_BASE,
         price_base_description,
         base_period)
         


# 1) Working Hours -----------------------------------------------------
oecd_hoursworked <- oecd_productivity_clean %>%
  filter(measure == "HRSAV") 

## Graphing US EU working hours--------------------------------

oecd_hoursworked %>%
  ggplot(aes(x = year, y = obs_value, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - Average Hours Worked per Person",
    caption = "Includes all overtime, public holidays, annual paid leave, strikes and labour disputes",
    x = "Year",
    y = "Annual Hours Worked"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_3_hoursworked.png")))

## OECD num: GDP per Hours Worked -----------------------------------------
# GDP per Hour Worked
# in USD per hour


oecd_gdp_per_hourworked <- oecd_productivity_clean %>%
  filter(measure %in% c("GDPHRS")) %>%     # GDP per hour worked
  filter(unit == "US dollars per hour, PPP converted") %>% # so we can compare US and EU
  filter(price_base_description == "Constant prices") # we can look at 'constant' (real)
                                                      # or 'current' (inflation) prices
                                                      # we choose to keep parity



oecd_gdp_per_hourworked %>%
  ggplot(aes(x = year, y = obs_value, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - Productivity per Hour Worked ",
    subtitle = "US dollars per hour, PPP converted",
    x = "Year",
    y = "GDP/hr in USD"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_3_2_gdp_perhoursworkedOECD.png")))



# 1.1) Simple GDP plot -----------------------------------
oecd_simplegdp <- oecd_productivity_clean %>%
  filter(measure %in% c("GDP")) %>%     # GDP 
  filter(unit == "US dollars, PPP converted") %>% # so we can compare US and EU
  filter(price_base_description == "Constant prices")


oecd_simplegdp %>%
  ggplot(aes(x = year, y = obs_value, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - GDP, Millions",
    subtitle = "US dollars per hour, PPP converted",
    x = "Year",
    y = "GDP USD (M)"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_3_1_simple_gdp_OECD.png")))


# 1.2) Calculating GDP per hours worked -----------------------------------
oecd_calculating_gdp_per_hourworked <- oecd_productivity_clean %>%
  filter(measure %in% c("GDP")) %>%     # GDP 
           filter(unit == "US dollars, PPP converted") %>% # so we can compare US and EU
           filter(price_base_description == "Constant prices") %>% # we can look at 'constant' (real)
         # or 'current' (inflation) prices
# we choose 'constant' because we want to 
# compare the differences in labor input (hours worked) 
# as effect on REAL ECONOMIC OUTPUT,
# we dont want our measure polluted by inflation
  rename(real_gdp_millions = "obs_value") %>%
  left_join(oecd_hoursworked %>%
              rename(hours_worked_avg = "obs_value") %>%
              select(CountryCode,
                     year,
                     hours_worked_avg), 
            by = c("CountryCode", 
                   "year")) %>%
  mutate(hours_worked_hypothetical = 2080) %>%
  mutate(gdp_per_hrworked_baseline = real_gdp_millions/hours_worked_hypothetical,
         gdp_per_hrworked_actual = real_gdp_millions/hours_worked_avg)
         

t2_result <- oecd_calculating_gdp_per_hourworked %>%
  select(CountryCode,
         year,
         gdp_per_hrworked_baseline,
         gdp_per_hrworked_actual) %>%
  pivot_longer(-c(CountryCode,
                  year),
               names_to = "gdp_projection") %>%
  mutate(gdp_projection = recode(gdp_projection,
                          gdp_per_hrworked_baseline = "GDP/Hr Worked - Baseline",
                          gdp_per_hrworked_actual = "GDP/Hr Worked - Actual"))




## graphing hypothetical GDPs per hr worked together ---------------

t2_result %>%
  ggplot(aes(x = year,
             y = value,
             color = CountryCode,
             linetype = gdp_projection,
             linewidth = gdp_projection,
             alpha = gdp_projection)) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_color_manual(values = c(EUU = "navyblue",
                                USA = "darkred")) +
  scale_linetype_manual(values = c(
    "GDP/Hr Worked - Actual" = "solid",
    "GDP/Hr Worked - Baseline" = "dashed"
  )) +
  scale_alpha_manual(values = c(
    "GDP/Hr Worked - Actual" = 1,
    "GDP/Hr Worked - Baseline" = 0.3
  )) +
  labs(
    title = "US vs EU - GDP per Hour Worked, USD Millions PPP",
    subtitle = "Solid lines = True Hrs Worked | Faded dashed lines = Baseline",
    caption = "Baseline Scenario = 2080 annual hours (52weeks * 40hrs)",
    x = "Year",
    y = "GDP per Hour Worked (M. USD)",
    linetype = "",
    alpha = ""
  ) +
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
    panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"))

ggsave(here(paste0("outputs/1_3_gdp_per_hrworked_fullcomparison.png")))


# 2) Labour Force Participation -----------------------------------------------------
oecd_participation <- oecd_productivity_clean %>%
  filter(measure %in% c("EMP", # number of people employed, thousands
                        "POP", # population, 
                        "HRSPOP")) 

# create our own measure (simple EMP/POP)
oecd_participation_wide <- oecd_participation %>% 
  select(-c(unit,
            description,
            unit_multiplier,
            PRICE_BASE,
            price_base_description,
            base_period)) %>%
  pivot_wider(names_from = measure, 
              values_from = obs_value) %>%
  mutate(emp_participation = EMP/POP) %>%
  filter(year < 2024) # USA no 2024

oecd_participation_wide %>%
  group_by(CountryCode) %>%
  summarise(avg_participation = mean(emp_participation))


## Graphing US EU labour force participation--------------------------------

oecd_participation_wide %>%
  ggplot(aes(x = year, y = emp_participation, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - Labour Participation Rate",
    subtitle = "Percentage of Population that is Employed",
    x = "Year",
    y = "Participation Rate"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here("outputs/1_3_labour_participation.png"))




# OLD - calculating growth rate per hours worked ------------------------
# idea was:
# we turn to the dataframe we created for growth and pop adjustments
# wb_adjusting_growth
# which has the 'adjusted' gdp growth (adjusted for pop growth)
# we will take this as our running growth for US and EU
# and reduce them further according to working hours

# but:
# was mathematically not good because growth rates are
# dependant on more than just the current year
# ie: being that they are a delta and labour is an input
# we cant just do this calc and be done with it

# 
# oecd_hoursworked_multipliers <- oecd_hoursworked %>%
#   select(year, CountryCode, obs_value) %>%
#   pivot_wider(names_from = CountryCode, values_from = obs_value) %>%
#   mutate(
#     multiplier_USA = EUU / USA,
#     multiplier_EUU = EUU / EUU) 
# 
# t2_result <- oecd_hoursworked_multipliers %>%
#   select(year, starts_with("multiplier_")) %>%
#   pivot_longer(
#     cols = starts_with("multiplier_"),
#     names_to = "CountryCode",
#     names_prefix = "multiplier_",
#     values_to = "multiplier") %>%
#   left_join(t1_result %>%
#               select(c(CountryCode, 
#                        year, 
#                        gdp_percap_growth)),
#             by = c("CountryCode", "year"))
