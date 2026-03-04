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
         obs_value = "OBS_VALUE") %>%
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
         unit)


# Working Hours -----------------------------------------------------
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


# Labour Force Participation -----------------------------------------------------
oecd_participation <- oecd_productivity_clean %>%
  filter(measure %in% c("EMP", # number of people employed, thousands
                        "POP", # population, 
                        "HRSPOP")) 

oecd_participation_wide <- oecd_participation %>% # Labour Utilisation (hours worked per head of population)
  select(-c(unit,
            description,
            unit_multiplier)) %>%
  pivot_wider(names_from = measure, 
              values_from = obs_value) %>%
  mutate(emp_participation = EMP/POP) %>%
  filter(year < 2024) # USA no 2024



## Graphing US EU labour force participation--------------------------------

oecd_participation_wide %>%
  ggplot(aes(x = year, y = emp_participation, color = CountryCode)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "US vs EU - Labour Force Participation Rate",
    x = "Year",
    y = "Participation Rate"
  ) +
  scale_color_manual(values = c(EUU = "navyblue", USA = "darkred")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/1_3_labourforce_participation.png")))