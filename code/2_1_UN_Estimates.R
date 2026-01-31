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


# Import UN Data -------------------------------------------------------------

## India -------------------------------------------------------------------
india_median <- read_csv(here("data/UN_data/UN Population Data India.csv")) %>%
  rename(year = "Time")
india_nomigration <- read_csv(here("data/UN_data/UN Population Data India No Migration.csv")) %>%
  rename(year = "Time")
india_median$Projection <- "Median"
india_nomigration$Projection <- "No Migration"
india_bound <- rbind(india_median, india_nomigration) # joining both, LONG FORMAT!!


## China and Japan ----------------------------------------------------------
china_median <- read_csv(here("data/UN_data/UN Population Data China.csv")) %>%
  rename(year = "Time")
china_nomigration <- read_csv(here("data/UN_data/UN Population Data China No Migration.csv")) %>%
  rename(year = "Time")
china_median$Projection <- "Median"
china_nomigration$Projection <- "No Migration"
china_bound <- rbind(china_median, china_nomigration) # joining both, LONG FORMAT!!

japan_median <- read_csv(here("data/UN_data/UN Population Data Japan.csv")) %>%
  rename(year = "Time")
japan_nomigration <- read_csv(here("data/UN_data/UN Population Data Japan No Migration.csv")) %>%
  rename(year = "Time")
japan_median$Projection <- "Median"
japan_nomigration$Projection <- "No Migration"
japan_bound <- rbind(japan_median, japan_nomigration)


## EU ---------------------------------------------------------
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
  rename(year = "Time")
fgis_nomigration <- read_csv(here("data/UN_data/UN Population Data FGIS No Migration.csv")) %>%
  rename(year = "Time")
fgis_bound <- rbind(fgis_median, fgis_nomigration)


## US ----------------------------------------------------------
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
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme(panel.background = element_rect(fill = 'white', color = 'white'), 
        panel.grid.major = element_line(color = '#EBEBEB', linetype = 'solid', linewidth = 0.2),
        panel.grid.minor = element_line(color = '#EBEBEB', linewidth = 0.2),
        legend.position = "bottom",
        text = element_text(family="Times New Roman"))

ggsave(here(paste0("outputs/UN_projections_FGIS.png")),
       width = 9,height = 5,   # 2:1 ratio
       units = "in", dpi = 300)


