### UN - Demographics Trends and Immigration 

# Libraries -----------------------------------------------------------------
install.packages("readstata13")
#install.packages("readxl")
install.packages("DescTools")
install.packages("ggplot2")
library(readstata13)
#library(readxl)
#library(ggplot2)


install.packages("ggthemes")
#install.packages("ggrepel")
install.packages("gridExtra")
library(ggthemes)
library(ggrepel)
library(gridExtra)


library(tidyverse)
library(here)

# set directory
here::i_am("code/graphing.R")


# Import UN Data -------------------------------------------------------------

## India -------------------------------------------------------------------
india_median <- read_csv(here("data/UN_data/UN Population Data India.csv"))
india_nomigration <- read_csv(here("data/UN_data/UN Population Data India No Migration.csv"))
india_median$Projection <- "Median"
india_nomigration$Projection <- "No Migration"
india_bound <- rbind(india_median, india_nomigration) # joining both, LONG FORMAT!!


## China and Japan ----------------------------------------------------------
china_median <- read_csv(here("data/UN_data/UN Population Data China.csv"))
china_nomigration <- read_csv(here("data/UN_data/UN Population Data China No Migration.csv"))
china_median$Projection <- "Median"
china_nomigration$Projection <- "No Migration"
china_bound <- rbind(china_median, china_nomigration) # joining both, LONG FORMAT!!

japan_median <- read_csv(here("data/UN_data/UN Population Data Japan.csv"))
japan_nomigration <- read_csv(here("data/UN_data/UN Population Data Japan No Migration.csv"))
japan_median$Projection <- "Median"
japan_nomigration$Projection <- "No Migration"
japan_bound <- rbind(japan_median, japan_nomigration)

eastasia_bound <- rbind(japan, china)


## EU ---------------------------------------------------------
# EU - High level, total population
eu_median <- read_csv(here("data/UN_data/UN Population Data EU.csv")) # data is country-level
eu_median <- aggregate(Value ~ Time, data = eu_median, FUN = sum) # so we aggregate to reach Euro level
eu_nomigration <- read_csv(here("data/UN_data/UN Population Data EU No Migration.csv"))
eu_nomigration <- aggregate(Value ~ Time, data = eu_nomigration, FUN = sum) # same aggregation step
eu_median$Projection <- "Median"
eu_nomigration$Projection <- "No Migration"
eu_bound <- rbind(eu_median, eu_nomigration)

# EU Zoom In - France, Germany, Italy, Spain
fgis_median <- read_csv(here("data/UN_data/UN Population Data FGIS.csv"))
fgis_nomigration <- read_csv(here("data/UN_data/UN Population Data FGIS No Migration.csv"))
fgis_bound <- rbind(fgis_median, fgis_nomigration)


## US ----------------------------------------------------------
# US - High level, total population
us_median <- read_csv(here("data/UN_data/UN Population Data US.csv"))
us_nomigration <- read_csv(here("data/UN_data/UN Population Data US No Migration.csv"))
us_median$Projection <- "Median"
us_nomigration$Projection <- "No Migration"
us_bound <- rbind(us_median, us_nomigration)

### US Population Adjustment --------------------------------------------------

us_workingage <- read_csv(here("data/UN_data/UN Population Data US Working Age.csv"))
# this step is the same as doing group_by(location, time)
# which is just USA and then by year
# the 'data' argument extracts the age ranges that are classified as 'working age'
# which does not mean the ages 67, 75, 57 etc
# but rather these are the numeric codes for age ranges like 57 = 25-29
us_workingage <- aggregate(Value ~ Location + Time, 
                        data = us_workingage[us_workingage$AgeId %in% c(67, 75, 57, 62, 63, 64, 69, 66, 68, 60), ],
                        FUN = sum)

# adds new column with adjusted population numbers, using adjustment rates
# calculates difference between original UN data and adjusted, as percentage
# (which is just the same rate we adjusted by)
adjust_working_pop <- function(data, year_col = "Time", pop_col = "Value") {
  years <- data[[year_col]] # pull list of all year values
  population <- data[[pop_col]] # pull list of all population values
  adj_rate <- ifelse(years < 2000, 0.03, # for pre-2000, new estimate will be 0.97 of original 
                     ifelse(years < 2020, 0.05, # for 2000-2019, new estimate 0.95 of original
                            ifelse(years <= 2023, 0.135, # for 2020-2023 and 2024->, new estimate 0.865 original
                                   0.135)))
  adjusted_pop <- population * (1 - adj_rate) # apply new estimate (reduction)
  
  data$adjustment_rate <- adj_rate # add to dataframe
  data$working_pop_adjusted <- adjusted_pop # apply to new population column
  data$difference <- population - adjusted_pop # calculate difference b/w UN data and new adjusted pop
  data$percent_difference <- (data$difference / population) * 100 # convert to percentage
  
  return(data)
}

# create new dataframe with adjusted numbers
us_workingage_adjusted <- adjust_working_pop(us_workingage)

us_workingage$Projection <- "Standard"

# then drop unnecessary columns
us_workingage_adjusted[c("Value", "adjustment_rate", "difference", "percent_difference")] <- NULL
us_workingage_adjusted$Projection <- "Census Adjustment"
names(us_workingage_adjusted)[3] <- "Value" # rename column "Projection" to "Value"
us_census_adjustment_bound <- rbind(us_workingage, us_workingage_adjusted)


# US 'Encounters' Data ----------------------------------------------------------
us_encounters <- read_csv(here("data/US_CBP_data/nationwide-encounters-fy22-fy25-state.csv"))
us_encounters <- us_encounters %>%
  rename(encounter_count = "Encounter Count",
         fiscal_year = "Fiscal Year")

# achieves save result as group_by(fiscal_year) and sum
us_encounters1 <- aggregate(encounter_count ~ fiscal_year, 
                           data = us_encounters, 
                           FUN = sum)

# Eurostat data ----------------------------------------------------------------
eurostat_baseline <- read_csv(here("data/eurostat_data/Eurostat Projections.csv"))

# renaming columns to match the naming conventions from UN data
eurostat_baseline <- eurostat_baseline %>%
  rename(Projection = "Type of projection",
         Location = "Geopolitical entity (reporting)",
         Time = "TIME_PERIOD",
         Value = "OBS_VALUE")



# Graphing -----------------------------------------------------------
graph_UNprojection <- function(df, region_name){
  df %>%
    ggplot(aes(x = Time, y = Value, color = Projection))+
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
  ggsave(here(paste0("outputs/UN_",region_name,"_combined.png")))
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

fgisprojection <- ggplot(fgis_bound, aes(x = Time, y = Value, color = Projection)) +
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

ggsave(here(paste0("outputs/UN_FGIS_combined.png")))




# US CENSUS ----------------------------------------------------------------------
uscensusprojection <- ggplot(us_census_adjustment, aes(x = Time, y = Value, color = Projection)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "US Working Population, Adjusted with Census Update", 
    x = "Year",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme_minimal() +
  theme(legend.position = "bottom")

encountersplot <- ggplot(us_encounters, aes(x = fiscal_year, y = encounter_count)) +
  geom_col() +
  geom_text(aes(label = format(encounter_count, big.mark = ",")), vjust = -0.5, size = 4) +
  labs(
    title = "US Border Encounters by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Encounters"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  theme_minimal()

eurostat_plot <- ggplot(eurostat_baseline, aes(x = Time, y = Value, color = Projection)) +
  geom_line(linewidth = 1) +
  geom_point(size = 0.5) +
  facet_wrap(~ Location, scales = "free_y") +
  labs(
    title = "Eurostat Working Population Projections: France, Germany, Italy, Spain", 
    x = "Year",
    y = "Population") +
  scale_y_continuous(labels = function(x) paste0(x/1e6, "M")) +
  theme_minimal() +
  theme(legend.position = "bottom")