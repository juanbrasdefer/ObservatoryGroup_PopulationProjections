# libraries ------------------------------------------------------------



### US Population Adjustment --------------------------------------------------

us_workingage <- read_csv(here("data/UN_data/UN Population Data US Working Age.csv"))
# this step is the same as doing group_by(location, year)
# which is just USA and then by year
# the 'data' argument extracts the age ranges that are classified as 'working age'
# which does not mean the ages 67, 75, 57 etc
# but rather these are the numeric codes for age ranges like 57 = 25-29
us_workingage <- aggregate(Value ~ Location + year, 
                           data = us_workingage[us_workingage$AgeId %in% c(67, 75, 57, 62, 63, 64, 69, 66, 68, 60), ],
                           FUN = sum)

# adds new column with adjusted population numbers, using adjustment rates
# calculates difference between original UN data and adjusted, as percentage
# (which is just the same rate we adjusted by)
adjust_working_pop <- function(data, year_col = "year", pop_col = "Value") {
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




# Graphing ---------------------------------------------------------------------

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




# US CENSUS ----------------------------------------------------------------------
uscensusprojection <- ggplot(us_census_adjustment, aes(x = year, y = Value, color = Projection)) +
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
