
rm(list = ls())

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)

source("src/helpers.R")

dmarc_clean <- read.csv("clean_data/dmarc_locations_imputed.csv")



# count of visits for each afn by month and year
afn_visit_count <- dmarc_clean %>% # of times they visited on
  filter(served_year %in% c(2022, 2023)) %>%
  group_by(afn, served_month, served_year) %>%
  summarise(visit_count = n_distinct(served_date),
            any_child = any(age < 18), # true if anyone is under 18, in the family
            kid_household = kids_in_household[1],
            senior_household = seniors_in_household[1],
            old_peeps = all(age > 65), # true if age > 65, everyone in the household is old
            fam_size = family_size[1],
            income_per_person = annual_income[1]/fam_size,
            total_distance_travelled = sum(distance_travelled_miles),
            prop_snap = snap_household[1], 
            prop_white = (sum(race == "White")/length(race))*100,
            prop_asian = (sum(race == "Asian")/length(race))*100,
            prop_black = (sum(race == "Black/African American")/length(race))*100,
            prop_multi_race = (sum(race == "Multi-Race")/length(race))*100,
            prop_pac = (sum(race == "Pacific Islander")/length(race))*100,
            prop_other_race = (sum(race == "Other")/length(race))*100,
            location_count = n_distinct(location), # count of different locations - count of location names
            prop_hispanic = (sum(ethnicity == "Hispanic or Latino")/length(ethnicity))*100,
            prop_not_hispanic = (sum(ethnicity %in% c("Not Hispanic or Latino", "Other"))/length(ethnicity))*100, 
            poverty_level = mean(fed_poverty_level),
            prop_hs = (sum(education == "HS Grad")/length(education))*100,
            prop_higher_ed = (sum(education %in% c("College Degree", "Masters/Doctorate"))/length(education))*100,
            prop_grade_school = (sum(education %in% c("9-12", "Pre-k", "K-8"))/ length(education))*100,
            prop_drop_out = (sum(education == "K-12 Drop Out") / length(education))*100,
            prop_other_ed = (sum(education == "Other") / length(education))*100,
            prop_own = (sum(housing == "Own/Buying") / length(housing))*100,
            prop_rent = (sum(housing == "Renting") / length(housing))*100,
            prop_no_housing = (sum(housing == "Homeless") / length(housing))*100,
            prop_other_housing = (sum(housing == "Other") / length(housing))*100,
            prop_stable_housing = (sum(homeless == "Stably Housed")/length(homeless))*100,
            prop_unstable_housing = (sum(homeless == "Unstably Housed") / length(homeless))*100,
            prop_homeless = (sum(homeless %in% c("Imminently Homeless", "Literally Homeless", "Unkown"))/ length(homeless))*100,
            prop_close_living = (sum(housing_type %in% c("Five or More Unit Apartments",
                                                         "Duplex/Triplex/Quadplex"))/length(housing_type))*100,
            prop_house = (sum(housing_type == "House")/length(housing_type))*100,
            prop_mobile_home = (sum(housing_type == "Mobile Home")/length(housing_type))*100,
            prop_single_room = (sum(housing_type == "Rent a room") / length(housing_type))*100,
            prop_other_housing = (sum(housing_type == "Other") / length(housing_type))*100,
            prop_stable_income = (sum(income_source == "Full Time")/length(income_source))*100,
            prop_part_time = (sum(income_source %in% c("Seasonal/Temporary","Part Time", 
                                                       "Student/ Workforce Training")) / length(income_source))*100,
            prop_unemployed = (sum(income_source == "Unemployed") / length(income_source))*100,
            prop_other_income = (sum(income_source %in% c("Child Support", "Unemployment Benefits", "Other",
                                                          "FIP (Family Investment Program"))/ length(income_source))*100,
            prop_social_sec = (sum(income_source == "Social Security") / length(income_source))*100,
            prop_disability = (sum(income_source %in% c("Disability", "Pending Disability"))/length(income_source))*100,
            prop_retired = (sum(income_source == "Retired")/length(income_source))*100,
            prop_stay_at_home = (sum(income_source == "Stay at Home Parent")/length(income_source))*100,
            prop_male = (sum(gender == "Male") / length(gender))*100,
            prop_female = (sum(gender == "Female") / length(gender))*100,
            prop_other_gender = (sum(gender == "Other")/length(gender))*100
            
  )%>%
  filter((afn != "")) %>% ungroup() %>%
  mutate(poverty_level_c = cut(poverty_level, breaks = c(-Inf, seq(0, 200, by = 10), 300, 400, Inf)))


afn_visit_count$prop_snap <- factor(afn_visit_count$prop_snap, levels = c("Y", "N"))
afn_visit_count$is_multi_visitor <- ifelse(afn_visit_count$visit_count > 1, "Y", "N")
afn_visit_count$is_multi_visitor <- factor(afn_visit_count$is_multi_visitor, levels = c("Y", "N"))


write.csv(afn_visit_count, "clean_data/multi_visit_rf_w_distance.csv", row.names=FALSE)
