# Logistic Regression

library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyverse)

source("/Users/keltiwise/Case Studies/Jordan/SRC/Helpers.R") # look at helpers file, has all of the cleaning stuff


dmarc_clean <- import_clean_data()
dmarc_clean_imputed <- impute_age_income(dmarc_clean)

write.csv(dmarc_clean, "Case Studies/Personal/dmarc_locations_imputed.csv", row.names=FALSE)


summary(dmarc_clean_imputed$annual_income)
summary(dmarc_clean_imputed$age)


# (1) what demographic traits corresponds with a higher number of visits
# by served_month - frequency of people repeating monthly

# & (count > 1) # visit pantry/pantries > 1 times a month

# plot of number of repeats, box plot of income
# using geom_violin or boxplot

ggplot(data = monthly_visit_count) +
  geom_boxplot(aes(x = as.factor(count), y = median_income)) +
  labs(x = "Count", y = "Income") +
  ggtitle("Income of Repeat Pantry Goers") +
  theme_bw() 
# 1 and 2 visit counts look very similar - also only 1 afn that has repeated 3 times in a month, which is odd

# compare 1's vs 2's -- look at different demographics, etc

# goal is to see what kind of traits are correlated with 2,3,4 visits per month

ggplot(data = monthly_visit_count) +
  geom_boxplot(aes(x = poverty_level_c, y = median_income)) +
  labs(x = "Count", y = "Distance") +
  ggtitle("Distance Travelled of Repeat Pantry Goers") +
  theme_bw() + scale_y_log10()

ggplot(data = monthly_visit_count) +
  geom_boxplot(aes(x = poverty_level_c, y = median_income)) +
  labs(x = "Count", y = "Distance") +
  ggtitle("fix later") +
  theme_bw() + scale_y_log10()
# add color eventually


# scatter plot of y = dist, x = poverty level
ggplot(data = monthly_visit_count) +
  geom_point(aes(y = median_income, x = poverty_level), alpha = I(.01))
# add facet wrap eventually for dist, narrow it down

# group pov level into groups of 10, and then make same plot with box plot


# -------- monthly repeat visits ------------

# aggregate family level
afn_visit_count <- dmarc_clean_imputed %>% # of times they visited on 
  filter(served_year %in% c(2022,2023)) %>%
  group_by(afn, served_month, served_year) %>%
  summarise(visit_count = n_distinct(served_date),
            median_income = median(annual_income),  # median income
            any_child = any(age < 18), # true if anyone is under 18, in the family
            kid_household = kids_in_household[1],
            senior_household = seniors_in_household[1],
            old_peeps = all(age > 65), # true if age > 65, everyone in the household is old
            fam_size = family_size[1],
            med_dist = median(distance_travelled_miles), # median, max, min distance traveled
            max_dist = max(distance_travelled_miles), 
            min_dist = min(distance_travelled_miles), 
            dist = mean(distance_travelled_miles),
            prop_snap = sum(snap_household == "Y")/length(snap_household), # proportion of time they have had snap
            prop_white = sum(race == "White")/length(race),
            prop_asian = sum(race == "Asian")/length(race),
            prop_black = sum(race == "Black/African American")/length(race),
            prop_multi_race = sum(race == "Multi-Race")/length(race),
            prop_pac = sum(race == "Pacific Islander")/length(race),
            prop_other_race = sum(race == "Other")/length(race),
            location_count = n_distinct(location), # count of different locations - count of location names
            prop_hispanic = sum(ethnicity == "Hispanic or Latino")/length(ethnicity),
            prop_not_hispanic = sum(ethnicity %in% c("Not Hispanic or Latino", "Other"))/length(ethnicity), 
            poverty_level = mean(fed_poverty_level),
            prop_hs = sum(education == "HS Grad")/length(education),
            prop_higher_ed = sum(education %in% c("College Degree", "Masters/Doctorate"))/length(education),
            prop_grade_school = sum(education %in% c("9-12", "Pre-k", "K-8"))/ length(education),
            prop_drop_out = sum(education == "K-12 Drop Out") / length(education),
            prop_other_ed = sum(education == "Other") / length(education),
            prop_own = sum(housing == "Own/Buying") / length(housing),
            prop_rent = sum(housing == "Renting") / length(housing),
            prop_no_housing = sum(housing == "Homeless") / length(housing),
            prop_other_housing = sum(housing == "Other") / length(housing),
            prop_stable_housing = sum(homeless == "Stably Housed")/length(homeless),
            prop_unstable_housing = sum(homeless == "Unstably Housed") / length(homeless),
            prop_homeless = sum(homeless %in% c("Imminently Homeless", "Literally Homeless", "Unkown"))/ length(homeless),
            prop_close_living = sum(housing_type %in% c("Five or More Unit Apartments",
                                                        "Duplex/Triplex/Quadplex"))/length(housing_type),
            prop_house = sum(housing_type == "House")/length(housing_type),
            prop_mobile_home = sum(housing_type == "Mobile Home")/length(housing_type),
            prop_single_room = sum(housing_type == "Rent a room") / length(housing_type),
            prop_other_housing = sum(housing_type == "Other") / length(housing_type),
            prop_stable_income = sum(income_source == "Full Time")/length(income_source),
            prop_part_time = sum(income_source %in% c("Seasonal/Temporary","Part Time", 
                                                      "Student/ Workforce Training")) / length(income_source),
            prop_unemployed = sum(income_source == "Unemployed") / length(income_source),
            prop_other_income = sum(income_source %in% c("Child Support", "Unemployment Benefits", "Other",
                                                         "FIP (Family Investment Program"))/ length(income_source),
            prop_social_sec = sum(income_source == "Social Security") / length(income_source),
            prop_disability = sum(income_source %in% c("Disability", "Pending Disability"))/length(income_source),
            prop_retired = sum(income_source == "Retired")/length(income_source),
            prop_stay_at_home = sum(income_source == "Stay at Home Parent")/length(income_source),
            prop_male = sum(gender == "Male") / length(gender),
            prop_female = sum(gender == "Female") / length(gender),
            prop_other_gender = sum(gender == "Other")/length(gender)
  )%>%
  filter((afn != "")) %>% ungroup() %>%
  mutate(poverty_level_c = cut(poverty_level, breaks = c(-Inf, seq(0, 200, by = 10), 300, 400, Inf)))


afn_visit_count$is_multi_visitor <- ifelse(afn_visit_count$visit_count > 1, "Y", "N")

visit_count_rf_data <- afn_visit_count %>%
  select(-c(afn, visit_count))

write.csv(visit_count_rf_data, "Case Studies/Personal/multi_visit_rf.csv", row.names=FALSE)

write.csv(afn_visit_count, "clean_data/multi_visit_rf_w_distance.csv", row.names=FALSE)
