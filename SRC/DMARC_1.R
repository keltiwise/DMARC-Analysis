
# ----------- loading dataset -------------------
dmarc_clean = read.csv("/Users/keltiwise/Downloads/Case Studies Data Analytics/drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv")
dmarc_clean <- subset(dmarc, select = -c(family_type)) # REMOVE family_type, not helpful or needed

# -------------- installing packages --------------
# install.packages('lubridate')
library(lubridate)
# install.packages("tidyverse")
library(tidyverse)
# install.packages('dplyr')
library(dplyr)
# install.packages('ggplot2')
library(ggplot2)
# install.packages("stringr")
library(stringr)
# install.packages("plotly")
library(plotly)
# install.packages("ggiraph")
library(ggiraph)

# checking characters
str(dmarc_clean)

# service_name      : chr  "Food Pantry" "Food Pantry" "Food Pantry 20" "Food Pantry 20" ...
# afn               : chr  "IM-346306" "IM-419743" "IM-415756" "IM-415756" ...
# served_date       : Date, format: "2018-01-02" "2018-01-02" "2018-01-02" "2018-01-02" ...
# annual_income     : num  35600 740 27000 27000 20000 0 0 710 0 0 ...
# fed_poverty_level : int  175 7 133 133 124 0 0 3 0 0 ...
# individual_id     : chr  "560284" "584744" "547960" "547958" ...
# lnm               : chr  "W" "E" "M" "R" ...
# dob               : chr  "1946-04-10" "1989-04-14" "1973-10-08" "1964-01-14" ...
# gender            : chr  "Man (boy)" "Man (boy)" "Woman (girl)" "Man (boy)" ...
# race              : chr  "White" "White" "Asian" "Asian" ...
# ethnicity         : chr  "Not Hispanic or Latino" "Not Hispanic or Latino" "Not Hispanic or Latino" "Not Hispanic or Latino" ...
# education         : chr  "HS Grad" "HS Grad" "College 2 or 4 yr  Degree" "HS Grad" ...
# family_type       : chr  "Adults with Children" "Single Person" "Adults with Children" "Adults with Children" ...
# snap_household    : chr  "Y" "Y" "Y" "Y" ...
# zip               : chr  "50317" "50317" "50266" "50266" ...
# location          : chr  "Polk County River Place" "Polk County North Side" "WDM HUMAN SERVICES" "WDM HUMAN SERVICES" ...
# housing           : chr  "Own/Buying" "Renting" "Renting" "Renting" ...
# housing_type      : chr  "House" "Five or More Unit Apartments" "Five or More Unit Apartments" "Five or More Unit Apartments" ...
# homeless          : chr  "Stably Housed" "Stably Housed" "Stably Housed" "Stably Housed" ...
# income_source     : chr  "Social Security" "Disability" "Unemployed" "Full Time" ...
# served_year       : num  2018 2018 2018 2018 2018 ...
# served_month      : num  1 1 1 1 1 1 1 1 1 1 ...
# served_day_of_week: Ord.factor w/ 7 levels "Sun"<"Mon"<"Tue"<..: 3 3 3 3 3 3 3 3 3 3 ...
# dob_new           : Date, format: "1946-04-10" "1989-04-14" "1973-10-08" "1964-01-14" ...
# age               : num  72 29 45 54 56 39 37 39 21 57 ...


# -----------------------------------------------------------------------------------------------------------


# ------------------ cleaning -----------------------------
# using lubridate to change the dates from character types to date data types
dmarc_clean <- dmarc_clean %>%
  mutate(served_date = ymd(served_date),
         served_year = year(served_date),
         served_month = month(served_date),
         served_day_of_week = wday(served_date, label=TRUE),
         dob_new = ymd(dob),
         individual_id = as.character(individual_id),
         age = ceiling(interval(dob, served_date) / years(1)))

# there were some issues with parsing date of birth, so I subsetted the data to 
# only show rows
# where date of birth was null
bad_ages <- dmarc_clean[dmarc_clean$age < 0 | is.na(dmarc_clean$dob_new), ] 
# looks like a lot of these rows were filled incompletely
# These seem very difficult to impute because a lot of the data is missing for that specific row

dmarc_good_ages <- dmarc_clean[dmarc_clean$age >= 0 & !is.na(dmarc_clean$dob_new),] 
# !is.na(linear_vars$dob_new - ensures dob_new has no missing values

# grouping dmarc_good_ages by afn
afn <- dmarc_good_ages %>%
  group_by(afn) %>%
  summarise(count = n()) # calculates the count of observations for each group
# dob column and count, easier to see what age groups we are working with

# -------------------------- more data cleaning ------------------------------

# ----- Gender -----
table(dmarc_clean$gender)
#               Man (boy)  Non-binary/Non-conforming    Not Selected     Other 
#       18              430084                259             105           71 
# Prefer Not to Respond       Transgender             Woman (girl) 
#           48                    634                    532331 

# I think lets go 3 categories?
# (1) Male
# (2) Female
# (3) Other -- transgender, non-binary/non-conforming, other, Prefer Not to Respond, Not Selected, ""

dmarc_clean$gender[dmarc_clean$gender == "Man (boy)"] <- "Male"
dmarc_clean$gender[dmarc_clean$gender == "Woman (girl)"] <- "Female"
dmarc_clean$gender[dmarc_clean$gender %in% c("Transgender", "Non-binary/Non-conforming",
                                             "Prefer Not to Respond", "", "Not Selected")] <- "Other"

table(dmarc_clean$gender)
# Female         Male      Other 
#   532331       430084     1135

# the "Other" level isn't very big at all...

# ------ Race --------
table(dmarc_clean$race)

#                    American Indian/Alaskan Native   Asian       Black/African American      
#          18                   7931                   94661           152169 
# Multi-Race        Native Hawaiin/Pacific Islander     Not Selected          Other 
#       18780                  2456                       88               41868 
# Unknown           White 
#   8653             636926

# I'm thinking 6 categories??
# (1) White 
# (2) Black/African American
# (3) Asian
# (4) Other -- Unknown, "", Not Selected, Other 
# (5) Multi-Race
# (6) Pacific Islander - Native Hawaiin/Pacific Islander, American Indian/Alaskan Native

# race seems to have a decent amount of individuals at each level given the way 
# the categories have been made/rearranged

dmarc_clean$race[dmarc_clean$race %in% c("Unknown", "", "Not Selected", "Other")] <- "Other"
dmarc_clean$race[dmarc_clean$race %in% c("Native Hawaiin/Pacific Islander", 
                                         "American Indian/Alaskan Native")] <- "Pacific Islander"

#   Asian   Black/African American  Multi-Race  Other   Pacific Islander   White
#   94661          152169            18780      50627         10387         636926 


# ----- Ethnicity -------
table(dmarc_clean$ethnicity)

#               Hispanic or Latino   Not Hispanic or Latino     Not Selected 
#    18             126555                   836661               30 
# Other                Unknown 
#   88                    198 

# regardless of grouping these into possibly 3 levels, the test/train split
# will be an issue
# I'm thinking we just do 2 levels: "Hispanic or Latino" and "Not Hispanic or Latino"
# but at the same time, it feels wrong 


# ----- Education ---------
table(dmarc_clean$education)

# (1) Other -- "", No Schooling, Not Selected, Unknown
# (2) HS Grad -- HS Grad, HS Grad / Some College, HSED / GED Grad (or current), Low Grade Grade
dmarc_clean$education[dmarc_clean$education %in% c("", "No Schooling", "Not Selected", "Unknown")] <- "Other"
dmarc_clean$education[dmarc_clean$education %in% c("HS Grad / Some College", "HSED / GED Grad (or current)",
                                                   "Low Grade Grad")] <- "HS Grad"

dmarc_clean$education[dmarc_clean$education == "College 2 or 4 yr  Degree"] <- "College Degree"
dmarc_clean$education[dmarc_clean$education == "College Advanced Degree"] <- "Masters/Doctorate"
dmarc_clean$education[dmarc_clean$education == "Pre-K and younger (Currently)"] <- "Pre-k" 
# this is pre-k and younger
dmarc_clean$education[dmarc_clean$education == "K-8 (Currently)"] <- "K-8"
dmarc_clean$education[dmarc_clean$education == "9-12 (Currently)"] <- "9-12" 

# 9-12      College Degree      HS Grad     K-12 Drop Out      K-8    Masters/Doctorate 
# 24125          81081            636319        152097         4895         8061 
#     Other            Pre-k 
#     55872            1100 

# ----- homeless -------
table(dmarc_clean$homeless)

dmarc_clean$homeless[dmarc_clean$homeless %in% c("", "Not Selected")] <- "Unknown"

# Imminently Homeless   Literally Homeless    Stably Housed     Unknown     Unstably Housed 
#  4738                     18624              910061             1054            29073


# changing dates
dmarc_clean <- dmarc_clean %>%
  mutate(served_date = ymd(served_date),
         served_year = year(served_date),
         served_month = month(served_date),
         served_day_of_week = wday(served_date, label=TRUE),
         dob = ymd(dob),
         age = ceiling(interval(dob, now()) / years(1)))

# there were some issues with parsing date of birth, so I subsetted the data to only show rows
# where date of birth was null
null_dob <- dmarc_clean[is.na(dmarc_clean$dob), ] # looks like a lot of these rows were filled incompletely
# These seem very difficult to impute because a lot of the data is missing for that specific row

length(unique(dmarc_clean$individual_id))

# adding column into data set containing order in which food stamps were recieved
# (based on last name first initial)

# https://www.joinproviders.com/state/iowa/payment-schedule/ website used to pull
# information about time stamp was assigned
# order is in days, 1st = first day of month, 2nd = second day of month, etc...

dmarc_clean$lnm = toupper(dmarc_clean$lnm)

dmarc_clean = dmarc_clean %>%
  add_column(stamp_assigned = case_when(str_detect(dmarc_clean$lnm, "A|B") ~ "1",
                                        str_detect(dmarc_clean$lnm, "C|D") ~ "2",
                                        str_detect(dmarc_clean$lnm, "E|F|G") ~ "3",
                                        str_detect(dmarc_clean$lnm, "H|I") ~ "4",
                                        str_detect(dmarc_clean$lnm, "J|K|L") ~ "5",
                                        str_detect(dmarc_clean$lnm, "M|N|O") ~ "6",
                                        str_detect(dmarc_clean$lnm, "P|Q|R") ~ "7",
                                        str_detect(dmarc_clean$lnm, "S") ~ "8",
                                        str_detect(dmarc_clean$lnm, "T|U|V") ~ "9",
                                        str_detect(dmarc_clean$lnm, "W|X|Y|Z") ~ "10"))




# stamp assigned date (ymd) column 
dmarc_clean$stamp_date = as.Date(with(dmarc_clean, paste(dmarc_clean$served_year, 
                                                         dmarc_clean$served_month, 
                                                         dmarc_clean$stamp_assigned,
                                                         sep = "-")), "%Y-%m-%d")

summary(dmarc_clean$stamp_date)

# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2018-01-01" "2019-07-01" "2021-03-02" "2021-02-26" "2022-12-01" "2024-02-10"       "2832" 

# ---------------------------------------------------------------------------------------------------------

# ---------- visualizations ---------

dmarc_clean %>% ggplot() +
  geom_histogram(aes(x = annual_income))
# histogram shows right skewness for annual income
# also in the 5-num summary the max value is $1,000,000?? Is that right?
# Should we even be looking at those values?

# 5-number summary for annual income:
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#   0    7680   16800   19896   29000   1000000 

# resource used for household income value: https://www.census.gov/quickfacts/fact/table/IA,US/INC910222
below_median_household_income <- dmarc_clean[dmarc_clean$annual_income < 70571,]

median_vals <- below_median_household_income %>%
  group_by(served_year) %>%
  summarize(median = median(annual_income))

# This graph shows a histogram for all people that went to the pantry and
# had less than the median household income as per the US Census for Iowa
# the red line marks the median annual income for that year.
below_median_household_income %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median), color = "red") +
  facet_wrap(~served_year) +
  theme_bw()


# bar chart of what day of the week the most people visit pantries
dmarc_clean %>% ggplot() +
  geom_bar(aes(x=served_day_of_week)) +
  ggtitle("Number of People Who Went to the Pantry by the Day of the Week and Year") +
  labs(x = "Day of the Week", y = "People (Count)")+
  facet_wrap(~served_year) +
  theme_bw()

dmarc_clean %>% ggplot() +
  geom_bar(aes(x=service_name)) +
  ggtitle("Number of People Who Went to the Pantry by the Day of the Week and Year") +
  labs(x = "Type of Pantry", y = "People (Count)")+
  facet_wrap(~served_year) +
  coord_flip() +
  theme_bw() 

# histogram of age 
dmarc_clean %>% ggplot() +
  geom_histogram(aes(x=age))

# there are some bad numbers here... lets subset and see what's going on
summary(dmarc_clean$age)
#   Min.     1st Qu.   Median     Mean    3rd Qu.     Max.     NA's 
# -7897.00    15.00    35.00    36.36    56.00      1010.00     78 

bad_ages <- dmarc_clean[(dmarc_clean$age < 0) | (is.na(dmarc_clean$age)),]

dmarc_clean %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median), color = "red") +
  facet_wrap(~served_year) +
  theme_bw()

median_vals <- dmarc_clean %>%
  group_by(served_year) %>%
  summarize(median = median(annual_income))

dmarc_clean %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median(dmarc$annual_income)), color = "red") +
  theme_bw()

# histogram shows right skewness for annual income
# also in the 5-num summary the max value is $1,000,000?? Is that right?
# Should we even be looking at those values?

# 5-number summary for annual income:
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#   0    7680   16800   19896   29000   1000000 

# resource used for household income value: https://www.census.gov/quickfacts/fact/table/IA,US/INC910222
below_median_household_income <- dmarc_clean[dmarc_clean$annual_income < 70571,]

# below median household income grouped by served 
median_vals <- below_median_household_income %>%
  group_by(served_year) %>%
  summarize(median = median(annual_income))

# This graph shows a histogram for all people that went to the pantry and
# had less than the median household income as per the US Census for Iowa
# the red line marks the median annual income for that year.
below_median_household_income %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median), color = "red") +
  facet_wrap(~served_year) +
  theme_bw()


# bar chart of what day of the week the most people visit pantries
dmarc_clean %>% ggplot() +
  geom_bar(aes(x=served_day_of_week)) +
  ggtitle("Number of People Who Went to the Pantry by the Day of the Week and Year") +
  labs(x = "Day of the Week", y = "People (Count)")+
  facet_wrap(~served_year) +
  theme_bw()

dmarc_clean %>% ggplot() +
  geom_bar(aes(x=service_name)) +
  ggtitle("Number of People Who Went to the Pantry by the Day of the Week and Year") +
  labs(x = "Type of Pantry", y = "People (Count)")+
  facet_wrap(~served_year) +
  coord_flip() +
  theme_bw() 

# histogram of age 
dmarc_clean %>% ggplot() +
  geom_histogram(aes(x=age))

# there are some bad numbers here... lets subset and see what's going on
summary(dmarc_clean$age)
#   Min.     1st Qu.   Median     Mean    3rd Qu.     Max.     NA's 
# -7897.00    15.00    35.00    36.36    56.00      1010.00     78 

dmarc_clean %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median), color = "red") +
  facet_wrap(~served_year) +
  theme_bw()

dmarc_clean %>% ggplot() +
  geom_histogram(aes(x = annual_income), binwidth = 5000) +
  ggtitle("Annual Income for People Below the Median Household Income in IA") +
  labs(y = "People (count)", x = "Annual Income") +
  geom_vline(data = median_vals, aes(xintercept = median(dmarc$annual_income)), color = "red") +
  theme_bw()

# -------------------------------------------------------------------------------------------------

# --------- stamp assigned visuals --------------------

# looking for which stamp order most frequently visits the pantry

dmarc_clean %>% ggplot() +
  geom_bar(aes(x = stamp_assigned)) +
  ggtitle("Total Stamp Assigned Order") +
  labs( x = "Stamp Assigned Order", y = "Total Count of Stamps")

summary(dmarc_clean$served_year)

# ----------------------- IDEA: count for each person that came to pantry for given day,
# separated by class in which they received food stamp, unique values to each date --------------------

summary(unique(dmarc_clean$served_date))
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2018-01-02" "2019-06-10" "2020-12-04" "2020-12-29" "2022-07-14" "2024-02-06" 

# ------- creating new table ----------------
stamp_to_service = data.frame(service_date = dmarc_clean$served_date, 
                         stamp_dates = dmarc_clean$stamp_date, 
                         stamp_order = dmarc_clean$stamp_assigned,
                         year_served = dmarc_clean$served_year)
View(stamp_to_service)

# --------------------------------------------------------------------

# -------- models --------------

# substr() - extracts substrings from a character vector
dmarc_clean$zip_new <- substr(dmarc_clean$zip, 1, 5) 
# this is extracting substrings from the zip code column
# 1 - indicates starting position of the substring
# 5 - indicates ending position
head(dmarc_clean$zip_new) # this should now all be 5 digit zip codes

rich_kids <- dmarc_clean[dmarc_clean$annual_income > 100000,] # annual income over 1 mil

# groups dmarc data by afn then counts all distinct individuals related to that afn
family_size <- dmarc_clean %>%
  group_by(afn) %>%
  summarise(family_size = n_distinct(individual_id))
# figuring out who is family based off of the afn 

dmarc_new <- merge(dmarc_clean, family_size, by="afn") # merging dmarc and family size
summary(dmarc_new$family_size)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.359   3.000  15.000

# I'm going to try creating a linear regression to impute missing age values
# subset dataset to only variables we want to use during the modeling process
linear_vars <- subset(dmarc_new, select= -c(service_name, afn, served_date, individual_id, lnm, dob, zip))
linear_vars <- linear_vars[linear_vars$age >= 0 & linear_vars$age < 100 & !is.na(linear_vars$dob_new),]

# using linear regression model to see which values are most important in predicting age
# helps us narrow down which values to take into consideration when trying to impute those missing values
m1 <- lm(data = linear_vars, age ~ annual_income + fed_poverty_level + gender + race + ethnicity + 
           education + family_type +
           housing + housing_type + homeless + income_source + family_size)

summary(m1)
AIC(m1)
# r^2 = 0.51
# adjusted r^2 = 0.5099
# p-value for model = basically 0
# AIC = 7563868
# not much penalty taken off which is good and the r^2 is pretty average

m1b <- lm(data = linear_vars, age ~ annual_income + fed_poverty_level +
            housing + homeless + income_source + family_size)

summary(m1b)
AIC(m1b)
# r^2 = 0.7287
# standard error = 10.99
# AIC = 8828831

# Ordinary Linear Regression with Log link function
m2 <- glm(age ~ annual_income + fed_poverty_level + gender + race + ethnicity + education +
            housing + housing_type + homeless + income_source + family_size, 
          family = gaussian(link = "log"), data = linear_vars)
# AIC = 8709742

summary(m2)

# GLM using the Gamma distribution with log link function
m3 <- glm(age ~ annual_income + fed_poverty_level + gender + race + ethnicity + education +
            housing + housing_type + homeless + income_source + family_size, 
          family = Gamma(link = "log"), data = linear_vars)
summary(m3)
# AIC = 8459894


# Annual Income models
# Ordinary Linear Model with identity link
m4 <- lm(data = linear_vars, annual_income ~ age + gender + race + ethnicity + education +
           housing + homeless + income_source + family_size)

summary(m4)
AIC(m4)
#R^2 = 0.18
# AIC = 25296911
# Standard error = 13550

# GLM using Gamma Distribution with log link function
m5 <- glm(data = linear_vars, annual_income ~ age + gender + race + ethnicity + education +
            housing + homeless + income_source + family_size, family = Gamma(link = "log"))

summary(m5)
AIC(m5)
# AIC = 25073021

# GLM using Inverse Gaussian distribution
m6 <- glm(data = linear_vars, annual_income ~ age + gender + race + ethnicity + education +
            housing + homeless + income_source + family_size, family = gaussian(link = "inverse"))

summary(m6)
AIC(m6)
# AIC = 25321666


## --- Linear Model Results -- ##

# For predicting Age, using the Gamma distribution with a log link function was the best model as per AIC

# For predicting annual income, the models weren't as good as predicting age but if we use a model
# using a gamma distribution with a log link function would be the best call in this situation

# -----------------------------------------------------------------------------------------------

# ---------- evaluating snap household -------

table(dmarc_clean$snap_household)

# N      Y 
# 773651 691755 

# ------- computing days between when stamp was given and date of pantry visit -----------

dmarc_clean = dmarc_clean %>%
  add_column(days_between_visit_and_stamp = as.numeric(
    difftime(dmarc_clean$stamp_date, dmarc_clean$served_date, units = c("days"))
  ))

summary(dmarc_clean$days_between_visit_and_stamp)

# values that have negative numbers are when an individual went to pantry before 
# stamp was assigned for that month - meaning they used stamp from last month 

# ---------- changing stamp_assigned column to a numerical value ---------------

dmarc_clean$stamp_assigned = as.numeric(dmarc_clean$stamp_assigned)

# last issue 1 column: created by combining serve year, serve month and stamp assigned day based 
# off last name. data pulled from external website about last name and stamp assigned order. last issue 
# 1 column is date stamp was assigned. 

# last issue 2: is the date of stamp issued a month before last issue 1. 

# if the served date (pantry visit) is after stamp was assigned, these people used the stamp they were given 
# for that month. ie: receiving stamp jan 4 and going to pantry jan 9
# if the served date (pantry visit) is before stamp was assigned, these people are 
# using stamp from the previous month. ie: receiving stamp jan 3 and going to pantry feb 2

# to ensure no negative numbers, had to use case_when to detect which stamp was used when at pantry, which
# then resulted in days between stamp was recieved and when they went to pantry

# -------- cleaning ------------

dmarc_clean = dmarc_clean %>%
  mutate(last_issue_1 = make_date(served_year, served_month, stamp_assigned),
         last_issue_2 = last_issue_1 %m-% months(1))

dmarc_clean = dmarc_clean %>%
  mutate(last_issued = case_when(served_date >= last_issue_1 ~ last_issue_1,
                                 served_date < last_issue_1 ~ last_issue_2)) %>%
  mutate(days_since_snap = as.double(difftime(served_date, last_issued, units = "days")))

# ------------ visualizations ----------

# histogram of only people that receive snap
dmarc_clean %>% 
  filter(snap_household == "Y") %>%
  ggplot(aes(x = days_since_snap)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  facet_wrap(~ served_year) +
  ggtitle("People Visiting Pantry by Computing Days Since Snap Was Recieved With Snap") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals") 
  
# histogram of everyone visiting pantry
dmarc_clean %>% 
  ggplot(aes(x = days_since_snap)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  ggtitle("People Visiting Pantry by Computing Days Since Snap Was Recieved With and Without Snap") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# comparing histograms by year for people that have snap
dmarc_clean %>% 
  filter(snap_household == "Y") %>%
  ggplot(aes(x = days_since_snap)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  facet_wrap(~ served_year) +
  ggtitle("People Visiting Pantry by Computing Days Since Snap Was Recieved With Snap / Year") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# comparing histogram by year for all individuals 
dmarc_clean %>% 
  ggplot(aes(x = days_since_snap)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  facet_wrap(~ served_year) + 
  ggtitle("People Visiting Pantry by Computing Days Since Snap Was Recieved With Snap and Without") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# by month
dmarc_clean %>% 
  ggplot(aes(x = days_since_snap)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  facet_wrap(~ served_month) + 
  ggtitle("People Visiting Pantry by Computing Days Since Snap Was Recieved With Snap and Without") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2018
dmarc_clean %>%
  filter(served_year == 2018) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2018: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2019
dmarc_clean %>%
  filter(served_year == 2019) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2019: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2020
dmarc_clean %>%
  filter(served_year == 2020) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2020: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2021
dmarc_clean %>%
  filter(served_year == 2021) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2021: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2022
dmarc_clean %>%
  filter(served_year == 2022) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2022: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# 2023
dmarc_clean %>%
  filter(served_year == 2023) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_histogram(colour = "black",
                 lwd = 0.35,
                 position = "dodge", 
                 binwidth = 1) + 
  ggtitle("2023: Days Between When Snap Was Recieved and Day of Pantry Visit") + 
  xlab("Days Between When Snap Was Recieved and Visited Pantry") + 
  ylab("Count of Total Individuals")

# group by snap and day - proportional plots to compare shape of plots etc...

ggplot(dmarc_clean) + 
  geom_histogram(aes(x = days_since_snap, y = after_stat(density), 
                     fill = snap_household), 
                 binwidth = 1, 
                 colour = "black") + 
  facet_wrap(~ served_year)

ggplot(dmarc_clean) + 
  geom_density(aes(x = days_since_snap, fill = snap_household), alpha = 0.2) + 
  facet_wrap(~ served_year)

ggplot(dmarc_clean, aes(x = days_since_snap)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) 

library(scales)

# with and without snap

# ------- percentage plot ------------

remotes::install_github("wilkelab/cowplot", force = TRUE)
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
remotes::install_github("clauswilke/colorblindr")
library(colorblindr)


ggplot(dmarc_clean, aes(x = days_since_snap, fill = snap_household)) + 
  geom_bar(aes(y = ..prop..), position = "dodge", color = "black") + 
  labs(y = "Percent of Total Families", x = "Days Between Snap and Pantry Visit", 
       fill = "Snap Household", title = "Comparison of Families With/Without Snap & Days Between 
Snap and Pantry Visit") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_fill_OkabeIto()

dmarc_clean %>% 
  filter(served_year == 2022 | 2023) %>%
  ggplot(aes(x = days_since_snap, fill = snap_household)) + 
  geom_bar(aes(y = ..prop..), position = "dodge", color = "black") + 
  labs(y = "Percent of Total Families", x = "Days Between Snap and Pantry Visit", 
       fill = "Snap Household", title = "Comparison of Families With/Without Snap & Days Between 
Snap and Pantry Visit") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_fill_OkabeIto()

ggplot(dmarc_clean, aes(x = days_since_snap, color = snap_household)) + 
  geom_density() + 
  labs(y = "Density", x = "Days Between Snap and Pantry Visit", 
       color = "Snap Household", title = "Comparison Y/N Snap & Days Between Snap and Pantry Visit") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  scale_fill_OkabeIto()


# ---------------- logistic regression -------------

# logistic regression model - visit level
# binary column include meaningful x variables 

dmarc_clean = dmarc_clean[!is.na(dmarc_clean$stamp_assigned),]

dmarc_clean <- subset(dmarc_clean, select = -c(afn, dob))

# splitting data
dmarc_clean = dmarc_clean %>%
  mutate(first_half = case_when(days_since_snap < 15 ~ 1,
                           days_since_snap >= 15 ~ 0))

# splitting data into test and train
set.seed(10000)
sample <- sample(c(TRUE, FALSE), nrow(dmarc_clean), replace = T, prob = c(0.8,0.2))
train <- dmarc_clean[sample, ]
test <- dmarc_clean[!sample, ]

# logistic regression model
model = glm(first_half ~  annual_income + fed_poverty_level + gender + 
              race + ethnicity + education + 
              snap_household + zip + location + 
              homeless + age, family = binomial, data = train)

str(dmarc_clean)


# -------------------------------------------------------------------------------------------------------------

# ------------ zip --------------


library(ggplot2)
library(RColorBrewer)
library(dplyr)

# ---- zip codes --- 
table(dmarc_clean$zip_new) # 623 observations with missing zipcodes -- ignore missing values for now

# bar chart with all of the zip codes and counts -- not great 
ggplot(data = dmarc_clean)+
  geom_bar(aes(x = zip_new))

table(dmarc_clean$zip_new)

# subset with all of the individuals with missing zip codes -- can we infer where they are located?
missing_zip <- dmarc_clean[is.na(dmarc_clean$zip_new) | dmarc_clean$zip_new == "",]

# homeless: 50301 --> not a zip code, NA
# actually points to a PO box
# https://www.zip-codes.com/zip-code/50301/zip-code-50301.asp 

good_zip <- dmarc_clean %>% filter(!is.na(zip_new) & zip_new != "") # filter out any of the observations with missing zip codes

# group data by zip_new then counts all the distinct zip codes
zip_size <- dmarc_clean %>%
  group_by(zip_new) %>%
  summarise(zip_size = n_distinct(afn))
# households at each zip code, grouped by distinct afn

# ------- Most Popular Zip Codes -----------

# zip codes with counts >= 12000
large_zip <- zip_size$zip_new[zip_size$count >= 12000]

# create subset of popular zip codes
pop_zip <- dmarc_clean[dmarc_clean$zip_new %in% large_zip, ]
pop_zip <- data.frame(pop_zip)

ggplot(data = pop_zip)+
  geom_bar(aes(x = zip_new))+
  coord_flip()

# take annual income, group by (zip_new) and take median annual income for each 
# zip code and look at all the people above the median
income_size <- dmarc_clean %>%
  group_by(zip_new) %>%
  summarise(zip_size = n_distinct(afn),
            median = median(annual_income))

# count of the times they went to the pantry at least twice and including the median annual income for each zip code
visit_count <- dmarc_clean %>%
  group_by(afn, served_year)%>%
  summarise(count = n(), median_income = median(annual_income)) %>%
  filter((afn != "") & (count > 2)) # afn and # of visits to the pantry > 2. 

# look at all the repeat customers with income > median, help us 
# find the people that have a car, and are able to visit several
# see out of all the people the proportion of those who are able to visit several times because 
# they have higher income or 

# filter down to snap = NO

# PEOPLE TAKING ADVANTAGE OF MULTIPLE VISITS - food bank rule change

# -- Smallest Zip Code Counts
super_small <- zip_size$zip_new[zip_size$count <= 5]
teeny_tiny <- dmarc_clean[dmarc_clean$zip_new %in% super_small, ]

ggplot(data = teeny_tiny)+
  geom_bar(aes(x = zip_new))+
  coord_flip()

# housing 

ggplot(data = dmarc_clean)+
  geom_bar(aes(x = housing, fill = housing_type))+
  theme_bw()+
  coord_flip()

# ------------------------------------------------------------------------------------------------------

# groups dmarc data by afn then counts all distinct individuals related to that afn
family_size <- dmarc_clean %>%
  group_by(afn) %>%
  summarise(family_size = n_distinct(individual_id))

dmarc_clean <- merge(dmarc_clean, family_size, by="afn")
summary(dmarc_clean$family_size)


ia <- counties(state = "Iowa", cb=TRUE)

ia_zip <- zctas(state = "ia", year = 2010)

polk_zip <- zctas(year = 2010, starts_with = "50", state = "Iowa")


ia %>% ggplot() +
  geom_sf()

# we want to identify the county that each person is in.
# once we have that we can raster the counties by the count of people that went to a pantry
# We can also raster based on median distance traveled to a pantry.


dmarc_lat_long <- dmarc_clean[!is.na(dmarc_clean$person_lat) | !is.na(dmarc_clean$person_long), ]

dmarc_lat_long <- st_as_sf(dmarc_lat_long, coords = c("person_long","person_lat"), crs = 4326)

summary(dmarc_clean$person_long)

st_crs(ia) <- st_crs(dmarc_lat_long)

result_ia <- st_join(ia, dmarc_lat_long) %>%
  group_by(NAME) %>%
  summarise(count = n_distinct(individual_id),
            median_dist = median(distance_travelled_miles))

result_ia %>% ggplot() +
  geom_sf(aes(fill=median_dist))


st_crs(ia_zip) <- st_crs(dmarc_lat_long)

result_zip <- st_join(ia_zip, dmarc_lat_long) %>%
  group_by(ZCTA5CE10) %>%
  summarise(count = n_distinct(individual_id),
            median_dist = median(distance_travelled_miles))

result_zip %>% ggplot() +
  geom_sf(aes(fill=median_dist))

st_crs(polk_zip) <- st_crs(dmarc_lat_long)

result_polk <- st_join(polk_zip, dmarc_lat_long) %>%
  group_by(ZCTA5CE10) %>%
  summarise(count = n_distinct(individual_id),
            median_dist = median(distance_travelled_miles))

result_polk %>% ggplot() +
  geom_sf(aes(fill=median_dist))

# --------------------------------------------------------------------------------------------------

linear_vars <- subset(dmarc_clean, select= -c(service_name, afn, served_date, individual_id, lnm, dob, zip))
linear_vars <- linear_vars[linear_vars$age > 0 & linear_vars$age < 100 & !is.na(linear_vars$dob_new) & !is.na(linear_vars$zip_new)
                           & linear_vars$annual_income > 0, ]

summary(linear_vars$age)
summary(linear_vars$annual_income)


##################################
## Linear models for imputation ##
##################################

#  remove outliers - use quantile function and take off top and bottom 5%
lower_quantile <- quantile(linear_vars$annual_income, 0.05) 
upper_quantile <- quantile(linear_vars$annual_income, 0.95)  

# Filter out rows where the annual income variable is outside the quantile range
linear_vars1 <- linear_vars[linear_vars$annual_income >= lower_quantile & linear_vars$annual_income <= upper_quantile, ]

# GLM using Gamma Distribution with log link
m5 <- glm(data = linear_vars1, annual_income ~ age + gender + race + ethnicity + education + factor(zip_new)+
            housing + homeless + income_source + family_size, family = Gamma(link = "log"))

summary(m5)
AIC(m5) # = 22049324


# For predicting annual income, the models weren't as good as predicting age but if we use a model
# using a gamma distribution with a log link function would be the best call in this situation

# Predict Annual Income
dmarc_clean$annual_income[is.na(dmarc_clean$annual_income) | (dmarc_clean$annual_income < 0) |(dmarc_clean$annual_income >= 250000)] <- predict(m5, type = "response",
                                                                                                                                                data = dmarc_clean[(is.na(dmarc_clean$annual_income)) | 
                                                                                                                                                                     dmarc_clean[dmarc_clean$annual_income < 0], ])

summary(dmarc_clean$annual_income)

rich_kids <- dmarc_clean[dmarc_clean$annual_income > 100000,]

# ---- zip codes --- 
table(dmarc_clean$zip_new) # 623 observations with missing zipcodes -- ignore missing values for now

# bar chart with all of the zip codes and counts -- not great 
ggplot(data = dmarc_clean)+
  geom_bar(aes(x = zip_new))

table(dmarc_clean$zip_new)

# subset with all of the individuals with missing zip codes -- can we infer where they are located?
missing_zip <- dmarc_clean[is.na(dmarc_clean$zip_new) | dmarc_clean$zip_new == "",]

# homeless: 50301 --> not a zip code, NA
# actually points to a PO box
# https://www.zip-codes.com/zip-code/50301/zip-code-50301.asp 

good_zip <- dmarc_clean %>% filter(!is.na(zip_new) & zip_new != "") # filter out any of the observations with missing zip codes

# group data by zip_new then counts all the distinct zip codes
zip_size <- dmarc_clean %>%
  group_by(zip_new) %>%
  summarise(zip_size = n_distinct(afn))
# households at each zip code, grouped by distinct afn

# ------- Most Popular Zip Codes -----------

# zip codes with counts >= 12000
large_zip <- zip_size$zip_new[zip_size$zip_size >= 12000]

# create subset of popular zip codes
pop_zip <- dmarc_clean[dmarc_clean$zip_new %in% large_zip, ]
pop_zip <- data.frame(pop_zip)

ggplot(data = pop_zip)+
  geom_bar(aes(x = zip_new))+
  coord_flip()


# take annual income, group by (zip_new) and take median annual income for each 
# zip code and look at all the people above the median
income_size <- dmarc_clean %>%
  group_by(zip_new) %>%
  summarise(zip_size = n_distinct(afn),
            median = median(annual_income))

# count of the times they went to the pantry at least twice and including the median annual income for each zip code
visit_count <- dmarc_clean %>%
  group_by(location, afn, served_year)%>%
  summarise(count = n(), 
            median_income = median(annual_income),
            distance = median(distance_travelled_miles)) %>%
  filter((afn != "") & (count > 2))# afn and # of visits to the pantry > 2. 

visit_count$percentile <- findInterval(visit_count$median_income, 
                                       quantile(visit_count$median_income, probs = seq(0, 1, 0.01), na.rm = TRUE))

top_5 <- visit_count[visit_count$percentile >= 95,]


# look at all the repeat customers with income > median, help us 
# find the people that have a car, and are able to visit several
# see out of all the people the proportion of those who are able to visit several times because 
# they have higher income or 

# filter down to snap = NO

# PEOPLE TAKING ADVANTAGE OF MULTIPLE VISITS - food bank rule change




# -- Smallest Zip Code Counts
super_small <- zip_size$zip_new[zip_size$count <= 5]
teeny_tiny <- dmarc_clean[dmarc_clean$zip_new %in% super_small, ]

ggplot(data = teeny_tiny)+
  geom_bar(aes(x = zip_new))+
  coord_flip()


# housing 

ggplot(data = dmarc_clean)+
  geom_bar(aes(x = housing, fill = housing_type))+
  theme_bw()+
  coord_flip()+
  
