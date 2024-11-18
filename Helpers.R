
import_clean_data <- function() {
  library(lubridate)
  library(dplyr)
  library(tidyverse)
  
  dmarc <- read.csv("Case Studies/Personal/dmarc_web_scrape_with_locations_and_affiliates.csv")
  
  dmarc_clean <- dmarc %>%
    mutate(served_date = ymd(served_date),
           served_year = year(served_date),
           served_month = month(served_date),
           served_day_of_week = wday(served_date, label=TRUE),
           dob_new = ymd(dob),
           individual_id = as.character(individual_id),
           age = ceiling(interval(dob, served_date) / years(1)))
  
  # remove row with missing afn
  dmarc_clean <- dmarc_clean %>%
    filter(afn != "" & !(zip %in% c("", "50301")))
  
  # gender
  dmarc_clean$gender[dmarc_clean$gender == "Man (boy)"] <- "Male"
  dmarc_clean$gender[dmarc_clean$gender == "Woman (girl)"] <- "Female"
  dmarc_clean$gender[dmarc_clean$gender %in% c("Transgender", "Non-binary/Non-conforming",
                                               "Prefer Not to Respond", "", "Not Selected")] <- "Other"
  
  # race
  dmarc_clean$race[dmarc_clean$race %in% c("Unknown", "", "Not Selected", "Other")] <- "Other"
  dmarc_clean$race[dmarc_clean$race %in% c("Native Hawaiin/Pacific Islander", 
                                           "American Indian/Alaskan Native")] <- "Pacific Islander"
  
  # ethnicity
  dmarc_clean$ethnicity[dmarc_clean$ethnicity %in% c("", "Not Selected", "Unknown")] <- "Other"
  
  # education 
  dmarc_clean$education[dmarc_clean$education %in% c("", "No Schooling", "Not Selected", "Unknown")] <- "Other"
  dmarc_clean$education[dmarc_clean$education %in% c("HS Grad / Some College", "HSED / GED Grad (or current)",
                                                     "Low Grade Grad")] <- "HS Grad"
  
  dmarc_clean$education[dmarc_clean$education == "College 2 or 4 yr  Degree"] <- "College Degree"
  dmarc_clean$education[dmarc_clean$education == "College Advanced Degree"] <- "Masters/Doctorate"
  dmarc_clean$education[dmarc_clean$education == "Pre-K and younger (Currently)"] <- "Pre-k"  # this is pre-k and younger
  dmarc_clean$education[dmarc_clean$education == "K-8 (Currently)"] <- "K-8"
  dmarc_clean$education[dmarc_clean$education == "9-12 (Currently)"] <- "9-12" 
  
  # homeless
  dmarc_clean$homeless[dmarc_clean$homeless %in% c("", "Not Selected")] <- "Unknown"
  
  # income source
  dmarc_clean$income_source[dmarc_clean$income_source %in% c("", "Not Selected")] <- "Other"
  
  # housing
  dmarc_clean$housing[dmarc_clean$housing %in% c("", "Not Selected")] <- "Other"
  
  # housing type
  dmarc_clean$housing_type[dmarc_clean$housing_type %in% c("", "Not Selected")] <- "Other"
  
  # location
  dmarc_clean$location[dmarc_clean$location %in% c("", "all")] <- "Other"
  
  ## --- order of SNAP --- ##
  
  dmarc_clean <- dmarc_clean %>%
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
  
  dmarc_clean$lnm = toupper(dmarc_clean$lnm)
  
  # stamp assigned date (ymd) column 
  dmarc_clean$stamp_date = as.Date(with(dmarc_clean, paste(dmarc_clean$served_year, 
                                                           dmarc_clean$served_month, 
                                                           dmarc_clean$stamp_assigned,
                                                           sep = "-")), "%Y-%m-%d")
  
  
  
  stamp_to_service = data.frame(service_date = dmarc_clean$served_date, 
                                stamp_dates = dmarc_clean$stamp_date, 
                                stamp_order = dmarc_clean$stamp_assigned,
                                year_served = dmarc_clean$served_year)
  
  
  dmarc_clean = dmarc_clean %>%
    add_column(days_between_visit_and_stamp = as.numeric(
      difftime(dmarc_clean$stamp_date, dmarc_clean$served_date, units = c("days"))
    ))
  
  
  
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
  
  # -------- date cleaning ------------
  
  dmarc_clean = dmarc_clean %>%
    mutate(last_issue_1 = make_date(served_year, served_month, stamp_assigned),
           last_issue_2 = last_issue_1 %m-% months(1))
  
  dmarc_clean = dmarc_clean %>%
    mutate(last_issued = case_when(served_date >= last_issue_1 ~ last_issue_1,
                                   served_date < last_issue_1 ~ last_issue_2)) %>%
    mutate(days_since_snap = as.double(difftime(served_date, last_issued, units = "days")))
  
  dmarc_clean$zip_new <- substr(dmarc_clean$zip, 1, 5)
  
  family_size <- dmarc_clean %>%
    group_by(afn, served_date) %>%
    summarise(family_size = n_distinct(individual_id))
  
  dmarc_clean <- merge(dmarc_clean, family_size, by=c("afn", "served_date"))
  
  return(dmarc_clean)
}



impute_age_income <- function(dmarc_clean) {
  
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
  
  # GLM using the Gamma distribution with log link function
  m3 <- glm(age ~ annual_income + fed_poverty_level + gender + race + ethnicity + education +
              housing + housing_type + homeless + income_source + family_size, 
            family = Gamma(link = "log"), data = linear_vars1)
  summary(m3)
  # AIC = 1252268
  
  # Predict Age -- use model m3 to impute missing or negative age values
  dmarc_clean$age[(is.na(dmarc_clean$age)) | (dmarc_clean$age < 0) | (dmarc_clean$age > 100)] <- ceiling(predict(m3, data = bad_ages))
  summary(dmarc_clean$age)
  
  
  # GLM using Gamma Distribution with log link
  m5 <- glm(data = linear_vars1, annual_income ~ age + gender + race + ethnicity + education + factor(zip_new)+
              housing + homeless + income_source + family_size, family = Gamma(link = "log"))
  AIC(m5) # AIC= 3676298
  
  # Predict Annual Income
  dmarc_clean$annual_income[(is.na(dmarc_clean$annual_income) | 
                               (dmarc_clean$annual_income < 0) |
                               (dmarc_clean$annual_income >= 250000))] <- predict(m5, type = "response",
                                                                                  data = dmarc_clean[(is.na(dmarc_clean$annual_income)) | 
                                                                                                       dmarc_clean[dmarc_clean$annual_income < 0], ])
  
  household_characteristics <- dmarc_clean %>%
    group_by(afn, served_date) %>%
    summarise(kids_in_household = length(unique(individual_id[age < 18])),
              seniors_in_household = length(unique(individual_id[age > 65]))) %>%
    ungroup()
  
  dmarc_clean <- merge(dmarc_clean, household_characteristics, by=c("afn", "served_date"))
  
  
  return(dmarc_clean)
}
