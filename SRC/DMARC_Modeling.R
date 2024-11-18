# Logistic Models

# Aggregated on family level for multiple visits(Jordan & Kelsey): afn_visit_count

# Aggregated on family level for which half of month: afn_visit_half

# Aggregate family level for which half of month
afn_visit_half <- dmarc_clean_imputed %>% # of times they visited on 
  filter(served_year %in% c(2022,2023)) %>%
  group_by(afn, served_month, served_year) %>%
  summarise(visit_count = n_distinct(served_date),
            median_income = median(annual_income),  # median income
            any_child = any(age < 18), # true if anyone is under 18, in the family
            kid_household = kids_in_household[1],
            senior_household = seniors_in_household[1],
            old_peeps = all(age > 65), # true if age > 65, everyone in the household is old
            fam_size = family_size[1],
            income_per_person = annual_income[1]/fam_size,
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
            prop_other_gender = sum(gender == "Other")/length(gender),
            first_half = any(days_since_snap <= 15), # true if went before 15 days 
            stamp_assigned = stamp_assigned[1],
            snap_household = snap_household[1]
  )%>%
  filter((afn != "")) %>% ungroup()

table(afn_visit_half$first_half)

# Creating predictor variable for logistic regression
afn_visit_half$first_half = as.integer(afn_visit_half$first_half == "TRUE")
# 0 = second half, 1 = first half
table(afn_visit_half$first_half)

# Removing afn from data set
visit_half_rf_data <- afn_visit_half %>%
  select(-c(afn, visit_count))

visit_half_rf_data = na.omit(visit_half_rf_data)

write.csv(visit_half_rf_data, "Case Studies/Personal/half_visit_rf.csv", row.names=FALSE)

# ---------

set.seed(1234)

# Backward regression to shrink the model
full.model = glm(first_half ~ ., data = visit_half_rf_data)

summary(full.model)
# AIC: 83150

# performing backwards regression to find best model of predicting first half of month
back.model = step(full.model, direction = "backward")

# backward regression
set.seed(2)

glm.backward.model = glm(first_half ~ served_month + served_year + kid_household + senior_household + 
                           old_peeps + fam_size + min_dist + dist + prop_white + prop_asian + 
                           prop_black + prop_multi_race + location_count + prop_hispanic + 
                           prop_hs + prop_grade_school + prop_own + prop_no_housing + 
                           prop_other_housing + prop_stable_housing + prop_mobile_home + 
                           prop_part_time + prop_unemployed + prop_retired + prop_male + 
                           prop_female + stamp_assigned + snap_household,
                         data = visit_half_rf_data,
                         family = binomial("logit"))

summary(glm.backward.model)
# AIC: 79250

glm.backward.model.2 = glm(first_half ~ served_month + served_year + kid_household + 
                           old_peeps + fam_size + 
                           prop_black + prop_multi_race + location_count + prop_hispanic + 
                           prop_hs + prop_grade_school + prop_own + prop_unemployed + prop_retired + prop_male + 
                           prop_female + stamp_assigned + snap_household,
                         data = visit_half_rf_data,
                         family = binomial("logit"))

summary(glm.backward.model.2)
# AIC: 79281

glm.backward.model.3 = glm(first_half ~ served_month + served_year + kid_household + 
                             old_peeps + fam_size + 
                             prop_black + prop_multi_race + location_count + prop_hispanic + 
                             prop_hs + prop_grade_school + prop_own + prop_unemployed + prop_male + 
                             prop_female + stamp_assigned + snap_household,
                           data = visit_half_rf_data,
                           family = binomial("logit"))

summary(glm.backward.model.3)
# AIC: 79281

# original model had the lowest AIC so far

glm.backward.model.4 = glm(first_half ~ served_month + served_year + kid_household + 
                             old_peeps + fam_size + location_count + prop_own+ stamp_assigned + snap_household,
                           data = visit_half_rf_data,
                           family = binomial("logit"))

summary(glm.backward.model.4)
# AIC: 79319

glm.final.model = glm(first_half ~ served_month + served_year + kid_household + senior_household + 
                           old_peeps + fam_size + min_dist + dist + prop_white + prop_asian + 
                           prop_black + prop_multi_race + location_count + prop_hispanic + 
                           prop_hs + prop_grade_school + prop_own + prop_no_housing + 
                           prop_other_housing + prop_stable_housing + prop_mobile_home + 
                           prop_part_time + prop_unemployed + prop_retired + prop_male + 
                           prop_female + stamp_assigned + snap_household,
                         data = visit_half_rf_data,
                         family = binomial("logit"))

summary(glm.final.model)
# AIC: 79250

# interpretations
exp(coef(glm.final.model))

coef(glm.final.model)

# intercept : -177.84261567 
# fam_size : 0.16445751 
# snap_householdY : -0.11776831 
# base: no snap household

# family without snap:
# the larger the family, the probability of said family visiting the pantry in the first half 
# of the month increases (family does not have snap)

exp(-0.11776831)
# 0.888902

# the odds of a 4 person family visiting the pantry in the first half of the month
# increases by 14% for those without snap benefits. 

family_snap = visit_half_rf_data[53,]
View(family_snap) # white
family_nosnap = visit_half_rf_data[54, ]
View(family_nosnap) # white

predict(glm.final.model, newdata = data.frame(family_snap), type = "response")
# 0.5098434

predict(glm.final.model, newdata = data.frame(family_nosnap), type = "response")
# 0.5608886

# White family of 4 with snap is 50.98% likely to visit pantry in first half of month
# White family of 4 without snap is 56.09% likely to visit pantry in first half of month
# White family of 4 without snap is 6% more likely to visit pantry in the first half of month
# compared to a white family of 4 with snap

fam2_nosnap = visit_half_rf_data[116, ] # white family of 2 no snap
fam4_snap = visit_half_rf_data[59, ] # white family of 2 snap

predict(glm.final.model, newdata = data.frame(fam2_nosnap), type = "response")
# 0.591289

predict(glm.final.model, newdata = data.frame(fam4_snap), type = "response")
# 0.5364767 

# White family of 2 with snap 53.65% likely to visit pantry in first half of month
# White family of 2 without snap 59.13% likely to visit pantry in first half of month
# White family of 2 without snap is 6% more likely to visit pantry in first half of month 
# compared to a white family of 2 with snap

# finding african american family of 3 with and without snap
fam = which(visit_half_rf_data$prop_black == 1, visit_half_rf_data$fam_size == 3, visit_half_rf_data$prop_snap == 1)
fam2 = visit_half_rf_data[fam, ]
black_fam_nosnap = visit_half_rf_data[1167, ]
black_fam_snap = fam2[1, ]
View(black_fam_nosnap) # black family of 3 no snap
View(black_fam_snap) # black family of 3 snap

predict(glm.final.model, newdata = data.frame(black_fam_nosnap), type = "response")
# 0.6471861

predict(glm.final.model, newdata = data.frame(black_fam_snap), type = "response")
# 0.5252959 

# Black family of 3 with snap 52.53% likely to visit pantry first half of month
# Black family of 3 without snap 64.72% likely to visit pantry first half of month
# Black family of 3 without snap are 12% more likely to visit pantry in first half
# of month compared to black family of 3 with snap

# Finding African american family of 4
fam1 = which(visit_half_rf_data$prop_black == 1, visit_half_rf_data$fam_size == 4, visit_half_rf_data$prop_snap == 1)
fam3 = visit_half_rf_data[fam1, ]
View(fam3) # 27, 28
black_fam_4_snap = fam3[28, ]
black_fam_4_nosnap = fam3[27, ]

predict(glm.final.model, newdata = data.frame(black_fam_4_nosnap), type = "response")
# 0.6033309 

predict(glm.final.model, newdata = data.frame(black_fam_4_snap), type = "response")
# 0.583508

# Black family of 4 with snap 58.35% likely to visit pantry first half of month
# Black family of 4 without snap 60.33% likely to visit pantry first half of month
# Black family of 4 without snap are 2% more likely to visit pantry in first half
# of month compared to black family of 4 with snap
