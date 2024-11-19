rm(list = ls())

source("src/repeat_visit_analysis.R")

# load packages
library(dplyr)
library(rpart) # fitting classification tress
library(ggplot2) # quality graphics
library(pROC) # for creating ROC curves
library(randomForest) # for fitting random forests
library(RColorBrewer)

# Random Forest with all raw x's 

# Y variable - repeat visits at the family level
    # Y - they repeat 
    # N - they don't repeat ]
visit_count_rf_data$is_multi_visitor <- factor(visit_count_rf_data$is_multi_visitor,
                                           levels = c("Y", "N")) 

# covert prop_snap to factor 
visit_count_rf_data$prop_snap <- factor(visit_count_rf_data$prop_snap, 
                                        levels = c("Y", "N"))

visit_count_rf_data <- visit_count_rf_data %>%
  select(-c(poverty_level_c, location_count, median_income)) # location_count is the number of unique times someone has visited a pantry

# omit N/A's in distance traveled and DOB
visit_count_rf_data <- na.omit(visit_count_rf_data)
  


RNGkind(sample.kind = 'default')
set.seed(172172172)
train.idx <- sample(x = 1:nrow(visit_count_rf_data), size = .7*nrow(visit_count_rf_data))
train.df <- visit_count_rf_data[train.idx, ]
test.df <- visit_count_rf_data[-train.idx, ] 

# Baseline Forest 
myforest <- randomForest(is_multi_visitor ~. ,  
                         data = train.df, 
                         ntree = 1000, #number of trees in forest, B = 1000
                         mtry = 7) # sqrt(47) = 6.855655
myforest
# OOB error rate = 1.63%

# tune forest 

# double loop and avg over 5 iterations, group by and summarize to get avg oob
mtry <- c(1:47) #think: what is even possible here? 100? no...
n_reps <- 5 # how many times do you want to fit each forest? for averaging
#make room for m, OOB error
keeps2 <- data.frame(m = rep(NA,length(mtry)*n_reps),#NOTE DIFFERENCE
                     OOB_err_rate = rep(NA, length(mtry)*n_reps))#NOTE DIFFERENCE
j = 0 #initialize row to fill#NOTE DIFFERENCE
for (rep in 1:n_reps){#NOTE DIFFERENCE
  print(paste0("Repetition = ", rep))#NOTE DIFFERENCE
  for (idx in 1:length(mtry)){
    j = j + 1 #increment row to fill over double loop#NOTE DIFFERENCE
    tempforest<- randomForest(is_multi_visitor ~ .,
                              data = train.df, 
                              ntree = 1000, #fix B at 1000!
                              mtry = mtry[idx]) #mtry is varying
    #record iteration's m value in j'th row
    keeps2[j , "m"] <- mtry[idx]#NOTE DIFFERENCE
    #record oob error in j'th row
    keeps2[j ,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$is_multi_visitor)#NOTE DIFFERENCE
  }
}




#calculate mean for each m value
keeps2 <- keeps2 %>%
  group_by(m) %>%
  summarise(mean_oob = mean(OOB_err_rate))

ggplot(data = keeps2) +
  geom_line(aes(x=m, y=mean_oob)) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB error rate") +
  scale_x_continuous(breaks = c(1:47))
# m = 9

# Final Forest
final_forest<- randomForest(is_multi_visitor ~ .,
                            data = train.df, 
                            ntree = 1000, 
                            mtry = 9, 
                            importance = TRUE)
final_forest
# OOB error rate =  1.62%
save(final_forest, file ="results/final_forest_repeat_visits.R") 

# --- Prediction --- 

# ROC Curve
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Y"] #positive event - they do repeat

rocCurve <- roc(response = test.df$is_multi_visitor,
                predictor = pi_hat,
                levels = c("N", "Y")) # negative event, positive event
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# set pi* = 0.021, can achieve specificity of 0.891 and sensitivity 0.625
# we would predict 'N' 89.1% of the time when the family doesn't actually repeat visit
# we would predict 'Y' 62.5% of the time when the family actually does repeat visit

# AUC = 0.815

pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
# create column of predicted repeat visit classification 
test.df$forest_pred <- ifelse(pi_hat > pi_star, "Yes", "No")

# --- interpretation/descriptive ---
vi <- as.data.frame(varImpPlot(final_forest, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi)+
  geom_bar(aes(x = reorder(Variable, MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy), 
           position = 'identity')+
  coord_flip()+
  labs( x = 'Variable Name', y = 'Importance')

# --- logistic regression ---
visit_count_rf_data$multi_visitor_binary <- ifelse(visit_count_rf_data$is_multi_visitor == "Y", 1, 0)

m1 <- glm(multi_visitor_binary ~ income_per_person + fam_size, 
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m1)
AIC(m1) # = 10962.81

# not using poverty level moving forward
m2 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability,
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m2)
# AIC = 10965

# tried this, but made AIC worse
visit_count_rf_data$income_per_person_squared <- visit_count_rf_data$income_per_person*visit_count_rf_data$income_per_person

m3 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male, 
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m3)
# AIC = 10960


m4 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs, 
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m4)
# AIC = 10931

m5 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs + prop_grade_school + prop_white, 
           data = visit_count_rf_data, family = binomial(link = logit))
summary(m5)
# AIC = 10907

m6 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec,
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m6)
# --- income is positive again ---- 
# AIC = 10896

m7 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
            prop_stable_income + prop_unemployed,
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m7)
# AIC = 10857

# snap categorical variable -- easier to interpret
visit_count_rf_data$snap_household_cat <- ifelse(visit_count_rf_data$is_on_snap > 0.5, "Y", "N") # if they had snap or not 

m8 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
            prop_stable_income + prop_unemployed + snap_household_cat + served_year,
          data = visit_count_rf_data, family = binomial(link = logit))

summary(m8)
# AIC = 10845

m9 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
            prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
            prop_stable_income + prop_unemployed + snap_household_cat + served_year + prop_asian,
          data = visit_count_rf_data, family = binomial(link = logit))
summary(m9)
# AIC = 10829

m10 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
             prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
             prop_stable_income + prop_unemployed + snap_household_cat + served_year + prop_asian + 
             kid_household,
           data = visit_count_rf_data, family = binomial(link = logit))
summary(m10)
# AIC = 10824
visit_count_rf_data$std_poverty_level <- visit_count_rf_data$poverty_level - mean(visit_count_rf_data$poverty_level)

# swapped out income_per_person for poverty_level since they measure the same thing and for 
# interpretations we want to explain things from a poverty standpoint
m11 <- glm(multi_visitor_binary ~ std_poverty_level + fam_size + prop_disability + prop_male + 
             prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
             prop_stable_income + prop_unemployed + snap_household_cat + served_year + prop_asian + 
             kid_household + prop_part_time + prop_other_ed + snap_household_cat*std_poverty_level,
           data = visit_count_rf_data, family = binomial(link = logit))
summary(m11)
# AIC = 10815


m12 <- glm(multi_visitor_binary ~ income_per_person + fam_size + prop_disability + prop_male + 
             prop_other_income + prop_hs + prop_grade_school + prop_white + prop_retired + prop_social_sec + 
             prop_stable_income + prop_unemployed + snap_household_cat + served_year + prop_asian + 
             kid_household + prop_part_time + prop_other_ed + prop_stay_at_home + prop_higher_ed,
           data = visit_count_rf_data, family = binomial(link = logit))

summary(m12)
# AIC = 10809 --> AIC didn't change when I added two more variables, so let's go with model m11




