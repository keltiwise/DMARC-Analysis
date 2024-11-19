library(glmnet)

lasso_visit_count_data <- visit_count_rf_data %>%
  mutate(multi_visit_bin = ifelse(is_multi_visitor == "Y", 1, 0)) %>%
  select(-c(is_multi_visitor, poverty_level_c, location_count))

str(lasso_visit_count_data)

RNGkind(sample.kind = "default")
set.seed(23591)
train.idx <- sample(x = 1:nrow(lasso_visit_count_data), size = floor(.7*nrow(lasso_visit_count_data)))
train.df <- lasso_visit_count_data[train.idx,]
test.df <- lasso_visit_count_data[-train.idx,]

lr_mle <- glm(multi_visit_bin ~ .,
               data = train.df,
               family = binomial(link = logit))

coef(lr_mle)
lr_mle %>% coef %>% exp

x <- model.matrix(multi_visit_bin ~ ., data = train.df)[,-1] # model matrix of predictor variables

# create a vector of 0/1 for y variable
y <- as.vector(train.df$multi_visit_bin) # vector of response variable

#  -- fit Lasso Regression -- 
lr_lasso <- glmnet(x=x, y=y, family = binomial(link = logit), alpha = 1)
summary(lr_lasso)
# glmnet() fit 79 distinct logistic regressions

# choose lambda value -- minimize out of sample error
lr_lasso_cv = cv.glmnet(x,y, family = binomial(link = "logit"), alpha = 1)

plot(lr_lasso_cv) # shows out of sample error for all lambda
  # first vertical line shows the lambda that minimizes out of sample error

lr_lasso_coefs<- coef(lr_lasso_cv, s="lambda.min") %>% as.matrix()
lr_lasso_coefs

# --- predictions --- 
x.test <- model.matrix(multi_visit_bin ~ ., data = test.df)[,-1]

test.df <- test.df %>%
  mutate(mle_pred = predict(lr_mle, test.df, type = "response"),
         lasso_pred = predict(lr_lasso_cv, x.test, s = "lambda.min", type = "response")[,1])

# visualize relationship between two types of predictions
test.df %>%
  ggplot() +
  geom_point(aes(x = mle_pred, y = lasso_pred)) +
  geom_abline(aes(intercept = 0, slope = 1))

# seem pretty correlated, which is strange??

# (1) quantify out of sample prediction performance and
# (2) compare the two models in terms of their out of sample prediction performance

mle_rocCurve   <- roc(response = as.factor(test.df$multi_visit_bin),#supply truth
                      predictor = test.df$mle_pred,#supply predicted PROBABILITIES
                      levels = c("0", "1")) #(negative, positive)

plot(mle_rocCurve, print.thres = TRUE, print.auc = TRUE) # AUC = 0.689

lasso_rocCurve   <- roc(response = as.factor(test.df$multi_visit_bin),#supply truth
                        predictor = test.df$lasso_pred,#supply predicted PROBABILITIES
                        levels = c("0", "1")) #(negative, positive)

plot(lasso_rocCurve, print.thres = TRUE,print.auc = TRUE) # AUC = 0.689


ggplot() +
  geom_line(aes(x = 1-mle_rocCurve$specificities, y = mle_rocCurve$sensitivities), colour = "darkorange1", alpha = 0.8) +
  geom_text(aes(x = .75, y = .75, 
                label = paste0("MLE AUC = ",round(mle_rocCurve$auc, 3))), colour = "darkorange1")+
  geom_line(aes(x = 1-lasso_rocCurve$specificities, y = lasso_rocCurve$sensitivities), colour = "cornflowerblue", alpha = 0.5)+
  geom_text(aes(x = .75, y = .65, 
                label = paste0("Lasso AUC = ",round(lasso_rocCurve$auc, 3))), colour = "cornflowerblue") +
  labs(x = "1-Specificity", y = "Sensitivity")

# they basically have the same AUC of 0.689, but the lasso_pred has a higher specificity and sensitivity




