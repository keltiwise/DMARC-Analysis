
rm(list = ls())

# -------------- packages --------------
library(ggplot2)
library(randomForest)
library(lubridate)
library(dplyr)
library(pROC)

source("src/Helpers.R") # sourcing cleaning code

afn_snap <- snap_cleaning()

afn_snap$stamp_assigned <- as.integer(gsub("(st|nd|rd|th)", "", afn_snap$stamp_assigned))

afn_snap <- na.omit(afn_snap)


# Random Forest predicting whether people come the 1st half or second half of the month
RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx <- sample(x = 1:nrow(afn_snap), size = floor(0.7*nrow(afn_snap)))
train.df <- afn_snap[train.idx, ]
test.df <- afn_snap[-train.idx,]

str(train.df)

# For Final Forest
# importance = TRUE,
# na.action = na.pass)

# Fitting a random forest
myforest <- randomForest(second_half_of_month ~ .,
                         data = train.df,
                         ntree = 1000, 
                         mtry = 7)

myforest


nreps <- 10
mtry <- seq(1,45, by = 5) # possibilities for m
keeps <- data.frame(m = rep(NA,length(mtry)*nreps), 
                    OOB_err_rate = rep(NA, length(mtry)*nreps))
j <- 0
for (rep in 1:nreps){
  print(paste0("Repetition = ", rep))
  for (idx in 1:length(mtry)){
    j <- j + 1
    print(paste0("mtry = ", mtry[idx]))
    tempforest<- randomForest(second_half_of_month ~ .,
                              data = train.df, 
                              ntree = 1000, #fix B at 1000!
                              mtry = mtry[idx]) #mtry is varying
    #record iteration's m value in idx'th row
    keeps[j , "m"] <- mtry[idx] 
    #record oob error in idx'th row
    keeps[j ,"OOB_err_rate"] <- mean(predict(tempforest)!= train.df$second_half_of_month) 
    
  }
}

keeps[which.min(keeps$OOB_err_rate),"m"]

#calculate mean for each m value
keeps2 <- keeps %>%
  group_by(m) %>%
  summarise(mean_oob = mean(OOB_err_rate))

keeps2[which.min(keeps2$mean_oob),"m"]

# Final Forest
final_forest <- randomForest(second_half_of_month ~ .,
                             data = train.df, 
                             ntree = 1000, 
                             mtry = keeps2$m[which.min(keeps2$mean_oob)], 
                             importance = TRUE)
final_forest

save(final_forest, file="results/snap_rf.RData")

pi_hat <- predict(final_forest, test.df, type = "prob")[,"Y"] #positive event - they go in the second half of the month

rocCurve <- roc(response = test.df$second_half_of_month,
                predictor = pi_hat,
                levels = c("N", "Y")) # negative event, positive event

png("results/roc_curve_plot.png")
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
dev.off()

# pi_star = 0.713
# specificity = 0.687
# sensitivty = 0.592

# We would predict "Y" 68.7% of the time when it actually was "Y"
# We would predict "N" 59.2% of the time when it actually was "N"

# AUC = 0.709

vi <- as.data.frame(varImpPlot(final_forest, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi)+
  geom_bar(aes(x = reorder(Variable, MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy), 
           position = 'identity')+
  coord_flip()+
  labs( x = 'Variable Name', y = 'Importance', title='Variable Importance Plot for SNAP Model')
ggsave("results/snap_rf_importance.png")

