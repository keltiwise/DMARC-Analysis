rm(list = ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)


dmarc_clean <- read.csv("clean_data/dmarc_locations_imputed.csv")
dmarc_clean <- dmarc_clean %>% filter(afn != "")

model_data <- read.csv("clean_data/multi_visit_rf_w_distance.csv")

model_data$any_child <- as.numeric(model_data$any_child)
model_data$old_peeps <- as.numeric(model_data$old_peeps)
model_data$prop_snap <- ifelse(model_data$prop_snap == "Y", 1,0)
model_data <- model_data %>% filter(!is.na(total_distance_travelled))


repeat_X <- model_data %>% select(-c(is_multi_visitor, poverty_level_c, afn, served_month, 
                                     served_year, location_count, visit_count, total_distance_travelled))
repeat_stand <- apply(repeat_X, 2, function(x){ (x-mean(x))/sd(x)})

#k-means clustering

set.seed(123123)
wss <- (nrow(repeat_stand)-1)*sum(apply(repeat_stand,2,var)) #wss for k = 1 (total ss of data)

for (i in 2:45) {
  wss[i] <- sum(kmeans(repeat_stand, centers=i)$withinss)
}


#Elbow method uses a "scree plot":

scree <- ggplot() + geom_line(aes(1:45, wss)) +
  labs(x = "Number of Clusters", y = "Within Cluster Sums of Squares (WSS)")+
  ggtitle("Scree Plot Used to Find Optimal Number of Clusters (Elbow Method)")
scree
ggsave(scree, file="results/kmeans_scree_plot.png")


set.seed(123123)
repeat_kmeans <- kmeans(repeat_stand, 15)
save(repeat_kmeans, file="results/repeat_visit_kmeans_w_dist.RData")

repeat_X$km_cluster <- as.factor(repeat_kmeans$cluster)

repeat_long <- melt(repeat_X, id.vars = c("km_cluster"))

repeat_long %>% filter(km_cluster == 9) %>% ggplot() +
  geom_boxplot(aes(x = km_cluster, y = value, fill = km_cluster)) +
  facet_wrap(~variable, scales = "free") +
  ggtitle("K-means Clusters")

model_data$is_multi_visitor_bin <- ifelse(model_data$is_multi_visitor == "Y", 1, 0)
model_data$km_cluster <- as.factor(repeat_kmeans$cluster)

kmeans_logit <- glm(data = model_data, is_multi_visitor_bin ~ km_cluster, family = binomial("logit"))
summary(kmeans_logit)

predictor.df <- data.frame(km_cluster=as.factor(c(1:15)), is_multivisitor_bin = 1)

predictor.df$multivisit_prob <- predict(kmeans_logit,predictor.df, type="response")

afn_cluster <- model_data %>%
  select(afn,km_cluster) %>%
  filter(km_cluster == 9)

time_series <- merge(dmarc_clean, afn_cluster, by="afn")


time_series <- time_series %>%
  mutate(served_date = ymd(served_date)) %>%
  group_by(afn, month_year=floor_date(served_date, "month")) %>%
  ungroup()

time_series_grouping <- time_series %>%
  group_by(month_year) %>%
  summarise(count=n()) %>%
  filter(month_year != "2024-02-01")


cluster_9 <- time_series_grouping %>% ggplot()+
  geom_line(aes(x=month_year,y=count))+
  geom_vline(aes(xintercept = as.Date("2022-10-01"), color="Rule\nChange"))+
  labs(x="Month", 
       y="Count",
       title="Frequency of Pantry Visits Over Time")+
  scale_color_manual(name = "", values = c("Rule\nChange" = "red"))
ggsave(cluster_9, file="results/time_series_cluster_9_visits.png")
