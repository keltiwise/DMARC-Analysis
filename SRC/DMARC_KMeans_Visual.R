rm(list = ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)


dmarc_clean <- read.csv("clean_data/dmarc_base_clean_imputed.csv")
dmarc_clean <- dmarc_clean %>% filter(afn != "")


model_data <- read.csv("clean_data/multi_visit_rf.csv")

model_data$any_child <- as.numeric(model_data$any_child)
model_data$old_peeps <- as.numeric(model_data$old_peeps)


repeat_X <- model_data %>% select(-c(is_multi_visitor, poverty_level_c, afn, visit_count))
repeat_stand <- apply(repeat_X, 2, function(x){ (x-mean(x))/sd(x)})

load("results/repeat_visit_kmeans.RData")

model_data$km_cluster <- as.factor(repeat_kmeans$cluster)

model_data$is_multi_visitor_bin <- ifelse(model_data$is_multi_visitor == "Y", 1, 0)

kmeans_logit <- glm(data = model_data, is_multi_visitor_bin ~ km_cluster, family = binomial("logit"))
summary(kmeans_logit)

afn_cluster <- model_data %>%
  select(afn,km_cluster)

time_series <- merge(dmarc_clean, afn_cluster, by="afn")


time_series <- time_series %>%
  mutate(served_date = ymd(served_date)) %>%
  group_by(afn, month_year=floor_date(served_date, "month")) %>%
  ungroup()

time_series_grouping <- time_series %>%
  group_by(km_cluster, month_year) %>%
  summarise(count=n()) %>%
  filter(month_year != "2024-02-01")


time_series_grouping %>% ggplot()+
  geom_line(aes(x=month_year,y=count))+
  geom_vline(aes(xintercept = as.Date("2022-10-01"), color="Rule\nChange"))+
  facet_wrap(~km_cluster)+
  labs(x="Month", 
       y="Count",
       title="Frequency of Pantry Visits for Each Cluster Group Over Time")+
  scale_color_manual(name = "", values = c("Rule\nChange" = "red"))
