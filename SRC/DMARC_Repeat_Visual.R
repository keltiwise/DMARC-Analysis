rm(list = ls())

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(lubridate)

visit_count_data <- read.csv("clean_data/multi_visit_rf_w_distance.csv", stringsAsFactors = TRUE)

visit_count_data$multi_visit <- ifelse(visit_count_data$visit_count > 1, "Y", "N")

visit_count_data$mon_year <- paste0(visit_count_data$served_year, "-", visit_count_data$served_month)
visit_count_data$mon_year <- ym(visit_count_data$mon_year)


original_afns <- visit_count_data %>%
  filter(served_year == 2022) %>%
  reframe(afn = unique(afn))

afn_2022 <- visit_count_data %>%
  filter(afn %in% original_afns$afn) %>%
  group_by(mon_year) %>%
  summarise(count_multi_visit = sum(multi_visit == "Y"))

afn_2022 %>% ggplot()+
  geom_line(aes(x=mon_year, y=count_multi_visit))+
  labs(x="Date", y="Count of Families that Multivisited")+
  ggtitle("Count of Multivisitors That Visited in 2022 between 2022-2023")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  theme_grey()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("results/multivisit_over_time.png")

  
