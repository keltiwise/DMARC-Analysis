rm(list = ls())

library(ggplot2)
library(dplyr)
library(pROC)
library(lubridate)
library(randomForest)
library(patchwork)

source("src/helpers.R")

afn_snap <- snap_cleaning()

afn_snap$mon_year <- paste0(afn_snap$served_year, "-", afn_snap$served_month)

afn_snap$mon_year <- ym(afn_snap$mon_year)

afn_snap$graph_snap_household <- factor(as.character(afn_snap$snap_household),
                                               levels=c("Y","N"),
                                               labels=c("Family Has SNAP", "Family Doesn't Have SNAP"))

afn_snap$graph_second_half_of_month <- factor(as.character(afn_snap$second_half_of_month),
                                                     levels=c("Y","N"),
                                                     labels=c("After the 15th", "Before the 15th"))

counts <- afn_snap %>%
  filter(served_year > 2019) %>%
  group_by(mon_year, graph_snap_household, graph_second_half_of_month) %>%
  summarise(count = n())

counts <- counts %>%
  group_by(mon_year, graph_second_half_of_month) %>%
  mutate(perc= count/sum(count))

counts %>% filter((graph_snap_household == "Family Has SNAP") & (graph_second_half_of_month == "After the 15th")) %>%
  ggplot()+
  geom_line(aes(x=mon_year, y=perc))+
  labs(x="Date", y="Proportion of Families Visiting After the 15th")+
  ggtitle("Proportion of SNAP Families Visiting After the 15th Over Time")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

original_afns <- afn_snap %>%
  filter(served_year == 2019) %>%
  reframe(afn = unique(afn))

original_afn_counts <- afn_snap %>%
  filter((afn %in% original_afns$afn) & (served_year >= 2019)) %>%
  select(c(afn,mon_year,graph_snap_household,graph_second_half_of_month)) %>%
  group_by(mon_year, graph_snap_household) %>%
  summarise(perc = sum(graph_second_half_of_month == "After the 15th")/n()) %>%
  filter(mon_year != "2024-02-01")

cpi_data <- read.csv('clean_data/cpi_data.csv')
cpi_data$mon_year <- paste0(cpi_data$year, "-", cpi_data$month)
cpi_data$mon_year <- ym(cpi_data$mon_year)


snap_visits <- original_afn_counts %>% filter(graph_snap_household == "Family Has SNAP") %>%
  ggplot()+
  geom_line(aes(x=mon_year, y=perc))+
  labs(x="Date", y="Proportion of SNAP Families")+
  ggtitle("Proportion of SNAP Families Visiting After the 15th Over Time")+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y")+
  scale_color_manual(values = c("#1f78b4", "#ff7f00"))+
  theme_grey()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

non_snap_visits <- original_afn_counts %>% filter(graph_snap_household == "Family Doesn't Have SNAP") %>%
  ggplot()+
  geom_line(aes(x=mon_year, y=perc))+
  labs(x="Date", y="Proportion of Non-SNAP Families")+
  ggtitle("Proportion of Non-SNAP Families Visiting After the 15th Over Time")+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y")+
  scale_color_manual(values = c("#1f78b4", "#ff7f00"))+
  theme_grey()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

combined <- snap_visits / non_snap_visits

cpi_over_time <- cpi_data %>% ggplot()+
  geom_line(data = cpi_data, aes(x=mon_year, y=value))+
  labs(x="Date", y="Consumer Price Index (All Urban Consumers)")+
  ggtitle("Consumer Price Index from 2019 to 2024")+
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y")+
  theme_grey()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

cpi_over_time + combined + plot_layout(ncol=2)

ggsave("results/snap_family_traffic.png", width = 12)
