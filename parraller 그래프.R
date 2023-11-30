setwd("~/다변량통계분석/기말프로젝트/")
library(dplyr)
library(Hotelling)
library(ggplot2)
library(Rtsne)
library(umap)
library(dplyr)
library(plotly)
library(skimr)
library(knitr)
library(kableExtra)
library(tidyverse)
library(ggrepel)

set.seed(42)  

london <- read.csv("london_data.csv")
attach(london)

london_scale <- as.data.frame(london[, 3:ncol(london)])

london_scale <- scale(london_scale, center = apply(london_scale, 2, min), scale = apply(london_scale, 2, max) - apply(london_scale, 2, min))


london_scale2 <- round(as.data.frame(london_scale), digits = 2)
print(london_scale)
london_scale <- london
london_scale[,3:ncol(london)] <- london_scale2


london_scale2 <- (london_scale2 +1)^3
london_scale[,3:ncol(london)] <- london_scale2

#t
london_log <- log(london[,3:ncol(london)])^2
london_log$Area_name <- london$Area_name

data <- london_scale[, c(
  "Population_density_.per_hectare._2016",
  "Average_Age._2016",
  "X._of_resident_population_born_abroad_.2014.",
  "Employment_rate_..._.2015.",
  "Modelled_Household_median_income_estimates_2012.13",
  "Crime_rates_per_thousand_population_2014.15",
  "Median_House_Price._2014",
  "Average_Public_Transport_Accessibility_score._2014",
  "Achievement_of_5_or_more_A.._C_grades_at_GCSE_or_equivalent_including_English_and_Maths._2013.14"
)]

# Add the Area_name column to the data
data$Area_name <- london$Area_name

data_long <- tidyr::gather(data, key = "Variable", value = "Value", -Area_name)

#cluster 1
selected_areas <- c("Barking_and_Dagenham", "Croydon", "Enfield", "Greenwich", "Hillingdon", 'Lewisham')  # 선택한 지역 이름으로 변경
#cluster 2
#selected_areas <- c("Barnet", "Brent", "Camden", "Ealing", "Hackney", 'Hammersmith_and_Fulham', 'Harrow', 'Hounslow', 'Islington', 'Kensington_and_Chelsea', 'Lambeth',
               #     'Newham', 'Redbridge', 'Southwark', 'Tower_Hamlets', 'Wandsworth', 'Westminster')  # 선택한 지역 이름으로 변경
#cluster 3
#selected_areas <- c("Bexley", "Bromley", "Haringey", "Havering", "Kingston_upon_Thames", 'Merton',
           #         'Richmond_upon_Thames', 'Sutton', 'Waltham_Forest')  # 선택한 지역 이름으로 변경

# 데이터 필터링
selected_data <- data_long[data_long$Area_name %in% selected_areas, ]
other_data <- data_long[!(data_long$Area_name %in% selected_areas), ]

# 시각화
ggplot() +
  geom_line(data = other_data, aes(x = Variable, y = Value, group = Area_name), color = "grey") +
  geom_point(data = other_data, aes(x = Variable, y = Value), color = "grey", size = 2) +
  geom_line(data = selected_data, aes(x = Variable, y = Value, group = Area_name), color = "salmon") +
  geom_point(data = selected_data, aes(x = Variable, y = Value, color = Area_name), size = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "<Parallel Coordinate Plot of Cluster1>", x = "", y = "Value",  caption = "Copyright © 2023 Daeseong Seo. All rights reserved.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Area_name", breaks = selected_areas, labels = selected_areas)

