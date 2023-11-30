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

options(scipen = 999, digits = 3)

y <- c(
  "Population_density_.per_hectare._2016",
  "Average_Age._2016",
  "X._of_resident_population_born_abroad_.2014.",
  "Employment_rate_..._.2015.",
  "Modelled_Household_median_income_estimates_2012.13",
  "Crime_rates_per_thousand_population_2014.15",
  "Median_House_Price._2014",
  "Average_Public_Transport_Accessibility_score._2014",
  "Achievement_of_5_or_more_A.._C_grades_at_GCSE_or_equivalent_including_English_and_Maths._2013.14"
)
summary_table <- skim(london[, y]) %>%
  select(-skim_type, -n_missing, -complete_rate) %>% 
  kable()

styled_table <- summary_table %>%
  kable_styling()

styled_table

par(mfrow = c(3, 3))  

for (i in 1:length(y)) {
  boxplot(london[, y[i]], col = "lightblue", main = y[i])
}

par(mfrow = c(1, 1))


london_scale <- as.data.frame(london[, 3:ncol(london)])
london_scale <- sapply(london_scale, as.numeric)
london_scale <- scale(london_scale, center = apply(london_scale, 2, median), scale = apply(london_scale, 2, IQR))
london_scale2 <- round(as.data.frame(london_scale), digits = 2)
print(london_scale)
london_scale <- london
london_scale[,3:ncol(london)] <- london_scale2




london_pca <- prcomp(london_scale[,3:ncol(london_scale)])
summary(london_pca)

london_pca$x

pca_data <- data.frame(london_pca$x, street = london_scale$Area_name)

q1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = street)) + geom_point() +
  geom_label_repel(aes(label = street), box.padding = 0.5, point.padding = 1) +  # 여백 및 중앙 맞춤 조절
  theme(plot.title = element_text(hjust = 0.5)) +  # 그래프 제목 중앙 정렬
  labs(title = "<PCA>", caption = "Copyright © 2023 Daeseong Seo. All rights reserved.")  # 그래프 제목 추가
q1


#TSNE
set.seed(42)  
tsne <- Rtsne(london_scale, dims = 2, perplexity = 1, verbose = TRUE)
tsne_data <- data.frame(tsne1 = tsne$Y[,1], tsne2 = tsne$Y[,2], street = london$Area_name)
t1 <- ggplot(tsne_data, aes(x = tsne1, y = tsne2, color = street)) + geom_point() +
  geom_label_repel(aes(label = street), box.padding = 0.5, point.padding = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "<t-SNE>", caption = "Copyright © 2023 Daeseong Seo. All rights reserved.")
t1


#UMAP
set.seed(42)  
umap <- umap(london_scale[,3:ncol(london_scale)], n_neighbors = 2, min_dist = 0.3, n_components = 2)
umap_data <- data.frame(umap1 = umap$layout[,1], umap2 = umap$layout[,2], street = london$Area_name)
u1 <- ggplot(umap_data, aes(x = umap1, y = umap2, color = street)) + geom_point() +
  geom_label_repel(aes(label = street), box.padding = 0.5, point.padding = 1, max.overlaps = Inf) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "<UMAP>", caption = "Copyright © 2023 Daeseong Seo. All rights reserved.")
u1


distance_matrix <- dist(umap_data[, c("umap1", "umap2")])
hclust_result <- hclust(distance_matrix, method = "ward.D")

# 덴드로그램 그리기
plot(hclust_result, main = "Hierarchical Clustering based on UMAP", xlab = "Observations", sub = "", cex = 0.8, labels = umap_data$street)
mtext("Copyright © 2023 Daeseong Seo. All rights reserved.", side = 1, line = 4, adj = 1, cex = 0.7)


# 군집 결과를 데이터프레임에 추가
umap_data$cluster <- cutree(hclust_result, k = 3)  # k는 군집의 수

# UMAP 시각화 with 군집 색상
u1 <- ggplot(umap_data, aes(x = umap1, y = umap2, color = as.factor(cluster), label = street)) +
  geom_point() +
  geom_label_repel(aes(label = street), box.padding = 0.5, point.padding = 1, max.overlaps = Inf) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "<UMAP with Hierarchical Clustering>", caption = "Copyright © 2023 Daeseong Seo. All rights reserved.")
u1


subset_data <- umap_data[umap_data$cluster == 3, ]
subset_data$street

