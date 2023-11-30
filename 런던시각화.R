library(sf)
library(ggplot2)

setwd("~/다변량통계분석/기말프로젝트/")
ldn <- st_read("London_Borough_Excluding_MHW.shp")

ldn_coords <- st_coordinates(st_centroid(ldn))

label <- c("Kington_upon_Thames", "Croydon", "Bromley", "Hounslow", "Ealing", "Havering",
           "Hillingdon", "Harrow", "Brent", "Barent", "Lambeth", "South_wark", "Lewisham", "Greenwich", "Bexley",
           "Enfield", "WalthamForest", "Redbridge", "Sutton", "Richmond_upon_Thames", "Mertron",
           "Wandsworth", "Hammersmith_Fullham", "Kensington_Chelsea", "Wester_minster", "Camden",
           "Tower_Hamlets", "Islington", "Hackney", "Haringey", "Newham", "Barking_Dageham", "City")

cluster1 <- c("Greenwich", "Barking_Dageham", "Lewisham","Croydon", 
              "Hillingdon", 'Enfield')

cluster3 <- c("Haringey", 'WalthamForest', 'Richmond_upon_Thames',
              'Kingston_upon_Thames', 'Merton', 'Bromley',
              'Sutton', 'Havering', 'Bexley')

ggplot() +
  geom_sf(data = ldn, aes(fill = ifelse(label %in% cluster1, "cluster1", 
                                        ifelse(label %in% cluster3, 'cluster3', 'cluster2')))) +
  geom_text(data = as.data.frame(ldn_coords), aes(label = label, x = X, y = Y), 
            size = 2.5, color = "black") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("<London Borough Visualization with Clusters>") +
  scale_fill_manual(values = c(cluster1 = 'salmon', cluster2 = 'forestgreen', cluster3 = 'dodgerblue'),
                    name = "Cluster") +
  labs(fill = "Cluster",
       caption = "Copyright © 2023 Daeseong Seo. All rights reserved.")
