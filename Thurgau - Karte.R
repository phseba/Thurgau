# Load and install packages
install.packages("tidyverse")
install.packages("sf")
install.packages("ggthemes")
install.packages("DescTools")
library(tidyverse)
library(sf)
library(ggthemes)
library(DescTools)

# map of the canton Thurgau
Thurgau_Gemeindegrenzen <- st_read("C:/R Projects/Thurgau/Gemeinden_TG/Gemeinden_TG.shp")
ggplot() +
  geom_sf(data = Thurgau_Gemeindegrenzen) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

# map modifications
Thurgau_Konsum_saurer_Most <- Thurgau_Gemeindegrenzen %>% 
  mutate(Konsum_saurer_Most = "Konsum")