# load and install packages
install.packages("tidyverse")
install.packages("sf")
install.packages("ggthemes")
install.packages("DescTools")
install.packages("ggrepel")
library(tidyverse)
library(sf)
library(ggthemes)
library(DescTools)
library(ggrepel)

# table of the canton Thurgau
Thurgau_Gemeindegrenzen <- st_read("C:/R Projects/Thurgau/Gemeinden_TG/Gemeinden_TG.shp")

# list of new values
Liste_neu <- c(1:80)
view(Liste_neu)

# table modifications
Konsum_von_saurem_Most_am_Arbeitsplatz <- Thurgau_Gemeindegrenzen %>% 
  mutate(Konsum_von_saurem_Most_am_Arbeitsplatz = c(1:80))
Frauenfeld <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "170") 
Muensterlingen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "179")
Bottighofen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "183")
Geburten_Frauenfeld <- Konsum_von_saurem_Most_am_Arbeitsplatz %>% 
  mutate(Geburten = c("Wenige")) %>% 
  filter(objectid == "170") 
Geburten_Muensterlingen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  mutate(Geburten = c("Viele")) %>% 
  filter(objectid == "179") 
Geburten_Bottighofen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  mutate(Geburten = c("Viele")) %>% 
  filter(objectid == "183") 

# map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf_text(data = Konsum_von_saurem_Most_am_Arbeitsplatz, 
               aes(label = gemeinde_b)) +
  theme_void()
  
# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf_text(data = Frauenfeld, 
               aes(label = gemeinde_n)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf_text(data = Konsum_von_saurem_Most_am_Arbeitsplatz, 
               aes(label = Konsum_von_saurem_Most_am_Arbeitsplatz)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Muensterlingen, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Muensterlingen, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Geburten_Frauenfeld, 
          aes(fill = Geburten)) +
  geom_sf(data = Geburten_Bottighofen, 
          aes(fill = Geburten)) +
  theme_void() +
  geom_sf_text(data = Frauenfeld, 
               aes(label = gemeinde_n)) +
  geom_sf_text(data = Bottighofen, 
               aes(label = gemeinde_n)) +
  ggtitle("Geburten im Jahr 2024")
