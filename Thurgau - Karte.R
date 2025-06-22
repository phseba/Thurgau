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
Thurgau_Gemeindegrenzen <- st_read(
  "C:/R Projects/Thurgau/Gemeinden_TG/Gemeinden_TG.shp")
Thurgau_Gemeindegrenzen <- Thurgau_Gemeindegrenzen %>% 
  mutate(ID = c(1:80)) %>% 
  relocate(ID) %>% 
  arrange(Thurgau_Gemeindegrenzen$gemeinde_n)

# table of the canton Zuerich
Zuerich_Gemeindegrenzen <- st_read(
  "C:/R Projects/Thurgau/cfd33eee54344c5ea14031030ab9ba02/Gemeindegrenzen_-OGD/UP_GEMEINDEN_F.shp")
Zuerich_Gemeindegrenzen <- Zuerich_Gemeindegrenzen %>% 
  mutate(ID = c(1:165)) %>% 
  relocate(ID) %>% 
  arrange(Zuerich_Gemeindegrenzen$GEMEINDENA)

# table modifications of the canton Thurgau
Konsum_von_saurem_Most_am_Arbeitsplatz <- Thurgau_Gemeindegrenzen %>% 
  mutate(Konsum_von_saurem_Most_am_Arbeitsplatz_diskret = c(1:80)) %>% 
  mutate(Konsum_von_saurem_Most_am_Arbeitsplatz_kategorisch = c("Hoch"))
Geburten_Thurgau <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  mutate(Geburten_kategorisch = c("Viele")) %>% 
  mutate(Geburten_diskret = c(c(1:80)))
Frauenfeld <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "170") 
Muensterlingen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "179")
Bottighofen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "183")
Diessenhofen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "198")
Kreuzlingen <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "168")
Amriswil <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "233")
Weinfelden <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "223")
Arbon <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "192")
Bischofszell <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  filter(objectid == "186")
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

# map of the canton Zuerich
ggplot() +
  geom_sf(data = Zuerich_Gemeindegrenzen) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

# another map of the canton Zuerich
ggplot() +
  geom_sf(data = Zuerich_Gemeindegrenzen) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf_text(data = Zuerich_Gemeindegrenzen, 
               aes(label = ID)) +
  theme_void()

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
  geom_sf_text(data = Konsum_von_saurem_Most_am_Arbeitsplatz, 
               aes(label = ID)) +
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
               aes(label = Konsum_von_saurem_Most_am_Arbeitsplatz_diskret)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz_kategorisch)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Muensterlingen, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz_diskret)) +
  theme_void()

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Muensterlingen, 
          aes(fill = Konsum_von_saurem_Most_am_Arbeitsplatz_kategorisch)) +
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

# map of both the canton Thurgau and Zuerich
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Zuerich_Gemeindegrenzen) + 
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
  geom_sf_text(data = Frauenfeld, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Amriswil, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Arbon, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Bischofszell, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Diessenhofen, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Kreuzlingen, 
               aes(label = gemeinde_n)) +
  theme_void() +
  geom_sf_text(data = Weinfelden, 
               aes(label = gemeinde_n)) +
  theme_void() 
