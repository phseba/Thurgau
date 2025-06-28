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

# table of the canton Thurgau
Dataframe_Thurgau_Gemeindegrenzen <- as.data.frame(Thurgau_Gemeindegrenzen)

# table by STGAG
Thurgau_1 <- Thurgau_1 
Thurgau_1 <- rename(Thurgau_1, gemeinde_b = Identifikator)

# join tables
Dataframe_STGAG <- full_join(Dataframe_Thurgau_Gemeindegrenzen, Thurgau_1, 
                              by = "gemeinde_b")
Dataframe_STGAG <- st_as_sf(Dataframe_STGAG)
Dataframe_STGAG <- rename(Dataframe_STGAG, Anteil_STGAG_Geburten = 
                            "Anteil STGAG-Geburten")

# change proportion values greater than 1
Dataframe_STGAG$Anteil_STGAG_Geburten <- ifelse(
  Dataframe_STGAG$Anteil_STGAG_Geburten >= 1.0, 1.0, 
  Dataframe_STGAG$Anteil_STGAG_Geburten)

# table modifications of the canton Thurgau
Konsum_von_saurem_Most_am_Arbeitsplatz <- Thurgau_Gemeindegrenzen %>% 
  mutate(Konsum_von_saurem_Most_am_Arbeitsplatz_diskret = c(1:80)) %>% 
  mutate(Konsum_von_saurem_Most_am_Arbeitsplatz_kategorisch = c("Hoch"))
Geburten_Thurgau <- Konsum_von_saurem_Most_am_Arbeitsplatz %>%
  mutate(Geburten_kategorisch = c("Viele")) %>% 
  mutate(Geburten_diskret = c(c(1:80))) %>% 
  mutate(Geburten_total = c(2)) 
Geburten_Thurgau$Geburten_ratio <- Geburten_Thurgau$Geburten_diskret /
  Geburten_Thurgau$Geburten_total
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

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Geburten_Thurgau, 
          aes(fill = Geburten_diskret)) +
  scale_fill_viridis_c(direction = -1) +
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

# another map of the canton Thurgau
ggplot() +
  geom_sf(data = Konsum_von_saurem_Most_am_Arbeitsplatz) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  geom_sf(data = Dataframe_STGAG, 
          aes(fill = Anteil_STGAG_Geburten)) +
  scale_fill_viridis_c(direction = -1) +
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
