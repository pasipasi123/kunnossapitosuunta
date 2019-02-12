#' ---
#' title: Kunnossapidon optimisuunta
#' author: Pasi Haapakorva
#' output: github_document
#' ---

#+ koodi, message=FALSE, warning=FALSE

library(tidyverse)
library(sf)
library(here)

rotuaari <- st_read(here("shp", "rotuaarin_pallo.shp")) %>% st_set_crs(3067)
paareitit <- st_read(here("shp", "paareitit_fixed.shp")) %>% st_set_crs(3067)
ruudut <- st_read(here("shp", "ruudut.shp")) %>% st_set_crs(3067)

# liitetään ruudut pääreitteihin
ruutu_join <- paareitit %>%
  st_buffer(1500) %>%
  st_join(ruudut)

# piirretään simppeli kuva
ruutu_join %>%
  st_set_geometry(NULL) %>%
  distinct(id_nro, .keep_all = TRUE) %>%
  st_as_sf(coords = c("xkoord", "ykoord")) %>%
  st_set_crs(3067) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = paareitit %>% st_simplify)

# lasketaan ruuduista etäisyys rotuaarille
rotuaari_dist <- ruutu_join %>%
  st_distance(rotuaari) %>%
  as.numeric() %>%
  enframe(name = NULL, value = "distance")

# poistetaan ruutudatasta geometria ja liitetään etäisyys
ruutu_dist <- ruutu_join %>%
  st_set_geometry(NULL) %>%
  as_tibble() %>%
  bind_cols(rotuaari_dist)

# lasketaan väestön kumuloituminen kahteen eri suuntaan
cum_dist <- ruutu_dist %>%
  distinct(id_nro, .keep_all = TRUE) %>%
  group_by(distance) %>%
  summarise(vaesto = sum(vaesto)) %>%
  mutate(peri_dist = max(distance) - distance) %>%
  mutate(kesk_vaesto = cumsum(vaesto)) %>%
  arrange(peri_dist) %>%
  mutate(peri_vaesto = cumsum(vaesto))

# piirretään kuva
cum_dist %>%
  select(contains("dist"), contains("_vaesto")) %>%
  ggplot(aes(distance, kesk_vaesto)) +
  geom_step(aes(color = "Keskustasta ulos")) +
  geom_step(aes(peri_dist, peri_vaesto, color = "Ulkoa keskustaan")) +
  scale_color_brewer(palette = "Set1", name = "Kunnossapidon suunta") +
  scale_x_continuous(labels = function(x) paste(x / 1000, "km")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
  labs(x = "Etäisyys", y = "Tavoitettu väestö",
       caption = "Etäisyys laskettu Rotuaarin pallolta, kun lähdetään keskustasta,
       tai uloimmaksi yltävän pääreitin kohdasta alkaen, kun lähdetään ulkoa.") +
  theme_bw() +
  theme(legend.position = "top")

ggsave(here("fig", "kuva.png"))
