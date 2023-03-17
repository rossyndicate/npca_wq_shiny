library(tidyverse)
library(sf)
library(nhdplusTools)
library(mapview)
library(rmapshaper)
sf::sf_use_s2(FALSE)
colors = data.frame(
  Assessment_Category = c("Not Supporting", "Fully Supporting", "Not Assessed", NA),
  col = c("#DC851E", "#059FA4", "#A1A522", "#C2CAD7"),
  Priority = c(1, 2, 3, 4))

# park boundary polygons
# parks <- st_read('data/prep/nps_boundary.shp') %>%
#   select(Park = UNIT_NAME) %>%
#   st_transform(4326) %>%
#   saveRDS('data/nps_boundary_lines.RDS')
# parks <- readRDS('data/nps_boundary_lines.RDS')

# park boundy polylines
boundary_lines <- st_read('data/prep/nps_boundary_line.shp') %>%
  select(Park = UNIT_NAME) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_boundary_lines.RDS')
parks <- readRDS('data/nps_boundary_lines.RDS')

# clipped catchments in attains
attains_inside <- st_read('data/prep/nps_attains_inside.shp') %>%
  select(Assessment_Unit = assssmn,
         comid = nhdplsd,
         Assessment_Category = ovrllst,
         Impairments = imprmnt,
         Park = UNIT_NAME) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_attains_inside.RDS')
attains_inside <- readRDS('data/nps_attains_inside.RDS')

# catchments in attains
attains_outside <- st_read('data/prep/nps_attains_outside.shp') %>%
  select(Assessment_Unit = assssmn,
         comid = nhdplsd,
         Assessment_Category = ovrllst,
         Impairments = imprmnt,
         Park = UNIT_NAME) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_attains_outside.RDS')
attains_outside <- readRDS('data/nps_attains_outside.RDS')

# catchments without info:
unassessed_outside <- st_read('data/prep/nps_raw_outside.shp') %>%
  select(comid = FEATUREID,
         Park = UNIT_NAME) %>%
  # only keep catchments that aren't listed in attains
  filter(!comid %in% attains_outside$comid & !comid %in% attains_inside$comid) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_unassessed_outside.RDS')
unassessed_outside <- readRDS('data/nps_unassessed_outside.RDS')

# clipped catchments without info:
unassessed_inside <- st_read('data/prep/nps_raw_inside.shp') %>%
  select(comid = FEATUREID,
         Park = UNIT_NAME) %>%
  # only keep catchments that aren't listed in attains
  filter(!comid %in% attains_outside$comid & !comid %in% attains_inside$comid) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_unassessed_inside.RDS')
unassessed_inside <- readRDS('data/nps_unassessed_inside.RDS')

inside <- bind_rows(attains_inside, unassessed_inside) %>%
  sp::merge(colors, by ="Assessment_Category") %>%
  arrange(Priority) %>%
  distinct(comid, .keep_all = TRUE) %>%
  mutate(Assessment_Unit = ifelse(is.na(Assessment_Unit), "None", Assessment_Unit)) %>%
  mutate(Assessment_Category = case_when(Assessment_Category == "Fully Supporting" ~ "Good",
                                         Assessment_Category == "Not Supporting" ~ "Impaired",
                                         Assessment_Category == "Not Assessed" ~ "Not Assessed",
                                         is.na(Assessment_Category) ~ "No State Data")) %>% 
  saveRDS('data/nps_inside.RDS')
inside <- readRDS('data/nps_inside.RDS')

outside <- bind_rows(attains_outside, unassessed_outside) %>%
  sp::merge(colors, by ="Assessment_Category") %>%
  arrange(Priority) %>%
  distinct(Park, comid, .keep_all = TRUE) %>%
  mutate(Assessment_Unit = ifelse(is.na(Assessment_Unit), "None", Assessment_Unit)) %>%
  mutate(Assessment_Category = case_when(Assessment_Category == "Fully Supporting" ~ "Good",
                                         Assessment_Category == "Not Supporting" ~ "Impaired",
                                         Assessment_Category == "Not Assessed" ~ "Not Assessed",
                                         is.na(Assessment_Category) ~ "No State Data")) %>% 
  saveRDS('data/nps_outside.RDS')
outside <- readRDS('data/nps_outside.RDS') 
