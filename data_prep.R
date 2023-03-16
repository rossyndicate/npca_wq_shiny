library(tidyverse)
library(sf)
library(nhdplusTools)
library(mapview)
library(rmapshaper)

# park boundary polygons
parks <- st_read('data/prep/nps_boundary.shp') %>%
  select(Park = UNIT_NAME) %>%
  # st_make_valid() %>%
  # st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # ms_simplify() %>% 
  st_transform(4326) %>%
  saveRDS('data/nps_boundary_lines.RDS')
parks <- readRDS('data/nps_boundary_lines.RDS')
# park boundy polylines
boundary_lines <- st_read('data/prep/nps_boundary_line.shp') %>%
  select(Park = UNIT_NAME) %>%
  st_transform(4326) %>%
  saveRDS('data/nps_boundary_lines.RDS')

# clipped catchments in attains
attains_inside <- st_read('data/prep/nps_attains_inside.shp') %>%
  select(Assessment_Unit = assssmn,
         comid = nhdplsd,
         Assessment_Category = ovrllst,
         Impairments = imprmnt,
         Park = UNIT_NAME) %>%
  # st_make_valid() %>%
  # st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # ms_simplify() %>%
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
  # st_make_valid() %>%
  # st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # ms_simplify() %>%
  st_transform(4326) %>%
  saveRDS('data/nps_attains_outside.RDS')
attains_outside <- readRDS('data/nps_attains_outside.RDS')

# catchments without info:
unassessed_outside <- st_read('data/prep/nps_raw_outside.shp') %>%
  select(comid = FEATUREID,
         Park = UNIT_NAME) %>%
  # only keep catchments that aren't listed in attains
  filter(!comid %in% attains_outside$comid & !comid %in% attains_inside$comid) %>%
  # st_make_valid() %>%
  # st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # ms_simplify() %>% 
  st_transform(4326) %>%
  saveRDS('data/nps_unassessed_outside.RDS')
unassessed_outside <- readRDS('data/nps_unassessed_outside.RDS')

# clipped catchments without info:
unassessed_inside <- st_read('data/prep/nps_raw_inside.shp') %>%
  select(comid = FEATUREID,
         Park = UNIT_NAME) %>%
  # only keep catchments that aren't listed in attains
  filter(!comid %in% attains_outside$comid & !comid %in% attains_inside$comid) %>%
  # st_make_valid() %>%
  # st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # ms_simplify() %>% 
  st_transform(4326) %>%
  saveRDS('data/nps_unassessed_inside.RDS')
unassessed_inside <- readRDS('data/nps_unassessed_inside.RDS')

inside <- bind_rows(attains_inside, unassessed_inside) %>%
  mutate(Assessment_Unit = ifelse(is.na(Assessment_Unit), "None", Assessment_Unit)) %>%
  mutate(Assessment_Category = ifelse(is.na(Assessment_Category), "No Data", Assessment_Category)) %>%
  saveRDS('data/nps_inside.RDS')
outside <- bind_rows(attains_outside, unassessed_outside) %>%
  mutate(Assessment_Unit = ifelse(is.na(Assessment_Unit), "None", Assessment_Unit)) %>%
  mutate(Assessment_Category = ifelse(is.na(Assessment_Category), "No Data", Assessment_Category)) %>%
  saveRDS('data/nps_outside.RDS')
