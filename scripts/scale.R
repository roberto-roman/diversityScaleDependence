# setup -------------------------------------------------------------------

pacman::p_load(tidyverse, vegan, readxl, writexl, 
               santoku, sf, sp, rgbif, rnaturalearth,
               rnaturalearthdata, dbscan, ggnewscale,
               caret, vegan)

# clean data ---------------------------------------------------------------
db_01 <- readxl::read_xls("data/base_actual_plots2023.xls")

## dates
mon_ab_names <- 
  date_names_lang("es") %>% 
  .$mon_ab %>% 
  str_remove('\\.') %>% 
  str_remove('(?<=p)t$')

es_mx <- 
  date_names(date_names_lang("es")$mon,
             mon_ab_names,
             date_names_lang("es")$day,
             date_names_lang("es")$day_ab)

db_02 <-   
  db_01 %>% 
  mutate(
    DATE = str_to_lower(DATE),
    DATE2 = 
      case_when(
        
        str_detect(DATE, '^[a-z]+-') ~
          parse_date(DATE,
                     "%B-%d-%Y",
                     locale = locale("es")),
        
        str_detect(DATE, '^\\d{2}-[a-z]{3}-') ~ 
          parse_date(DATE, 
                     "%d-%b-%y",
                     locale =
                       locale(es_mx)),
        
        str_detect(DATE, '^\\d{2}-[a-z]+-') ~
          parse_date(DATE,
                     "%d-%B-%y",
                     locale =
                       locale("es"))
      ),
    DATE2 = 
      if_else(
        is.na(DATE2),
        parse_date(DATE, 
                   "%d-%b-%y"),
        DATE2),
    DATE2 = 
      if_else(
        is.na(DATE2),
        parse_date(DATE, 
                   "%d-%B-%Y",
                   locale = locale("es")),
        DATE2),
    DATE2 = 
      case_when(
        DATE == "14-diciember-05" ~ as.Date("2005-12-14"),
        DATE == "4-junio-07" ~ as.Date("2007-06-04"),
        DATE == "38637" ~ as.Date(38637, origin = "1899-12-30"),
        TRUE ~ DATE2)
  ) 


## time of day, forest, climate
db_03 <- 
  db_02 %>% 
  mutate(`TIME OF DAY` = str_to_lower(`TIME OF DAY`),
         `TIME OF DAY` = 
           case_when(
             `TIME OF DAY` == 'mañana' ~ 'morning',
             `TIME OF DAY` == 'noon' ~ 'morning',
             `TIME OF DAY` == 'tarde' ~ 'afternoon',
             is.na(`TIME OF DAY`) ~ 'unknown',
             TRUE ~ `TIME OF DAY`),
         FOREST = 
           case_when(
             FOREST == "isolated" ~ "primary_isolated",
             FOREST == "secondary" ~ "secondary_forest",
             FOREST == "isolated" ~ "isolated_forest",
             FOREST == "Bosque primario" ~ "primary_forest",
             FOREST == "Bosque secundario" ~ "secondary_forest",
             is.na(FOREST) ~ 'unknown',
             TRUE ~ FOREST),
         FOREST =
           FOREST %>% 
           str_replace(' ', '_') %>% 
           str_to_lower(),
         CLIMATE = 
           str_to_lower(CLIMATE),
         CLIMATE = 
           case_when(
             CLIMATE == 'lluvioso' ~ 'rainy',
             CLIMATE == 'soleado' ~ 'sunny',
             CLIMATE == 'nublado' ~ 'cloudy',
             CLIMATE == 'nublado y palpita lluvia' ~ 'cloudy',
             CLIMATE == 'lluvia' ~ 'rainy',
             is.na(CLIMATE) ~ 'unknown',
             TRUE ~ CLIMATE),
         CLIMATE = 
           str_replace(CLIMATE, ' ', '_'),
         ID_RECORD = 1:n()
  ) %>% 
  nest(data = -c(lat, long)) %>% 
  mutate(ID_COORD = 1:n()) %>% 
  unnest()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- clean coordinates ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
manual_rev_coords <- 
  read_csv('export/manual_clean_coord01.csv') %>%
  select(lat = original_lat, long = original_long, ID_COORD)

decimal_coords <- 
  db_03 %>%
  distinct(ID_COORD, lat, long) %>% 
  rows_update(manual_rev_coords, by  = 'ID_COORD') %>% 
  filter(!str_detect(lat, '17M')) %>% 
  mutate(
    original_lat = lat,
    original_long = long,
    lat = 
      if_else(str_detect(lat, '(\"|\'|\\d)$'), 
              paste0(lat, 'S'),
              lat),
    lat = 
      lat %>% 
      str_replace('º', '°') %>% 
      str_replace('(?<=00)\\s', '°') %>% 
      str_remove_all(' ')  %>% 
      str_replace('(?<!\'|\")S', '\'S') %>% 
      str_replace(',', '\\.') %>% 
      str_replace( "´", "\'") %>% 
      str_replace('(?<!\'|\")N', '\'N') %>% 
      str_replace('(?<=\\d{1,3}°\\d{1,3})°', '\'') %>% 
      str_replace('(?<=(\'|\")[0-9.]{2,7})\'(?=S)', '\"') %>% 
      str_replace('(?<=^\\d{1,2}°[0-9.]{2,7})\"', '\'') %>% 
      str_remove('(?<=\\.\\d{1,3})\\.'),
    long = 
      if_else(str_detect(long, '(\"|\'|\\d)$'), 
              paste0(long, 'W'),
              long),
    long = 
      long %>% 
      str_replace('º', '°') %>% 
      str_replace('(?<=77)(S| )', '°') %>% 
      str_remove_all(' ') %>% 
      str_replace('(?<!\'|\")W', '\'W') %>% 
      str_replace(',', '\\.') %>% 
      str_replace("´", "\'") %>% 
      str_replace('(?<=\\d{1,3}°\\d{1,3})°', '\'') %>% 
      str_replace('(?<=(\'|\")[0-9.]{2,7})\'(?=W)', '\"') %>% 
      str_replace('(?<=^\\d{1,2}°[0-9.]{2,7})\"', '\'') %>% 
      str_replace('(?<=^\\d{1,3}°[0-9.]{2,7})\"', '\'') %>%
      str_remove('(?<=\\.\\d{1,3})\\.'),
    lat2 = map_dbl(lat, ~sp::char2dms(.x, '°', '\'', '\\"') %>% as.double()),
    long2 = map_dbl(long, ~sp::char2dms(.x, '°', '\'', '\\"') %>% as.double())
    ) %>% 
  select(!c(lat, long))

# write_csv(decimal_coords, 'export/manual_clean_coord02.csv')

utm_coords <- 
  db_03 %>% 
  distinct(ID_COORD, lat, long) %>% 
  filter(str_detect(lat, '17M')) %>% 
  mutate(
    original_lat = lat,
    original_long = long,
    lat = str_remove(lat, '17M')) %>% 
  st_as_sf(coords = c('lat', 'long'), crs = 32717) %>% 
  st_transform(crs = 4326) %>% 
  mutate(
    long2 = st_coordinates(geometry)[,1],
    lat2 = st_coordinates(geometry)[,2]
  ) %>% 
  st_drop_geometry()

coords00 <- 
  bind_rows(utm_coords, decimal_coords)

# manual clean
# coords00 %>% 
#   write_csv('export/manual_clean_coord100.csv')

# ecuador
ec <-
  rnaturalearth::ne_countries(country = 'Ecuador',
                              returnclass = 'sf',
                              scale = 'medium')
coords01  <-
  coords00 %>% 
  mutate(X = long2, Y = lat2) %>% 
  filter(!is.na(X)&!is.na(Y)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4326) %>% 
  st_intersection(ec) %>% 
  st_drop_geometry() %>% 
  select(ID_COORD, original_lat, original_long, long2, lat2)


db_04 <- 
  db_03 %>% 
  mutate(elevation = parse_number(elevm),
         fct.elev = chop_pretty(elevation, 5)) %>% 
  left_join(coords01, by = 'ID_COORD') %>% 
  filter(!is.na(lat2)&!is.na(long2))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- find coords ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# db_04 %>% 
#   distinct(NUM, LOC, .keep_all = T) %>% 
#   # filter(NUM > 530, NUM < 630) %>% 
#   filter(str_detect(LOC, 'Vinillos')) %>%
#   view()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- manual coord correction ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db.coord.correction <- 
  db_04 %>% 
  mutate(x = long2, y = lat2) %>%
  select(NUM, LOC, DATE2,
         elevation, lat2, long2, x, y) %>%
  distinct(NUM, LOC, .keep_all = T) %>%
  st_as_sf(coords = c('x', 'y'), crs  = 4326) %>% 
  st_transform(crs = 32717) %>% 
  mutate(long2 = st_coordinates(geometry)[,1],
         lat2 = st_coordinates(geometry)[,2]) %>% 
  st_drop_geometry()

manual.coord <-
  read_sf('intermedium/quadrats_dbscan_fixcoord.gpkg')

manual.coord.02 <- 
  manual.coord %>% 
  st_drop_geometry() %>% 
  select(NUM, LOC, x.utm = xfix, y.utm = yfix) %>% 
  filter(x.utm != 0)

db_05 <- 
db_04 %>% 
  mutate(x = long2, y = lat2) %>%
  st_as_sf(coords = c('x', 'y'), crs  = 4326) %>% 
  st_transform(crs = 32717) %>% 
  mutate(x.utm = st_coordinates(geometry)[,1],
         y.utm = st_coordinates(geometry)[,2]) %>% 
  st_drop_geometry() %>% 
  rows_update(manual.coord.02, by = c('NUM', 'LOC')) %>% 
  filter(!is.na(GEN))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- taxonomic depuration ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(taxize)

ds.taxize  <- gnr_datasources()

dict.spp <- 
db_05 %>% 
  distinct(GEN) %>% 
  mutate(id.species = GEN,
         GEN = 
           GEN %>% 
           str_replace('Palicouera', 'Palicourea') %>% 
           str_replace('Munozia', 'Munnozia') %>% 
           str_replace('Cynoxys', 'Gynoxys') %>% 
           str_replace('Bacharia', 'Baccharis') %>% 
           str_replace('Bomara', 'Bomarea') %>% 
           str_replace('Coffee', 'Coffea arabica') %>% 
           str_replace('Ruelia', 'Ruellia') %>% 
           str_replace('Fiscus', 'Ficus') %>% 
           str_replace('Purouma', 'Pourouma') %>% 
           str_replace('Bohemeria', 'Boehmeria'))

dict.spp.01 <- 
dict.spp %>% 
  mutate(GEN = str_remove(GEN, ' sp$') %>% 
           str_to_sentence(),
         df.parsed.names = 
          map(GEN, ~gnr_resolve(.x, 
                           data_source_ids = 165,
                           canonical = T)))

dict.spp.02 <- 
dict.spp.01 %>% 
  unnest(df.parsed.names, keep_empty = T) %>% 
  mutate(matched_name2 = 
           str_replace(matched_name2, 
                       'Baccharis angustifolia', 
                       'Baccharis arbutifolia')) %>% 
  distinct(id.species, .keep_all = T) %>% 
  select(id.species, matched_name2)

db_05 <- 
db_05 %>% 
  left_join(dict.spp.02, by = c('GEN' = 'id.species')) %>% 
  mutate(GEN = matched_name2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- kmeans clustering ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Poor cluster performance
db.clustering <- 
  db_05 %>% 
  mutate(x = x.utm, y = y.utm) %>%
  select(NUM, LOC, DATE2,
         elevation, fct.elev, x.utm, y.utm, x, y) %>%
  distinct(NUM, LOC, .keep_all = T) %>% 
  mutate(id = 1:n())

## K-means clustering
db_elbow <- 
  tibble(centers = 1:30) %>% 
  mutate(residual =
           map_dbl(centers, 
               ~kmeans(as.matrix(select(db.clustering, elevation, y.utm, x.utm)), 
                       centers = .x) %>% 
                 .$withinss %>% 
                 sum()))

db_elbow %>% 
  ggplot() +
  geom_line(aes(centers, residual))

set.seed(132)
db.clustering %>% 
  mutate(group = 
           as.matrix(x.utm, y.utm, elevation) %>% 
           kmeans(centers = 10, nstart = 40, iter.max = 6000) %>% 
           .$cluster)  %>%
  mutate(x = x.utm, y = y.utm) %>% 
  st_as_sf(coords = c('x', 'y'), crs  = 32717) %>% 
  write_sf('intermedium/quadrats_kmeans.gpkg')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- DBSCAN cluster ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## DBSCAN better algorithm for spatial data
dist.matrix <- 
  db.clustering %>% 
  select(elevation, y.utm, x.utm, id) %>% 
  mutate(across(c(y.utm, x.utm, elevation),
                ~(.x - mean(.x))/sd(.x))) %>%
  column_to_rownames('id') %>% 
  as.matrix() %>% 
  dist() 

kNNdistplot(dist.matrix, 10)

## Loop to select best parameters for DBSCAN
list.dbscan <- list() 
list.per.index <- list()
eps.vect <- seq(0.1, 1.3, 0.1)
minPts.vect <- 4:24

for (i in seq_along(eps.vect)) {
  for (j in seq_along(minPts.vect)) {
    list.per.index[[j]] <- 
      dbscan(dist.matrix, 
             eps = eps.vect[[i]], 
             minPts = minPts.vect[[j]])$cluster %>% 
      table() %>% 
      as.list() %>% 
      unlist()
  }
  
  names(list.per.index) <- minPts.vect
  list.dbscan[[i]] <- list.per.index
}

db.models.dbscan <- 
list.dbscan %>% 
  tibble(a = .) %>% 
  unnest(a) %>% 
  mutate(eps = expand.grid(minPts.vect, eps.vect)[[2]],
         minPts = expand.grid(minPts.vect, eps.vect)[[1]],
         group = map(a, names)) %>% 
  unnest(c(a, group)) 

db.models.dbscan.plot <-
db.models.dbscan %>% 
  group_by(eps, minPts) %>%
  summarise(n.group = length(group),
            miss.group = a[group == 0]) %>% 
  ungroup() %>% 
  pivot_longer(c(n.group, miss.group)) %>% 
  ungroup()

## Plot best parameters to cluster quadrats
db.models.dbscan.plot %>% 
  ggplot(aes(as.factor(minPts), as.factor(eps))) +
  
  geom_tile(data = ~ subset(., name == 'n.group'),
            aes(fill = value)) +
  scale_fill_distiller(palette = "Reds") +
  new_scale_fill() +
  
  geom_tile(data = ~ subset(., name == 'miss.group'),
            aes(fill = value)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  new_scale_fill() +
  
  facet_wrap(~name)

## Final model DBSCAN
model.dbscan <- 
  dbscan (dist.matrix, eps = 0.7, minPts = 8)
model.dbscan

db.clustering.dbscan <- 
db.clustering %>% 
  mutate(group = 
           model.dbscan$cluster,
         fct.elev = santoku::chop_evenly(elevation, 6),
         group = group + 10 )

db.clustering.dbscan %>% 
  st_as_sf(coords = c('x', 'y'), crs  = 32717) %>% 
  write_sf('intermedium/quadrats_dbscan.gpkg')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Second cluster in group1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  DBSCAN clustering in group 11; group with the greatest number of plots
cluster.group1 <- 
  db.clustering.dbscan %>% 
  mutate(x = x.utm, y = y.utm) %>% 
  filter(group == 11)

dist.matrix.group1 <- 
  select(cluster.group1, x.utm, y.utm, elevation, id) %>%
  column_to_rownames('id') %>% 
  as.matrix() %>% 
  dist()

model.dbscan.group1 <- 
  hdbscan(dist.matrix.group1, minPts = 10)
model.dbscan.group1

cluster.group1.01 <- 
cluster.group1 %>% 
  mutate(group = 
           model.dbscan.group1$cluster)

## unite clusters
db.double.cluster <- 
db.clustering.dbscan %>% 
  filter(!group %in% c(11)) %>% 
  bind_rows(cluster.group1.01)

id.outliers.first.cluster <- 
db.clustering.dbscan %>% 
  filter(group == 10) %>% 
  pull(id)

id.outliers.second.cluster <- 
cluster.group1.01 %>%
  filter(group == 0) %>% 
  pull(id)

id.outliers <- 
  c(id.outliers.first.cluster, id.outliers.second.cluster)

id.with.class <- 
  db.double.cluster %>%
  filter(!group %in% c(0, 10)) %>% 
  pull(id)

outlier.classes <-
dist.matrix %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column('id') %>% 
  tibble() %>% 
  pivot_longer(-id) %>% 
  filter(name < id,
         id %in% id.outliers,
         name %in% id.with.class,
         value < 1) %>% 
  group_by(id) %>% 
  filter(rank(value, ties.method = 'first') %in% 1:5) %>% 
  ungroup() %>% 
  mutate(across(c(id, name), ~parse_number(.x))) %>% 
  left_join(db.double.cluster %>%
              filter(!group %in% c(0, 10)) %>% 
              select(group, id),
            by = c('name' = 'id')) %>%
  count(id, group) %>% 
  arrange(id, desc(n)) %>% 
  distinct(id, .keep_all = T) %>% 
  select(-n)

db.double.cluster.01 <- 
  db.double.cluster %>% 
  rows_update(outlier.classes, by = 'id')

# db.double.cluster.01 %>% 
#   count(group, fct.elev) %>% 
#   filter(n >= 9) %>% 
#   count(fct.elev)

db.double.cluster.01 %>% 
st_as_sf(coords = c('x', 'y'), crs  = 32717) %>% 
  write_sf('intermedium/quadrats_total_dbscan.gpkg')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- merge cluster group with db ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_06 <- 
db_05 %>% 
  left_join(db.double.cluster.01 %>% 
              select(NUM, LOC, group),
            by = c('NUM', 'LOC')) %>% 
  mutate(fct.elev = santoku::chop_evenly(elevation, 6),
         parcel = paste(NUM, LOC))

## filter db without groups with least than 10 plots; usually group with outliers
db_07 <- 
db_06 %>% 
  group_by(group, fct.elev) %>% 
  mutate(n.parcel = length(unique(parcel))) %>% 
  ungroup() %>% 
  nest(data = -c(group, fct.elev, n.parcel)) %>% 
  filter(n.parcel > 9) %>% 
  arrange(group) %>% 
  unnest()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- obtaining climatic variables ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(geodata, terra)

## download worldclim data for EC
# map(c('tmin', 'tmax', 'tavg', 'prec', 'wind', 'bio'),
#     ~worldclim_country(var = .x,
#                        res = 0.5, 
#                        path = 'data/wordclim_vars/',  
#                        country = 'Ecuador'))

temperature <-
  list.files('data/wordclim_vars/wc2.1_country/', full.names = T) %>% 
  str_subset('_t') %>% 
  map(rast)

bio.worldclim <- 
  rast('data/wordclim_vars/wc2.1_country/ECU_wc2.1_30s_bio.tif')

vect.parcel <- 
  db_07 %>% 
  distinct(fct.elev, parcel, x.utm, y.utm) %>% 
  mutate(x = x.utm, y = y.utm) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 32717) %>% 
  st_transform(4326) %>% 
  vect()

parcel.bio.wordclim <- 
  bio.worldclim %>% 
  extract(vect.parcel, bind = T) %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  tibble() %>% 
  rename_with(~str_extract(.x, 'bio_\\d+'), starts_with('wc2')) %>% 
  select(-ends_with('utm'))

db_08 <- 
  db_07 %>% 
  left_join(parcel.bio.wordclim,
            by = c('fct.elev', 'parcel')) %>% 
  mutate(year = year(DATE2))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- alpha diversity ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## bootstrap example
# set.seed(123)
# 
# df.bootstraped <- 
# tibble(replicates = 1:9) %>% 
#   mutate(
#     samples = 
#       map(replicates, 
#           ~db_07 %>% 
#             nest(data = -c(group, fct.elev, parcel)) %>% 
#             group_by(group, fct.elev) %>% 
#             slice_sample(n = 3)),
#     plots.agreggated = 3) %>% 
#   unnest(c(samples)) 
  # unnest(c(data)) %>% 
  # nest(data = -c(replicates, fct.elev, group)) %>% 
  # mutate(alpha = 
  #          map(data, 
  #              ~.x %>% 
  #                {table(.$GEN)} %>% 
  #                renyi(hill = T) %>% 
  #                .[c('0', '1', '2')] %>% 
  #                matrix(nrow = 1) %>% 
  #                `colnames<-`( c('richness', 'shannon', 'simpson')) %>% 
  #                as.data.frame()
  #              
  #          ))

# df.bootstraped %>%
#   select(-data) %>% view()
  # unnest(alpha) %>% 
  # select(-data)

# eg obtain diversity
# df.bootstraped[[4]][[3]] %>% 
#   {table(.$GEN)} %>% 
#   renyi(hill = T) %>% 
#   .[c('0', '1', '2')] %>% 
#   matrix(nrow = 1) %>% 
#   `colnames<-`( c('richness', 'shannon', 'simpson')) %>% 
#   as.data.frame()
# 
# df.bootstraped %>% 
#   count(fct.elev)
  
## iterate bootstrap 
alpha.bootstrap <- function(plots.agreggated, replicates = 9) {
  
  replicates.x <- seq(1, replicates)
  
  set.seed(123)
  tibble(replicates = replicates.x) %>% 
    mutate(
      samples = 
        map(replicates, 
            ~db_08 %>% 
              nest(data = -c(group, fct.elev, parcel)) %>% 
              group_by(group, fct.elev) %>% 
              slice_sample(n = plots.agreggated)),
      plots.agreggated = plots.agreggated) %>% 
    unnest(c(samples)) %>% 
    mutate(nrow = map_int(data, nrow)) %>% 
    unnest(c(data)) %>%
    nest(data = -c(plots.agreggated, replicates, fct.elev, group)) %>%
    mutate(alpha =
             map(data,
                 ~.x %>%
                   {table(.$GEN)} %>%
                   renyi(hill = T) %>%
                   .[c('0', '1', '2')] %>%
                   matrix(nrow = 1) %>%
                   `colnames<-`( c('richness', 'shannon', 'simpson')) %>%
                   as.data.frame()),
           bio.vars = 
             map(data, 
                 ~.x %>% 
                   summarise(across(starts_with('bio'),
                                    list(mean = mean))
                             )
                   )
           )
}

## Se debe aprovechar potencial de funciones de vegan que sirven a nivel de comunidad
# data(BCI)
# i <- sample(nrow(BCI), 12)
# mod <- renyi(BCI[i,])

## se deberia hacer con reemplazo bootstrap?
db.alpha <- 
map(1:10, alpha.bootstrap, 10)

# db.alpha[[2]][[7]][[1]]

db.alpha.01 <- 
db.alpha %>% 
  bind_rows() %>% 
  select(-data) %>% 
  unnest(c(alpha, bio.vars))

db.alpha %>% 
  bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Bootstrap for completing sample size ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Identify groups with less than 20 plots
plots.missing <- 
  db_08 %>% 
  count(group, fct.elev, parcel) %>% 
  count(group, fct.elev) %>% 
  arrange(fct.elev) %>% 
  filter(n < 20) %>% 
  mutate(plots.missing = 20-n) %>% 
  select(-n)

plots.bootstraped <-
  db_08 %>% 
  nest(df = -c(group, fct.elev, parcel)) %>% 
  semi_join(plots.missing, by = c('fct.elev', 'group')) %>% 
  nest(data = -c(fct.elev, group)) %>% 
  left_join(plots.missing) %>% 
  mutate(plots.bootstrap = 
           map2(data, plots.missing, 
                ~slice_sample(.x, n=.y, replace = T) )) %>% 
  select(-data) %>% 
  unnest(plots.bootstrap) %>% 
  mutate(plots.rows.bootstrap = 
           map(df, ~slice_sample(.x, n = nrow(.x), replace = T)),
         parcel = paste(parcel, 'boostraped', 1:n(), sep = '_')) %>% 
  select(-c(df, plots.missing))

db_08_bootstrap <- 
plots.bootstraped %>% 
  unnest(plots.rows.bootstrap) %>% 
  bind_rows(db_08)

## each group with at least 20 plots after bootstrap
db_08_bootstrap %>% 
  count(group, fct.elev, parcel) %>% 
  count(group, fct.elev)

db_08_bootstrap %>% 
  filter(is.na(bio_1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Beta diversity function ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## obtain random samples from each group
# eg <- 
# db_08_bootstrap %>% 
#   nest(data = -c(fct.elev, group, parcel)) %>% 
#   arrange(group, fct.elev) %>% 
#   group_by(fct.elev, group) %>% 
#   slice_sample(n = 2*2) %>% 
#   mutate(id.sample = 
#            rep(2:2, 1) %>% 
#            paste('sample', ., sep = '_'),
#          plots.agreggated = 2) %>% 
#   unnest(data) %>% 
#   nest(data = -c(group, fct.elev, id.sample, plots.agreggated)) %>% 
#   pivot_wider(names_from = id.sample, values_from = data)

## calculate beta diversity between sample_1 and sample_2 obtained
# g1.eg <- 
# eg[[4]][[1]] %>% 
#   {table(.$GEN)} %>% 
#   as.matrix() %>% 
#   as.data.frame() %>% 
#   rownames_to_column('species') %>% 
#   rename(site.1.n = 2)
# 
# g2.eg <- 
# eg[[4]][[5]] %>% 
#   {table(.$GEN)} %>% 
#   as.matrix() %>% 
#   as.data.frame()%>% 
#   rownames_to_column('species') %>% 
#   rename(site.2.n = 2)
# 
# left_join(g1.eg, g2.eg) %>% 
#   mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
#   pivot_longer(c(site.1.n, site.2.n)) %>% 
#   pivot_wider(names_from = species, values_from = value) %>% 
#   column_to_rownames('name') %>% 
#   betadiver(method = 'sor')

## beta diversity for one plot size
# eg.1 <- 
# eg %>% 
#   mutate(sample_1 = 
#            map(sample_1,
#                ~.x %>% 
#                  {table(.$GEN)} %>% 
#                  as.matrix() %>% 
#                  as.data.frame() %>% 
#                  rownames_to_column('species') %>% 
#                  rename(site.1.n = 2)),
#          sample_2 = 
#            map(sample_2,
#                ~.x %>% 
#                  {table(.$GEN)} %>% 
#                  as.matrix() %>% 
#                  as.data.frame() %>% 
#                  rownames_to_column('species') %>% 
#                  rename(site.2.n = 2)),
#          beta = 
#            map2(sample_1, sample_2,
#                 ~left_join(.x, .y) %>% 
#                   mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
#                   pivot_longer(c(site.1.n, site.2.n)) %>% 
#                   pivot_wider(names_from = species, values_from = value) %>% 
#                   column_to_rownames('name') %>% 
#                   betadiver(method = 'sor')),
#          beta = 
#            map_dbl(beta, 
#                ~as.matrix(.x) %>% .[1,2])
#          )

## final beta function with bootstrap and different plot size
beta.bootstrap <- function(plots.agreggated, replicates = 10, beta.index = 'sor') {

  set.seed(123)
  x.replicates <- seq(1, replicates)
    
  df.samples <- 
  tibble(replicates = x.replicates) %>% 
    mutate(
      samples = 
        map(replicates, 
            ~db_08_bootstrap %>% 
              nest(data = -c(group, fct.elev, parcel)) %>% 
              arrange(group, fct.elev) %>% 
              group_by(fct.elev, group) %>% 
              slice_sample(n = plots.agreggated*2) %>% 
              mutate(id.sample =
                       rep(1:2, plots.agreggated) %>%
                       paste('sample', ., sep = '_'),
                     plots.agreggated = plots.agreggated) %>% 
              unnest(data) %>%
              ungroup() %>% 
              nest(data = -c(group, fct.elev, plots.agreggated, id.sample)) %>%
              pivot_wider(names_from = id.sample, values_from = data))
    )

  mutate(df.samples,
         df.beta =
           map(samples,
               ~mutate(.x,
                       bio.vars.sample_1 =
                         map(sample_1,
                             ~.x %>%
                               distinct(parcel, .keep_all = T) %>% 
                               summarise(across(starts_with('bio'),
                                                list(mean = mean))) %>%
                               as.matrix()),
                       bio.vars.sample_2 =
                         map(sample_2,
                             ~.x %>%
                               distinct(parcel, .keep_all = T) %>% 
                               summarise(across(starts_with('bio'),
                                                list(mean = mean))) %>%
                               as.matrix()),
                       bio.vars =
                         map2(bio.vars.sample_1, bio.vars.sample_2,
                              ~.x - .y) %>% 
                         map(as.data.frame),
                       sample_1 =
                         map(sample_1,
                             ~.x %>%
                               {table(.$GEN)} %>%
                               as.matrix() %>%
                               as.data.frame() %>%
                               rownames_to_column('species') %>%
                               rename(site.1.n = 2)),
                       sample_2 =
                         map(sample_2,
                             ~.x %>%
                               {table(.$GEN)} %>%
                               as.matrix() %>%
                               as.data.frame() %>%
                               rownames_to_column('species') %>%
                               rename(site.2.n = 2)),
                       beta =
                         map2(sample_1, sample_2,
                              ~left_join(.x, .y, by = 'species') %>%
                                mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
                                pivot_longer(c(site.1.n, site.2.n)) %>%
                                pivot_wider(names_from = species, values_from = value) %>%
                                column_to_rownames('name') %>%
                                betadiver(method = beta.index)),
                       beta =
                         map_dbl(beta,
                                 ~as.matrix(.x) %>% .[1,2])
               )))
}

## Beta diversity
db.beta <- map(1:10, beta.bootstrap, replicates = 20)


## worldclim biovars are displayed as difference between pairs of plots
db.beta.01 <- 
bind_rows(db.beta) %>% 
  select(-samples) %>% 
  unnest((df.beta)) %>% 
  select(-c(sample_1, sample_2, bio.vars.sample_1, bio.vars.sample_2)) %>% 
  unnest(bio.vars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- spatial analysis of beta ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
beta.dist.plots <-
db_08_bootstrap %>% 
  mutate(fct.elev = 
           chop_evenly(elevation, 3)) %>% 
  # count(fct.elev)
  count(fct.elev, parcel, GEN) %>% 
  nest(df.species = -fct.elev) %>% 
  mutate(beta = 
           map(df.species, 
               ~.x %>% 
                 pivot_wider(names_from = GEN, 
                             values_from = n,
                             values_fill = 0) %>% 
                 column_to_rownames('parcel') %>% 
                 betadiver(method = 'sor') %>% 
                 as.matrix() %>% 
                 as.data.frame() %>% 
                 rownames_to_column('parcel') %>% 
                 tibble() %>% 
                 pivot_longer(-parcel, values_to = 'beta') %>% 
                 filter(parcel < name)
                 
                 )
           )

# beta.dist.plots[[3]][[2]]
# 
# beta.dist.plots %>% 
#   select(-df.species) %>% 
#   unnest()

geo.dist.plots <-
  db_08_bootstrap %>% 
  mutate(fct.elev = 
           chop_evenly(elevation, 3)) %>% 
  distinct(fct.elev, group, parcel, x.utm, y.utm) %>% 
  nest(coords = -c(fct.elev)) %>% 
  mutate(distance = 
           map(coords, 
               ~.x %>% 
                 column_to_rownames('parcel') %>% 
                 as.matrix() %>% 
                 dist() %>% 
                 as.matrix() %>% 
                 as.data.frame() %>% 
                 rownames_to_column('parcel') %>% 
                 tibble() %>% 
                 pivot_longer(-parcel, values_to = 'distance') %>% 
                 filter(parcel < name)
                 )
           )
  
geo.dist.plots[[2]][[3]] 

## distribution of distances between plots; cut point
geo.dist.plots %>% 
  select(-coords) %>% 
  unnest() %>%
  .$distance %>% 
  hist()

## db of beta and geographic distance between plots
db.dist.plots <- 
geo.dist.plots %>%
  select(-coords) %>% 
  unnest() %>% 
  left_join(beta.dist.plots %>% 
              select(-df.species) %>% 
              unnest(),
            by = c('fct.elev', 'parcel', 'name'))

db.dist.plots %>% 
  filter(distance < 5e3) %>% 
  mutate(fct.dist = chop_evenly(distance, 40)) %>% 
  group_by(fct.elev, fct.dist) %>%
  mutate(across(beta,
                   list(mean = mean, sd = sd))) %>%
  ungroup() %>% 
  ggplot() +
  geom_boxplot(aes(fct.dist, beta)) + 
  geom_smooth(data = ~distinct(., fct.elev, fct.dist, beta_mean),
              aes(as.integer(fct.dist), beta_mean),
              method = 'lm') +
  facet_wrap(~fct.elev) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Turnover and dominance over time ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(codyn)

## create clusters of plots; each group is a replicate for turnover and dominance
## calculations
db.turnover.clustering <- 
db_08 %>% 
  filter(!is.na(DATE2),
         group == 7) %>% 
  mutate(fct.elev = 
           chop_pretty(elevation, 2),
         year = year(DATE2)) %>% 
  # distinct(x.utm, y.utm, elevation)
  distinct(NUM, LOC, .keep_all = T) %>% 
  select(NUM, LOC, x.utm, y.utm, elevation)


## detect outlier plots with dbscan
turnover.dbscan.outlier <- 
  db.turnover.clustering %>% 
  select(x.utm, y.utm, elevation) %>% 
  as.matrix() %>% 
  hdbscan(minPts = 7)

# qgis view
db.turnover.clustering %>% 
  mutate(group = turnover.dbscan.outlier$cluster,
         x = x.utm, y = y.utm) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 32717) %>% 
  st_write('intermedium/turnover_clusters.gpkg', append =F)

## filter outlier plots
db.turnover.clustering.01 <- 
db.turnover.clustering %>% 
  mutate(group = turnover.dbscan.outlier$cluster) %>% 
  filter(group == 2)

## final cluster with kmeans
set.seed(1111)
kmeans.clusters.turnover <- 
  db.turnover.clustering.01 %>% 
  select(x.utm, y.utm, elevation) %>% 
  as.matrix() %>% 
  kmeans(20, nstart = 60)

# qgis view
db.turnover.clustering.01 %>% 
  mutate(x = x.utm, y = y.utm,
         group = kmeans.clusters.turnover$cluster) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 32717) %>% 
  st_write('intermedium/turnover_clusters_01.gpkg', append =F)

## filter occ from raw db
db.turnover.clustering.02 <-
db.turnover.clustering.01 %>% 
  mutate(group = kmeans.clusters.turnover$cluster) %>% 
  select(NUM, LOC, group) %>% 
  group_by(group) %>% 
  filter(length(LOC) > 9) %>% 
  ungroup()

db.turnover.clustering.01 %>% 
  count(group, sort = T)

## turnover analysis
turnover.df <- 
db_08 %>% 
  semi_join(db.turnover.clustering.02, 
            by = c('NUM', 'LOC')) %>% 
  rows_update(db.turnover.clustering.02,
              by = c('NUM', 'LOC')) %>% 
  filter(!is.na(year)) %>% 
  count(group, year, GEN)

turnover.df %>% count(group)

turnover.result <-
c('total', 'appearance', 'disappearance') %>% 
  map(
    ~turnover( 
      turnover.df,
      time.var = 'year', 
      species.var = 'GEN', 
      abundance.var = 'n',
      replicate.var = 'group',
      metric = .x)
  ) %>% 
  reduce(left_join, by = c('group', 'year')) %>% 
  pivot_longer(-c(year, group))
   
   
(turnover.plot <- 
  turnover.result %>% 
  ggplot(aes(year, value, color = name, fill = name)) +
  geom_point() +
  geom_smooth() +
  # geom_line() +
  theme_bw() +
  labs(y = 'Turnover'))

## rank abundance shift across time
rank.shift.result <- 
rank_shift( 
  turnover.df,
  time.var = 'year', 
  species.var = 'GEN', 
  abundance.var = 'n',
  replicate.var = 'group')

(rank.shift.plot <- 
rank.shift.result %>% 
  mutate(year = str_extract(year_pair, '^\\d+')) %>% 
  ggplot(aes(year, MRS, group = 1)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Mean Rank-Abundance Shift') +
  theme_bw())

## rate change
rate.change.result <- 
rate_change_interval( 
  turnover.df,
  time.var = 'year', 
  species.var = 'GEN', 
  abundance.var = 'n',
  replicate.var = 'group')

(rate.chage.plot <- 
rate.change.result %>% 
  ggplot(aes(interval, distance)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Time interval', y = 'Euclidean distance between comunities') +
  theme_bw())

## dominance rank clock
(dominance.clock.plot <-  
turnover.df %>% 
  group_by(year) %>% 
  filter(rank(n*-1) < 4) %>% 
  ggplot() +
  geom_line(aes(year, n, color = GEN), linewidth = 2) +
  coord_polar() +
  theme_bw())
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ALPHA
(area.alpha.general.plot <- 
db.alpha.01 %>% 
  filter(group != 17) %>%
  # pivot_longer(c(richness, shannon, simpson)) %>% 
  ggplot(aes(plots.agreggated*25, shannon)) +
  geom_point() +
  geom_smooth() +
  # facet_wrap(vars(fct.elev) ) +
  labs(x = 'area m2'))

(area.alpha.elev.plot <- 
db.alpha.01 %>% 
  filter(group != 17) %>%
  # pivot_longer(c(richness, shannon, simpson)) %>% 
  ggplot(aes(plots.agreggated*25, shannon)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(fct.elev) ) +
  labs(x = 'area m2') +
  theme_bw())


## Multiple correlations between alpha and bio.vars worldclim
pacman::p_load(GGally)
db.alpha.01 %>% 
  select(shannon, starts_with('bio')) %>% 
  GGally::ggcorr()

## Important variables:
## bio_8_mean (Mean Temperature of Wettest Quarter),
## bio_12_mean (anual precipitation), 
## bio_15_mean (prepitation seasonality)
## bio_16_mean (precipitation of warmest quarter)
(alfa.biovar.elev.plot <- 
db.alpha.01 %>% 
  mutate(fct.plots.agreggated = 
           chop_evenly(plots.agreggated, 3)) %>% 
  ggplot(aes(bio_8_mean, shannon, color = fct.plots.agreggated)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(fct.elev), scales = 'free' ) +
  labs(x = 'Mean Temperature of Wettest Quarter', color = 'Plots agreggated'))


## possible important variables bio_6, bio_8_mean, bio_12_mean, bio_15_mean,
## bio_16_mean, bio_19_mean
## En general, donde es mas humedo hay mas riqueza de especies, asi como 
## donde es más cálido
(alfa.biovar.elev.plot.02 <-
db.alpha.01 %>% 
  filter(group != 17) %>%
  mutate(fct.bio =
           chop_evenly(bio_19_mean, 3)) %>%
  # count(fct.plots.agreggated) %>% 
  ggplot(aes(plots.agreggated, shannon, color = fct.bio)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(fct.elev), scales = 'free' ) +
  labs(color = 'Precipitation of Coldest Quarter') +
  theme_bw())


## doble tendencia en tierras bajas (392. 1022) se da en groups de Lyarina
## y Jatunsacha (15 y 17). EN Jatun Sacha la diversidad es menor (15),
## y en Lyiarina la diversidad alpha es muy alta (17)
db_08 %>% 
  count(fct.elev, group, parcel, GEN) %>% 
  group_by(fct.elev, group) %>% 
  summarise(n.spp = 
              length(unique(GEN)))

## BETA
(beta.area.general.plot <- 
db.beta.01 %>% 
  ggplot(aes(plots.agreggated*25, 1-beta)) +
  geom_point() +
  geom_smooth() +
  # facet_wrap(~fct.elev) +
  labs(x = 'area m2'))

(beta.area.elev.plot <- 
db.beta.01 %>% 
  ggplot(aes(plots.agreggated*25, 1-beta)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~fct.elev) +
  labs(x = 'area m2'))

(beta.biovar.elev.plot <- 
db.beta.01 %>% 
  filter(group != 17) %>%
  mutate(fct.plots.agreggated = 
           chop_evenly(plots.agreggated, 2)) %>% 
  ggplot(aes(abs(bio_8_mean), beta, color = fct.plots.agreggated)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(fct.elev), scales = 'free') +
  labs(x = 'Absolute difference in Mean Temperature of Wettest Quarter',
       color = 'Plots agreggated'))


## ALPHA & BETA
coeff <- 13

(alfa.beta.area.plot <- 
db.alpha.01 %>% 
  filter(group != 17) %>% 
  left_join(db.beta.01, 
            by = c('replicates', 'fct.elev', 'group', 'plots.agreggated')) %>% 
  mutate(beta = 1-beta) %>% 
  ggplot(aes(plots.agreggated*25)) +
  geom_point(aes(y = shannon/coeff)) +
  geom_point(aes(y = beta)) +
  geom_smooth(aes(y = shannon/coeff, fill = 'shannon'), 
              color = 'blue') +
  scale_fill_manual(values = 'blue',
                    guide = guide_legend(title = NULL)) +
  ggnewscale::new_scale_fill() +
  
  geom_smooth(aes(y = beta, fill = 'beta'), 
              color = 'green') +
  scale_fill_manual(values = 'green',
                    guide = guide_legend(title = NULL)) +
  ggnewscale::new_scale_fill() +
  
  scale_y_continuous(name = 'beta', 
                     sec.axis = sec_axis(trans = ~.*coeff, name = 'shannon')) +
  facet_wrap(~fct.elev) +
  labs(x = 'Area m2') +
  theme_bw())


# db.alpha.01 %>%
#   filter(group != 17) %>%
#   left_join(db.beta.01,
#             by = c('replicates', 'fct.elev', 'group', 'plots.agreggated')) %>%
#   mutate(beta = 1-beta) %>%
#   ggplot(aes(shannon, beta)) +
#   geom_point() +
#   geom_smooth() +
#   facet_wrap(~fct.elev, scales = 'free')



rmarkdown::render("propuesta_investigacion.Rmd")




