# setup -------------------------------------------------------------------

pacman::p_load(tidyverse, vegan, readxl, writexl, 
               santoku, sf, sp, rgbif, rnaturalearth,
               rnaturalearthdata)



# clean data ---------------------------------------------------------------
db_plots <- readxl::read_xls("data/base_actual_plots2023.xls")

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

db_plots01 <-   
db_plots %>% 
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
db_plots02 <- 
db_plots01 %>% 
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
           str_replace(CLIMATE, ' ', '_')
         )


db_plots03 <- 
  db_plots02 %>%
  group_by(LOC, NUM, GEN) %>% 
  mutate(n_leaf = list(`L-LEAF`)) %>% 
  ungroup() %>% 
  distinct(LOC, NUM, GEN, .keep_all = T) %>% 
  mutate(
    across(
      n_leaf,
      list(mean = ~map_dbl(.x, mean), 
           abundance = ~map_dbl(.x, length), 
           min = ~map_dbl(.x, min),
           max = ~map_dbl(.x, max) ) ) ) %>%
  rename(abundance = n_leaf_abundance)

## diversity db
db_plots04 <- 
  db_plots03 %>% 
  group_by(LOC, NUM) %>% 
  mutate(GEN2 = list(GEN),
         abundance2 = list(abundance),
         alpha = map_dbl(abundance2, ~.x %>% unlist() %>% diversity),
         richness = length(unique(GEN)),
         elevm2 = parse_number(elevm),
         year = lubridate::year(DATE2),
         month = month(DATE2)
  ) %>% 
  ungroup()


# clean coordinates -------------------------------------------------------

manual_rev_coords <- 
read_csv('export/manual_clean_coord.csv') %>%
  filter(manual_rev == 'yes') %>% 
  select(lat = original_lat, long = original_long)
# 
# db_plots04 %>%
#   distinct(lat, long) %>% 
#   mutate(id_coord = 1:n()) %>% 
#   anti_join(manual_rev_coords)

decimal_coords <- 
db_plots04 %>%
  distinct(lat, long) %>% 
  mutate(id_coord = 1:n()) %>% 
  bind_rows(manual_rev_coords, .) %>% 
  # distinct(id_coord, .keep_all = T) %>% 
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
    lat2 = sp::char2dms(lat, '°', '\'') %>% as.double(),
    long2 = sp::char2dms(long, '°', '\'') %>% as.double()
  ) %>% 
  select(!c(lat, long))

utm_coords <- 
db_plots04 %>% 
  distinct(lat, long) %>% 
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
coords01 <- 
coords00 %>% 
  mutate(X = long2, Y = lat2) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4326) %>% 
  st_intersection(ec) %>% 
  st_drop_geometry() %>% 
  select(original_lat, original_long, long2, lat2)

# ggplot() +
#   geom_sf(data = ec) +
#   geom_sf(data = coords01)+
#   coord_sf(xlim = c(-81, -75), ylim = c(1, -4))
  
# plot diversidad alpha ---------------------------------------------------

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_point(aes(elevm2, alpha)) +
  geom_smooth(aes(elevm2, alpha))

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_boxplot(aes(chop_pretty(elevm2, 10), alpha)) +
  geom_smooth(aes(chop_pretty(elevm2, 10), alpha))


db_plots04 %>% 
  filter(richness < 16) %>%
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_point(aes(elevm2, richness)) +
  geom_smooth(aes(elevm2, richness))
  
db_plots04 %>% 
  filter(richness < 16) %>%
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_boxplot(aes(chop_pretty(elevm2, 10), richness)) 

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_point(aes(richness, alpha))

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot()+
  geom_boxplot(aes(FOREST, alpha))

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot()+
  geom_boxplot(aes(fct_reorder(FOREST, alpha), alpha))


db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot()+
  geom_boxplot(aes(fct_reorder(FOREST, alpha), alpha)) 

db_plots04 %>% 
  distinct(LOC, NUM,.keep_all = T) %>%
  ggplot() +
  geom_boxplot(aes(CLIMATE, alpha))

db_plots04 %>% names()

# plot hojas --------------------------------------------------------------

db_plots04 %>% 
    filter(n_leaf_mean < 600) %>% 
    ggplot() +
    geom_point(aes(cut(log(n_leaf_mean), 1:7), alpha ))+
    facet_wrap(~FOREST)
  
db_plots04 %>% 
  filter(n_leaf_mean < 600) %>% 
  ggplot() +
    geom_point(aes(log(n_leaf_mean), alpha ))
  
db_plots04 %>% 
  # select(n_leaf_mean) %>% 
  mutate(n_leaf_mean_log = log(n_leaf_mean)) %>% 
  filter(n_leaf_mean < 600 & n_leaf_mean_log > 0) %>% 
  ggplot() +
  geom_boxplot(aes(cut(alpha, 20), n_leaf_mean_log ))

db_plots04 %>% 
  # select(n_leaf_mean) %>% 
  mutate(n_leaf_mean_log = log(n_leaf_mean)) %>% 
  filter(n_leaf_mean < 600 & n_leaf_mean_log > 0) %>% 
  ggplot() +
  geom_boxplot(aes(cut(richness, 20), n_leaf_mean_log ))

db_plots04 %>% 
  # select(n_leaf_mean) %>% 
  mutate(elevm_fct = chop_pretty(elevm2, 4)) %>% 
  mutate(n_leaf_mean_log = log(n_leaf_mean)) %>% 
  filter(n_leaf_mean < 600 & n_leaf_mean_log > 0) %>% 
  ggplot() +
  geom_boxplot(aes(cut(richness, 20), n_leaf_mean_log )) +
  facet_wrap(~elevm_fct)

db_plots04 %>% 
  # select(n_leaf_mean) %>% 
  mutate(elevm_fct = chop_pretty(elevm2, 4)) %>% 
  mutate(n_leaf_mean_log = log(n_leaf_mean)) %>% 
  filter(n_leaf_mean < 600 & n_leaf_mean_log > 0) %>% 
  ggplot() +
  geom_boxplot(aes(cut(richness, 20), n_leaf_mean_log ))


db_plots04 %>% 
  # select(n_leaf_mean) %>% 
  mutate(n_leaf_mean_log = log(n_leaf_mean)) %>% 
  filter(n_leaf_mean < 600 & n_leaf_mean_log > 0) %>% 
  ggplot() +
  geom_boxplot(aes(chop_pretty(elevm2, 10), n_leaf_mean_log))


db_plots04 %>% 
  filter(n_leaf_mean < 400) %>% 
  ggplot() +
  geom_histogram(aes(n_leaf_mean), bins=100)

db_plots04 %>% 
  filter(n_leaf_mean < 400) %>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(FOREST, n_leaf_mean), log(n_leaf_mean)))

db_plots04 %>% 
  filter(n_leaf_mean < 400) %>% 
  ggplot() +
  geom_boxplot(aes(year, group = year, log(n_leaf_mean)))

db_plots04 %>% 
  filter(n_leaf_mean < 400) %>% 
  ggplot() +
  geom_boxplot(aes(month, group = month, log(n_leaf_mean)))

db_plots04 %>% 
  filter(n_leaf_mean < 400) %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), log(n_leaf_mean), fill = FOREST))


# abundance

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(chop_pretty(elevm2, 10), log10(abundance)))

# date analysis ----------------------------------------------------------
db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(year, alpha, group = year))

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), alpha))

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), richness))

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), log2(abundance)))

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), log2(abundance)))+
  facet_wrap(~year)

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(month), log2(abundance), fill = FOREST))

db_plots04 %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(year), alpha, fill = FOREST))


# diferentes parcelas, no se repite casi ninguna en el tiempo de analisis
db_plots04 %>% 
  group_by(NUM) %>% 
  summarise(n = length(unique(year))) %>% 
  view()


# beta diversity ----------------------------------------------------------

# se dejo los registros con menos de 50 spp porque son atipicos y 
# tienen mucho impacto en el pca

data_beta <- 
db_plots04 %>% 
  filter(abundance < 50) %>% 
  select(LOC, NUM, GEN, abundance) %>% 
  unite(LOC, NUM, col = 'loc') %>% 
  pivot_wider(names_from = 'GEN', values_from = 'abundance', values_fill = 0) %>% 
  column_to_rownames('loc')

beta_matrix <- 
betadiver(data_beta %>% as.matrix(), method = 'w')

pca_beta <- 
  data_beta %>% 
  mutate(across(everything(), ~.x-mean(.x)/sd(.x))) %>% 
  prcomp()

var_add_pca <- 
db_plots04 %>% 
  filter(abundance < 50) %>% 
  mutate(elevm_fct=
           chop_pretty(elevm2, 5),
         coord_by = c()) %>% 
  select(LOC, NUM, FOREST, GEN, abundance, elevm_fct) %>% 
  unite(LOC, NUM, col = 'loc') %>% 
  pivot_wider(names_from = 'GEN', values_from = 'abundance', values_fill = 0) %>% 
  distinct(loc, .keep_all = T) %>% 
  select(loc, FOREST, elevm_fct)


summary(pca_beta)

pca_beta$x %>%
  as.data.frame() %>% 
  tibble() %>% 
  bind_cols(var_add_pca, .) %>% 
  ggplot() +
  geom_point(aes(PC1, PC2, color = elevm_fct), size = 2) +
  scale_color_brewer(type  = 'qual')



plot(hclust(beta_matrix)
)

library(RColorBrewer)

beta_matrix %>% 
  as.matrix() %>% 
  heatmap(col= colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))(25))
  
  # as.data.frame() %>% 
  # rownames_to_column('plot') %>% 
  # tibble() %>% 
  # pivot_longer(-plot) %>% 
  # ggplot() +
  # geom_raster(aes(plot, name, fill = value))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- obtener escalas ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_plots05 <- 
db_plots02 %>% 
  mutate(elevation = parse_number(elevm),
         factor_elev = chop_pretty(elevation, 5)) %>% 
  left_join(coords01, by = c('lat' = 'original_lat', 'long' = 'original_long')) %>% 
  filter(!is.na(long2))
  # st_as_sf(coords = c('long2', 'lat2'), crs = 4326) 
  
# 
# bbox <-
#   st_bbox(db_plots05) %>% 
#   st_as_sfc()
# 
# cuadricula <- 
#   map(seq(20, ))
#   st_make_grid(bbox, cellsize = 10)

scale_db <- function(db, xscale = 1){
db %>%
  distinct(NUM, .keep_all = T) %>% 
  group_by(factor_elev) %>% 
  mutate(nrow = n(),
         n_row_elev = n()%/%xscale,
         n_row_needed = xscale*n_row_elev,
         dif_row = nrow - n_row_needed,
         eg = n_row_elev-floor(n_row_elev) == 0) %>% 
  ungroup() %>% 
  nest(.by=c(factor_elev, nrow, n_row_elev, n_row_needed, dif_row)) %>% 
  mutate(new_id = map2(n_row_elev, dif_row,
                       ~c(rep(1:.x, each=xscale),
                        rep(.x+1, .y)
                       ))) %>% 
    unnest()
    }

list_db_scales <- 
map(1:8, ~scale_db(db_plots05, .x))


db_plots05 %>% left_join(
list_db_scales[[2]] %>% 
  select(NUM, new_id)) %>% 
  group_by(factor_elev, new_id, GEN) %>% 
  count() %>% view()
  

# sudta articulo




  # unnest() %>% view()
  # mutate(data = 
  #          if_else(!eg,
  #                  map(data, ~.x  %>% .[-1,]),
  #                  data)) %>% 
  # unnest() %>% 
  # group_by(factor_elev) %>% 
  # mutate(new_id = )
  
  
         # eg = n_row_elev-floor(n_row_elev) == 0,
         # new_id = 
         #   if_else(eg,
         #           c(rep(1:n_row_elev, each = 2)),
         #           c(rep(1:n_row_elev, each = 2), 1))
         # new_id_num = 
         #   if_else((n_row_elev-ceiling(n_row_elev)) == 0,
         #           rep(1:ceiling(n_row_elev), 2),
         #           rep(1:ceiling(n_row_elev), 2) %>% .[-c(1,2)]

  view()

floor()
 
1:4 %>% 
  rep(each = 2)



db_plots05 %>% 
  ggplot() +
  geom_histogram(aes(elevation))

# jackknife estabilizar muestra
# script de ventanas y random forest con datos climaticos, elevacion
# modelos con PC!
