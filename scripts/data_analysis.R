#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- load data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source("scripts/scale.R")
rmarkdown::render("final_report/informe_final.Rmd")
                  
pacman::p_load(modelsummary, tinytable, MASS, car, VGAM, recipes, sf, lme4, caret)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Database final depurations ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(123)
db_alpha <- 
  db.alpha.01 %>% 
  filter(group != 17) %>%
  set_names(names(.) %>% str_replace('\\.', '_') %>% str_remove('_mean')) %>% 
  mutate(fct_plots =
           chop_evenly(plots_agreggated, 3),
         escala = plots_agreggated*25,
         # group = as.factor(group),
         fct_escala =
           chop_evenly(escala, 3),
         fct_elev=fct_recode(fct_elev, 
                             '400' = '[392, 1022)',
                             '1000' = '[1022, 1651)',
                             '1600' = '[1651, 2281)',
                             '2300' = '[2281, 2911)',
                             '2900' = '[2911, 3540)',
                             '3500' = '[3540, 4170]')
         ) %>% 
  group_by(fct_elev) %>% 
  slice_sample(n = 300) %>% 
  ungroup()

db_alpha_02 <- 
  db_alpha %>% 
  mutate(across(where(is.numeric),
                ~scale(.x) %>% as.vector()))

set.seed(123)
db_beta <- db.beta.01 %>% 
  set_names(names(.) %>% str_replace('\\.', '_') %>% str_remove('_mean')) %>% 
  filter(group != 17) %>%
  mutate(beta = 1- beta,
         beta_yeo = yeo.johnson(beta, -2.7),
         fct_plots =
           chop_evenly(plots_agreggated, 3),
         escala = plots_agreggated*25,
         fct_escala =
           chop_evenly(escala, 3),
         fct_elev=fct_recode(fct_elev, 
                             '400' = '[392, 1022)',
                             '1000' = '[1022, 1651)',
                             '1600' = '[1651, 2281)',
                             '2300' = '[2281, 2911)',
                             '2900' = '[2911, 3540)',
                             '3500' = '[3540, 4170]'
         )) %>% 
  filter(beta != 0) %>% 
  group_by(fct_elev) %>% 
  slice_sample(n = 300) %>% 
  ungroup()

db_beta_02 <- 
  db_beta %>% 
  mutate(across(where(is.numeric),
                ~scale(.x) %>% as.vector()))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- multiple correlations ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Multiple correlations between alpha and bio_vars worldclim
pacman::p_load(GGally)

(multiple_corr_alpha <- 
  db_alpha %>% 
  dplyr::select(shannon, simpson, richness, escala, starts_with('bio')) %>% 
  GGally::ggcorr(label = TRUE))

(multiple_corr_beta <- 
    db_beta %>% 
    dplyr::select(beta_yeo, escala, starts_with('bio')) %>% 
    GGally::ggcorr(label = TRUE))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- hipotesis 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Alpha diversidad
# Colocar en texto principal
(area_alpha_elev_plot <- 
    db_alpha %>%
   ggplot(aes(escala, shannon, fill = fct_elev, color = fct_elev)) +
   geom_point(alpha = 0.6, show.legend = F) +
   geom_smooth(method = 'lm', alpha = 0.1, show.legend = F) +
   labs(x = expression(paste("Área ", 'm'^2)), y = 'Diversidad Alpha (Shannon)', color = 'Elevación', fill = 'Elevación') +
   facet_wrap(vars(fct_elev) ) +
   theme_bw())

# Colocar en anexos este grafico para ejemplicar mayor riqueza a mayor altitud
(alfa_biovar_elev_plot_02 <-
    db_alpha %>%
    ggplot() +
    geom_boxplot(aes(fct_elev, shannon, color = fct_plots)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Alpha (Shannon)') +
    theme_bw())

# Anova para representar este grafico
(alfa_biovar_elev_plot_03 <-
    db_alpha %>%
    ggplot() +
    geom_boxplot(aes(fct_elev, shannon)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Alpha (Shannon)') +
    theme_bw())

## modelos
# diversidad alpha vs altitud
diff_alpha_elev <- aov(shannon ~ fct_elev, data = db_alpha)
posthoc_diff_alpha_elev <- TukeyHSD(diff_alpha_elev)

diff_alpha_elev <- lm(yeo.johnson(shannon, 2) ~ fct_elev, data = db_alpha)
diff_alpha_elev %>% summary()


# diversidad alpha vs altitud y escala de análisis
alpha_vs_elev_grain <- lm(shannon ~ fct_elev + plots_agreggated + fct_elev:plots_agreggated, data = db_alpha)
alpha_vs_elev_grain %>% summary()

## Beta diversidad
# colocar en escrito principal
(area_beta_elev_plot <- 
    db_beta %>%
    ggplot(aes(escala, beta_yeo, fill = fct_elev, color = fct_elev)) +
    geom_point(alpha = 0.6, show.legend = F) +
    geom_smooth(method = 'lm', alpha = 0.1, show.legend = F) +
    labs(x = expression(paste("Área ", 'm'^2)), y = 'Diversidad Beta (Sorensen)', color = 'Elevación', fill = 'Elevación') +
    facet_wrap(vars(fct_elev) ) +
    # scale_y_continuous(limits = c(0_60, 1)) +
    theme_bw())

# ejemplicar variación en beta según rango altitudinal
(beta_biovar_elev_plot_02 <-
    db_beta %>%
    ggplot() +
    geom_boxplot(aes(fct_elev, beta_yeo, color = fct_plots)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.4)) +
    theme_bw())

# resultados generales de beta diversidad según el rango altitudinal
(beta_biovar_elev_plot_03 <-
    db_beta %>%
    ggplot() +
    geom_boxplot(aes(fct_elev, beta_yeo)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.35)) +
    theme_bw())

## modelos
# diversidad beta vs altitud
diff_beta_elev <- aov(yeo.johnson(beta, -2.8) ~ fct_elev, data = db_beta)
posthoc_diff_beta_elev <- TukeyHSD(diff_beta_elev)

diff_beta_elev <- lm(beta_yeo ~ fct_elev, data = db_beta)
diff_beta_elev %>% summary()

ms_anova_alpha_beta_elev <- 
  modelsummary(list('Alfa'=diff_alpha_elev, 'Beta'=diff_beta_elev), statistic = NULL, estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
               gof_omit = 'AIC|BIC|Log|RMSE', output = 'flextable')

# diversidad beta vs altitud y escala de análisis
beta_vs_elev_grain <- lm(beta_yeo ~ fct_elev + plots_agreggated + fct_elev:plots_agreggated, data = db_beta)
beta_vs_elev_grain %>% summary()

# eg <-  recipe(beta ~ fct_elev + plots_agreggated, data = db_beta) |>
#   step_interact(terms = ~ fct_elev:plots_agreggated) %>%
#   step_YeoJohnson(beta) |>
#   prep()
# # -2.5 fue un parametro bueno para normalizar datos
# eg %>% tidy(2)

# tabla de ambos modelos para estimar alpha y beta diversidad en función de la elev y grain
ms_alpha_beta_elev_grain <- 
modelsummary(list('Alfa' =alpha_vs_elev_grain, 'Beta'=beta_vs_elev_grain), 
             # shape = model  ~ statistic + term,
             statistic = NULL, 
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE', 
             coef_omit = '^fct.+\\d+$',
             output = 'flextable')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- hipotesis 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alpha
(alpha_bio17 <-
   db_alpha %>% 
   ggplot(aes(bio_17, shannon, color = fct_escala, fill = fct_escala)) +
   geom_point() +
   geom_smooth(method = 'lm', alpha = 0.1) +
   facet_wrap(vars(fct_elev), scales = 'free' ) +
   labs(x = 'Precipitación (Precipitación del cuatrimestre más seco en mm)', 
        color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
        fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
        y = 'Diversidad alpha (Shannon)') +
   theme_bw() +
   theme(legend.position = 'bottom'))

(alpha_bio11 <-
    db_alpha %>% 
    ggplot(aes(bio_11, shannon, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Temperatura (Temperatura media de Cuatrimestre más frío C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad alpha (Shannon)') +
    theme_bw() +
    theme(legend.position = 'bottom'))

#beta
(beta_bio17 <-
    db_beta %>% 
    ggplot(aes(bio_17, beta_yeo, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta de Precipitación (Precipitación del cuatrimestre más seco en mm)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw() +
    theme(legend.position = 'bottom'))

(beta_bio11 <-
    db_beta %>% 
    ggplot(aes(bio_11, beta_yeo, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth( method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta Temperatura (Temperatura media de Cuatrimestre más frío C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw() +
    theme(legend.position = 'bottom'))

## modelos

# alpha
mod_alpha_bio <- lm(shannon ~ fct_elev + escala + bio_11 + bio_17 + 
                      bio_11:escala + bio_17:escala, 
                              data = db_alpha)

# beta
mod_beta_bio <- lm(beta_yeo ~ fct_elev + escala + bio_11 + bio_17 + 
                     bio_11:escala + bio_17:escala,
                   data = db_beta)
# unificación
(ms_alpha_beta_bio <- 
    modelsummary(list('Alfa' =mod_alpha_bio, 'Beta'=mod_beta_bio), 
                 fmt = fmt_sci(digits = 2),
                 statistic = NULL, 
                 estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
                 gof_omit = 'AIC|BIC|Log|RMSE', 
                 output = 'flextable'))


mod_beta_bio %>% summary()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Hipótesis 3 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alfa
models_alpha_elev_bio <- 
db_alpha %>% 
  nest(data = -fct_elev) %>% 
  arrange(fct_elev) %>% 
  mutate(
    models = map(data, ~lm(shannon ~ escala + bio_11 + bio_17 + bio_11:escala + bio_17:escala, data = .x)),
    )

ms_models_per_elev_alpha <- 
modelsummary(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), 
             statistic = NULL, 
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE', 
             output = 'flextable')

# Beta
models_beta_elev_bio <- 
  db_beta %>% 
  nest(data = -fct_elev) %>% 
  arrange(fct_elev) %>% 
  mutate(
    models = map(data, ~lm(beta_yeo ~ escala + bio_11 + bio_17 + bio_11:escala + bio_17:escala, data = .x)))

ms_models_per_elev_beta <- 
modelsummary(models_beta_elev_bio[[3]] %>% set_names(models_beta_elev_bio[[1]]), 
             statistic = NULL, 
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE',
             output = 'flextable') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- supuestos de modelos ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alpha vs altitud
par(mfcol=c(3,2))
diff_alpha_elev %>% plot()
diff_alpha_elev$residuals %>% hist(main = 'Histogram of Residual') 
# leveneTest(diff_alpha_elev)

# Beta vs altitud (actualizar en texto valores al elevar al cuadrado beta)
par(mfcol=c(3,2))
diff_beta_elev %>% plot()
diff_beta_elev$residuals %>% hist(main = 'Histogram of Residual')
# diff_beta_elev$residuals %>% shapiro.test()
# leveneTest(diff_beta_elev)

# Alpha vs elevacion y escala
par(mfcol=c(3,2))
alpha_vs_elev_grain %>% plot()
alpha_vs_elev_grain$residuals %>% hist(main = 'Histogram of Residual')
# alpha_vs_elev_grain$residuals %>% ks.test('rnorm')
car::vif(alpha_vs_elev_grain) %>%
  .[, "GVIF^(1/(2*Df))"] %>% 
  barplot(main = 'VIF')
  
# Beta vs elevacion y escala
par(mfcol=c(3,2))
beta_vs_elev_grain %>% plot()
beta_vs_elev_grain$residuals %>% hist(main = 'Histogram of Residual')
# beta_vs_elev_grain$residuals %>% ks.test('rnorm')
car::vif(beta_vs_elev_grain) %>% 
  .[, "GVIF^(1/(2*Df))"] %>% 
  barplot(main = 'VIF')

# Alpha vs elev, escala, y bio
par(mfcol=c(3,2))
mod_alpha_bio %>% plot()
mod_alpha_bio$residuals %>% hist(main = 'Histogram of Residual')
# mod_alpha_bio$residuals %>% ks.test('rnorm')
car::vif(mod_alpha_bio) %>% 
  .[, "GVIF^(1/(2*Df))"] %>% 
  barplot(main = 'VIF')

# Beta vs elev, escala, y bio
par(mfcol=c(3,2))
mod_beta_bio %>% plot()
mod_beta_bio$residuals %>% hist(main = 'Histogram of Residual')
# mod_beta_bio$residuals %>% ks.test('rnorm')
car::vif(mod_beta_bio) %>% 
  .[, "GVIF^(1/(2*Df))"] %>% 
  barplot(main = 'VIF')

# Alfa vs elev, escala, y bio a diferentes elevaciones


# Beta vs elev, escala, y bio a diferentes elevaciones

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Mapa de area de estudio ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(ggnewscale, ggspatial, elevatr, cowplot)

provincias <- st_read('C:/Users/rober/folder_mega/shape_files/provincias/nxprovincias.shp')

plots_coord <- 
db_08 %>% 
  distinct(x.utm, y.utm, group, fct.elev) %>% 
  semi_join(db_alpha, by = 'group') %>%
  mutate(x = x.utm, y = y.utm) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 32717) %>% 
  st_transform(4326)


db_08 %>%
  distinct(x.utm, y.utm, group, fct.elev, LOC) %>% 
  view()

  bbox_map <- 
plots_coord %>% 
  st_bbox() %>% 
  as.list()

bbox_rast <- 
terra::rast(xmin=bbox_map$xmin-0.4, xmax=bbox_map$xmax+0.4, 
            ymin=bbox_map$ymin, ymax=bbox_map$ymax)

dem  <- elevatr::get_elev_raster(bbox_rast, z = 9) %>% 
  rast()

slope <- terrain(dem, "slope", unit="radians")
aspect <- terrain(dem, "aspect", unit="radians")

hill_shade <- shade(slope, aspect, 40, 270)
hill_shade_df <- hill_shade %>% stretch() %>%  as.data.frame(xy = T) %>% tibble()
pal_hill <- grey.colors(5)


map_study_area <- 
ggplot() +
  
  geom_raster(data = hill_shade_df, aes(x, y, fill = hillshade),
              alpha = 0.5, show.legend = F) +
  scale_fill_gradientn(colours = pal_hill) +
  new_scale_fill() +
  
  geom_sf(data = provincias, fill = NA, color = 'black', size = 0.2) +
  
  geom_sf(data =  plots_coord, aes(fill = as.factor(group)),
          shape = 24, alpha = 0.85, stroke = 1.2, color = 'black') +
  scale_fill_brewer(type = 'qual', palette = 3, 
                     guide = guide_legend('Grupo')) + 
  new_scale_color() +
  
  # geom_sf_text(data =  provincias, aes(label = DPA_DESPRO)) +
  
  annotation_north_arrow(aes(location = 'tl')) +
  annotation_scale() +
  
  coord_sf(xlim = c(bbox_map$xmin-0.2, bbox_map$xmax+0.2), 
           ylim = c(bbox_map$ymin, bbox_map$ymax),
           crs = 4326) +
  
  labs(x = 'Longitud', y = 'Latitud') +
  theme_bw()

# mapa ecuador con contexto continental
bbox_ec <- list(xmin=-82.485, xmax=-73.960, ymin=-5.834, ymax=2.416)
bbox_ec_vect <- rast(xmin=bbox_ec$xmin, xmax=bbox_ec$xmax, 
                     ymin=bbox_ec$ymin, ymax=bbox_ec$ymax,
                     ncol = 1, nrow=1) %>% 
  terra::as.polygons() %>% 
  st_as_sf()

sf_use_s2(FALSE)
sudamerica <- read_sf("C:/Users/rober/folder_mega/shape_files/sudamerica/vc965bq8111.shp") %>% 
  st_crop(bbox_ec_vect) %>% 
  group_by(name) %>% 
  summarise() %>% 
  mutate(name = if_else(name == 'ECUADOR', NA, name))

sf_use_s2(TRUE)

napo_colored <- 
  provincias %>% 
  mutate(color=if_else(DPA_DESPRO=='NAPO', '1', NA))

map_ec <-
ggplot() +
  geom_sf(data = sudamerica, fill = 'white', color='black', size=0.2) +
  
  geom_sf_text(data = sudamerica, aes(label = name), size=2) +
  
  geom_sf(data=napo_colored, aes(fill=color), color='black', size=0.2,
          show.legend = F) +
  scale_fill_manual(values = 'grey', na.value = 'grey97') +
  new_scale_fill() +
  
  geom_sf(data =  plots_coord,
          shape = 3, alpha = 0.85, color = 'black') +
  
  coord_sf(expand = F, xlim = c(bbox_ec$xmin, bbox_ec$xmax),
           ylim = c(bbox_ec$ymin, bbox_ec$ymax)) +
  annotation_scale() +
  theme_void()+ 
  theme(axis.text = element_blank(), plot.background = element_rect(fill = 'ghostwhite', )) +
  labs(x = '', y='')
  

# inset map
map_total <- 
ggdraw() + 
  draw_plot(map_study_area) +
  draw_plot(map_ec, 
            x=0.62, y=0.26, width = 0.25)





