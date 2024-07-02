#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- load data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source("scripts/scale.R")
# rmarkdown::render("informe_final_Rmd")
pacman::p_load(modelsummary, tinytable)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- multiple correlations ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Multiple correlations between alpha and bio_vars worldclim
pacman::p_load(GGally)

(multiple_corr <- 
db.alpha.01 %>% 
  select(shannon, simpson, richness, starts_with('bio')) %>% 
  GGally::ggcorr(label = TRUE))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- hipotesis 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_alpha <- 
  db.alpha.01 %>% 
  filter(group != 17) %>% 
  set_names(names(.) %>% str_replace('\\.', '_') %>% str_remove('_mean')) %>% 
  mutate(fct_plots =
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
                             ))
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

diff_alpha_elev <- lm(shannon ~ fct_elev, data = db_alpha)
diff_alpha_elev %>% summary()

ms_anova_alpha_elev <- 
modelsummary(diff_alpha_elev, statistic = NULL, estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE', output = 'flextable')

# diversidad alpha vs altitud y escala de análisis
alpha_vs_elev_grain <- lm(shannon ~ fct_elev + plots_agreggated + fct_elev:plots_agreggated, data = db_alpha)
alpha_vs_elev_grain %>% summary()

## Beta diversidad
db_beta <- db.beta.01 %>% 
  set_names(names(.) %>% str_replace('\\.', '_') %>% str_remove('_mean')) %>% 
  filter(group != 17) %>% 
  mutate(fct_plots =
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
         ))


# colocar en escrito principal
(area_beta_elev_plot <- 
    db_beta %>%
    ggplot(aes(escala, 1-beta, fill = fct_elev, color = fct_elev)) +
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
    geom_boxplot(aes(fct_elev, 1-beta, color = fct_plots)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme_bw())

# resultados generales de beta diversidad según el rango altitudinal
(beta_biovar_elev_plot_03 <-
    db_beta %>%
    ggplot() +
    geom_boxplot(aes(fct_elev, 1-beta)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme_bw())

## modelos
# diversidad beta vs altitud
diff_beta_elev <- aov(1-beta ~ fct_elev, data = db_beta)
posthoc_diff_beta_elev <- TukeyHSD(diff_beta_elev)

diff_beta_elev <- lm(1-beta ~ fct_elev, data = db_beta)
diff_beta_elev %>% summary()

ms_anova_beta_elev <- 
  modelsummary(diff_beta_elev, statistic = NULL, estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
               gof_omit = 'AIC|BIC|Log|RMSE', output = 'flextable')

# diversidad beta vs altitud y escala de análisis
beta_vs_elev_grain <- lm(1-beta ~ fct_elev + plots_agreggated + fct_elev:plots_agreggated, data = db_beta)
beta_vs_elev_grain %>% summary()



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
(alpha_bio18 <-
   db_alpha %>% 
   ggplot(aes(bio_18, shannon, color = fct_escala, fill = fct_escala)) +
   geom_point() +
   geom_smooth(method = 'lm', alpha = 0.1) +
   facet_wrap(vars(fct_elev), scales = 'free' ) +
   labs(x = 'Precipitación (Precipitación del mes más cálido en mm)', 
        color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
        fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
        y = 'Diversidad alpha (Shannon)') +
   theme_bw())

(alpha_bio4 <-
    db_alpha %>% 
    ggplot(aes(bio_4, shannon, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Temperatura (Estacionalidad térmica en C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad alpha (Shannon)') +
    theme_bw())


#beta
(beta_bio18 <-
    db_beta %>% 
    ggplot(aes(bio_18, 1-beta, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta de Precipitación (Precipitación del mes más cálido en mm)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw())

(beta_bio4 <-
    db_beta %>% 
    ggplot(aes(bio_4, 1-beta, color = fct_escala, fill = fct_escala)) +
    geom_point() +
    geom_smooth( method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct_elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta Temperatura (Estacionalidad térmica en C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw())

## modelos

# alpha
mod_alpha_bio <- lm(shannon ~ fct_elev + escala + bio_4 + bio_18 + 
                      bio_4:escala + bio_18:escala, 
                              data = db_alpha)

# beta
mod_beta_bio <- lm(1-beta ~ fct_elev + escala + bio_4 + bio_18 + 
                     bio_4:escala + bio_18:escala, 
                   data = db_beta)
# unificación
(ms_alpha_beta_bio <- 
    modelsummary(list('Alfa' =mod_alpha_bio, 'Beta'=mod_beta_bio), 
                 fmt = fmt_sci(digits = 2),
                 statistic = NULL, 
                 estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
                 gof_omit = 'AIC|BIC|Log|RMSE', 
                 output = 'flextable'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- Hipótesis 3 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alfa
models_alpha_elev_bio <- 
db_alpha %>% 
  nest(data = -fct_elev) %>% 
  arrange(fct_elev) %>% 
  mutate(
    models = map(data, ~lm(shannon ~ escala + bio_4 + bio_18 + bio_4:escala + bio_18:escala, data = .x)))

ms_models_per_elev_alpha <- 
modelsummary(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), 
             statistic = NULL, 
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE', 
             output = 'flextable')

# modelplot(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]))


# Beta
models_beta_elev_bio <- 
  db_beta %>% 
  nest(data = -fct_elev) %>% 
  arrange(fct_elev) %>% 
  mutate(
    models = map(data, ~lm(1-beta ~ escala + bio_4 + bio_18 + bio_4:escala + bio_18:escala, data = .x)))

ms_models_per_elev_beta <- 
modelsummary(models_beta_elev_bio[[3]] %>% set_names(models_beta_elev_bio[[1]]), 
             statistic = NULL, 
             estimate  = "{estimate} [{conf.low}, {conf.high}] {stars}",
             gof_omit = 'AIC|BIC|Log|RMSE',
             output = 'flextable') 

# modelplot(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]))





