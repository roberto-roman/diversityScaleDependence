#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- load data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("scripts/scale.R")
rmarkdown::render("informe_final.Rmd")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- multiple correlations ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Multiple correlations between alpha and bio.vars worldclim
pacman::p_load(GGally)
db.alpha.01 %>% 
  select(shannon, simpson, richness, starts_with('bio')) %>% 
  GGally::ggcorr(label = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- hipotesis 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_alpha <- db.alpha.01 %>% filter(group != 17) %>% 
  mutate(fct.plots =
           chop_evenly(plots.agreggated, 3),
         escala = plots.agreggated*25,
         fct.escala =
           chop_evenly(escala, 3))

## Alpha diversidad
# Colocar en texto principal
(area.alpha.elev.plot <- 
    db_alpha %>%
   ggplot(aes(escala, shannon, fill = fct.elev, color = fct.elev)) +
   geom_point(alpha = 0.6) +
   geom_smooth(method = 'lm', alpha = 0.1) +
   labs(x = expression(paste("Área ", 'm'^2)), y = 'Diversidad Alpha (Shannon)', color = 'Elevación', fill = 'Elevación') +
   facet_wrap(vars(fct.elev) ) +
   theme_bw())

# Colocar en anexos este grafico para ejemplicar mayor riqueza a mayor altitud
(alfa.biovar.elev.plot.02 <-
    db_alpha %>%
    ggplot() +
    geom_boxplot(aes(fct.elev, shannon, color = fct.plots)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Alpha (Shannon)') +
    theme_bw())

# Anova para representar este grafico
(alfa.biovar.elev.plot.03 <-
    db_alpha %>%
    ggplot() +
    geom_boxplot(aes(fct.elev, shannon)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Alpha (Shannon)') +
    theme_bw())

## modelos
# diversidad alpha vs altitud
diff_alpha_elev <- aov(shannon ~ fct.elev, data = db_alpha)
posthoc_diff_alpha_elev <- TukeyHSD(diff_alpha_elev)

diff_alpha_elev <- lm(shannon ~ fct.elev, data = db_alpha)
diff_alpha_elev %>% summary()

# diversidad alpha vs altitud y escala de análisis
alpha_vs_elev_grain <- lm(shannon ~ fct.elev + plots.agreggated + fct.elev:plots.agreggated, data = db_alpha)
alpha_vs_elev_grain %>% summary()

## Beta diversidad
db_beta <- db.beta.01 %>% 
  filter(group != 17) %>% 
  mutate(fct.plots =
           chop_evenly(plots.agreggated, 3),
         escala = plots.agreggated*25,
         fct.escala =
           chop_evenly(escala, 3))


# colocar en escrito principal
(area.beta.elev.plot <- 
    db_beta %>%
    ggplot(aes(escala, 1-beta, fill = fct.elev, color = fct.elev)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = 'lm', alpha = 0.1) +
    labs(x = expression(paste("Área ", 'm'^2)), y = 'Diversidad Beta (Sorensen)', color = 'Elevación', fill = 'Elevación') +
    facet_wrap(vars(fct.elev) ) +
    # scale_y_continuous(limits = c(0.60, 1)) +
    theme_bw())

# ejemplicar variación en beta según rango altitudinal
(beta.biovar.elev.plot.02 <-
    db_beta %>%
    ggplot() +
    geom_boxplot(aes(fct.elev, 1-beta, color = fct.plots)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme_bw())

# resultados generales de beta diversidad según el rango altitudinal
(beta.biovar.elev.plot.03 <-
    db_beta %>%
    ggplot() +
    geom_boxplot(aes(fct.elev, 1-beta)) +
    labs(x = 'Elevación', color = 'Escala de análisis', y = 'Diversidad Beta (Sorensen)') +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme_bw())

## modelos
# diversidad beta vs altitud
diff_beta_elev <- aov(1-beta ~ fct.elev, data = db_beta)
posthoc_diff_beta_elev <- TukeyHSD(diff_beta_elev)

diff_beta_elev <- lm(1-beta ~ fct.elev, data = db_beta)
diff_beta_elev %>% summary()

# diversidad beta vs altitud y escala de análisis
beta_vs_elev_grain <- lm(1-beta ~ fct.elev + plots.agreggated + fct.elev:plots.agreggated, data = db_beta)
beta_vs_elev_grain %>% summary()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---- hipotesis 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# alpha
(alpha_bio18 <-
   db_alpha %>% 
   ggplot(aes(bio_18_mean, shannon, color = fct.escala, fill = fct.escala)) +
   geom_point() +
   geom_smooth(method = 'lm', alpha = 0.1) +
   facet_wrap(vars(fct.elev), scales = 'free' ) +
   labs(x = 'Humedad (Precipitación del mes más cálido en mm)', 
        color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
        fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
        y = 'Diversidad alpha (Shannon)') +
   theme_bw())

(alpha_bio4 <-
    db_alpha %>% 
    ggplot(aes(bio_4_mean, shannon, color = fct.escala, fill = fct.escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct.elev), scales = 'free' ) +
    labs(x = 'Temperatura (Estacionalidad térmica en C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad alpha (Shannon)') +
    theme_bw())


#beta

(beta_bio18 <-
    db_beta %>% 
    ggplot(aes(bio_18_mean, 1-beta, color = fct.escala, fill = fct.escala)) +
    geom_point() +
    geom_smooth(method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct.elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta de Humedad (Precipitación del mes más cálido en mm)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw())

(beta_bio18 <-
    db_beta %>% 
    ggplot(aes(bio_4_mean, 1-beta, color = fct.escala, fill = fct.escala)) +
    geom_point() +
    geom_smooth( method = 'lm', alpha = 0.1) +
    facet_wrap(vars(fct.elev), scales = 'free' ) +
    labs(x = 'Diferencia absoluta Temperatura (Estacionalidad térmica en C°)', 
         color = expression(paste('Escala de análisis ', '(m'^2, ')')), 
         fill = expression(paste('Escala de análisis ', '(m'^2, ')')),
         y = 'Diversidad beta (Sorensen)') +
    theme_bw())

## modelos

# alpha

mod_alpha_bio <- lm(shannon ~ fct.elev + bio_18_mean + bio_4_mean + fct.escala + 
                      bio_18_mean:fct.escala + bio_4_mean:fct.escala, 
                              data = db_alpha)
(sum_mod_alpha_bio  <-  summary(mod_alpha_bio))


pacman::p_load(modelsummary)

models_alpha_elev_bio <- 
db_alpha %>% 
  nest(data = -fct.elev) %>% 
  arrange(fct.elev) %>% 
  mutate(
    model_bio18 = map(data, ~lm(shannon ~ bio_18_mean + fct.escala + bio_18_mean:fct.escala, data = .x)),
    model_bio4 = map(data, ~lm(shannon ~ bio_4_mean + fct.escala + bio_4_mean:fct.escala, data = .x)),
    coef_bio18 = map(model_bio18,
                     ~.x %>% 
                       summary() %>% 
                       .$coefficients %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = 'variable') %>% 
                       select(1,2,5)
                       ),
    coef_bio4 = map(model_bio4,
                     ~.x %>% 
                       summary() %>% 
                       .$coefficients %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = 'variable') %>% 
                       select(1,2,5)
    ),
  )

modelsummary(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), stars = TRUE)
modelsummary(models_alpha_elev_bio[[4]] %>% set_names(models_alpha_elev_bio[[1]]), stars = TRUE)

modelplot(models_alpha_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), coef_omit = "Intercept|^fct")
modelplot(models_alpha_elev_bio[[4]] %>% set_names(models_alpha_elev_bio[[1]]), coef_omit = "Intercept")


# Beta
mod_beta_bio <- lm(beta ~ fct.elev + bio_18_mean + bio_4_mean + fct.escala + 
                      bio_18_mean:fct.escala + bio_4_mean:fct.escala, 
                    data = db_beta)
(sum_mod_beta_bio  <-  summary(mod_beta_bio))

models_beta_elev_bio <- 
  db_beta %>% 
  nest(data = -fct.elev) %>% 
  arrange(fct.elev) %>% 
  mutate(
    model_bio18 = map(data, ~lm(beta ~ bio_18_mean + fct.escala + bio_18_mean:fct.escala, data = .x)),
    model_bio4 = map(data, ~lm(beta ~ bio_4_mean + fct.escala + bio_4_mean:fct.escala, data = .x)),
    coef_bio18 = map(model_bio18,
                     ~.x %>% 
                       summary() %>% 
                       .$coefficients %>% 
                       as.data.frame() %>% 
                       rownames_to_column(var = 'variable') %>% 
                       select(1,2,5)
    ),
    coef_bio4 = map(model_bio4,
                    ~.x %>% 
                      summary() %>% 
                      .$coefficients %>% 
                      as.data.frame() %>% 
                      rownames_to_column(var = 'variable') %>% 
                      select(1,2,5)
    ),
  )

modelsummary(models_beta_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), stars = TRUE)
modelsummary(models_beta_elev_bio[[4]] %>% set_names(models_alpha_elev_bio[[1]]), stars = TRUE)


modelplot(models_beta_elev_bio[[3]] %>% set_names(models_alpha_elev_bio[[1]]), coef_omit = "Intercept")
modelplot(models_beta_elev_bio[[4]] %>% set_names(models_alpha_elev_bio[[1]]), coef_omit = "Intercept")





