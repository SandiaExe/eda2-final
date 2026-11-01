# ============================================================================
# g3_3_anova_kruskal.R
# g-iii. Inferencia estadística
# Parte 3. ANOVA de un factor y prueba de Kruskal-Wallis por tipo de IFI (OE4)
# ============================================================================

source("codigo/g3_0_funciones.R")

ncmv <- leer_datos_limpios()
modelo_anova <- aov(TASA ~ TIPO_IFI, data = ncmv)


# ----------------------------------------------------------------------------
# 1. Verificación de supuestos del ANOVA
# ----------------------------------------------------------------------------
# Normalidad dentro de cada grupo y de los residuos.
normalidad_tipo_ifi <- bind_rows(
  normalidad_por_grupo(ncmv, "TASA", "TIPO_IFI"),
  prueba_normalidad(residuals(modelo_anova), "Residuos del modelo")
)
guardar_tabla(normalidad_tipo_ifi, "normalidad_tasa_tipo_ifi", DIR_G3)

grafico_qq_residuos <- grafico_qq(
  tibble(residuo = residuals(modelo_anova)), "residuo",
  "Gráfico Q-Q de los residuos del ANOVA"
)
guardar_grafico(grafico_qq_residuos, "f17_qq_residuos_anova", ancho = 6.5,
                alto = 4.5, directorio = DIR_G3)

# Interpretación: se rechaza la normalidad en los cuatro grupos y en los residuos
# (W entre 0,8330 y 0,9648; p < 0,001). El gráfico Q-Q de los residuos se aleja de
# la recta en ambas colas.

homogeneidad_tipo_ifi <- prueba_levene(ncmv$TASA, ncmv$TIPO_IFI)
guardar_tabla(homogeneidad_tipo_ifi, "homogeneidad_tasa_tipo_ifi", DIR_G3)

cumple_supuestos <- all(normalidad_tipo_ifi$cumple_normalidad) &&
  homogeneidad_tipo_ifi$cumple_homogeneidad
cat("\n¿Se cumplen los supuestos del ANOVA?:", cumple_supuestos, "\n")

# Interpretación: H0: las varianzas son iguales. Con F = 680,25 y p < 0,001 se
# rechaza la homogeneidad. No se cumplen la normalidad ni la homogeneidad, y además
# los grupos son muy desbalanceados, por lo que la conclusión se basa en
# Kruskal-Wallis. El ANOVA y Tukey se presentan como comparación.


# ----------------------------------------------------------------------------
# 2. ANOVA de un factor
# ----------------------------------------------------------------------------
tabla_anova <- summary(modelo_anova)[[1]]

anova_resumen <- tibble(
  fuente = c("Tipo de IFI", "Residuos"),
  gl = tabla_anova$Df,
  suma_cuadrados = round(tabla_anova$`Sum Sq`, 2),
  cuadrado_medio = round(tabla_anova$`Mean Sq`, 3),
  F_valor = round(tabla_anova$`F value`, 2),
  valor_p = c(formatear_p(tabla_anova$`Pr(>F)`[1]), NA)
)
guardar_tabla(anova_resumen, "anova_tasa_tipo_ifi", DIR_G3)

# Interpretación: H0: las cuatro medias son iguales; H1: al menos una difiere. Con
# F = 3 850,75, gl = 3 y 8 561 y p < 0,001 se rechaza H0. Como los supuestos no se
# cumplen, este resultado es referencial.

resultado_anova <- fila_prueba(
  "OE4", "ANOVA de un factor: tasa según tipo de IFI",
  "las medias de los cuatro tipos son iguales", tabla_anova$`F value`[1],
  tabla_anova$`Pr(>F)`[1], gl = tabla_anova$Df[1]
)

# Medias con intervalo de confianza por tipo de IFI.
medias_tipo_ifi <- ncmv %>%
  group_by(TIPO_IFI) %>%
  summarise(n = n(), media = mean(TASA),
            ic_inferior = t.test(TASA)$conf.int[1],
            ic_superior = t.test(TASA)$conf.int[2], .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 3)))
guardar_tabla(medias_tipo_ifi, "ic_media_tasa_por_tipo_ifi", DIR_G3)

grafico_medias <- ggplot(medias_tipo_ifi,
                         aes(x = media, y = reorder(TIPO_IFI, media),
                             color = TIPO_IFI)) +
  geom_errorbar(aes(xmin = ic_inferior, xmax = ic_superior), width = 0.2,
                linewidth = 0.7, orientation = "y", show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(aes(label = format(media, decimal.mark = ",", nsmall = 2, digits = 4,
                               trim = TRUE)),
            color = "black", vjust = -1, size = 3.3) +
  scale_color_manual(values = COLORES_TIPO_IFI) +
  scale_x_continuous(labels = label_number(decimal.mark = ",")) +
  labs(title = "Tasa media con intervalo de confianza del 95 % por tipo de IFI",
       x = "Tasa media (%)", y = NULL) +
  tema_informe()
guardar_grafico(grafico_medias, "f18_ic_media_tipo_ifi", alto = 3.8,
                directorio = DIR_G3)

# Interpretación: ningún intervalo se superpone. El orden de menor a mayor tasa media
# es banco (9,465 %), financiera (10,219 %), Edpyme (10,776 %) y CMAC (12,867 %). El
# intervalo de los bancos es el más estrecho por tener más créditos y menor dispersión.


# ----------------------------------------------------------------------------
# 3. Comparaciones post hoc de Tukey
# ----------------------------------------------------------------------------
tukey <- TukeyHSD(modelo_anova, conf.level = NIVEL_CONFIANZA)$TIPO_IFI

comparaciones_tukey <- tibble(
  comparacion = rownames(tukey),
  diferencia = round(tukey[, "diff"], 3),
  ic95_inferior = round(tukey[, "lwr"], 3),
  ic95_superior = round(tukey[, "upr"], 3),
  valor_p_ajustado = sapply(tukey[, "p adj"], formatear_p),
  significativa = tukey[, "p adj"] < ALFA
)
guardar_tabla(comparaciones_tukey, "comparaciones_tukey", DIR_G3)

# Interpretación: las seis diferencias de medias son significativas. La mayor es
# CMAC frente a banco (3,402 puntos; IC 95 % de 3,319 a 3,485) y la menor, Edpyme
# frente a financiera (0,557 puntos). Tukey supone varianzas iguales, por lo que estos
# resultados son referenciales.


# ----------------------------------------------------------------------------
# 4. Prueba de Kruskal-Wallis
# ----------------------------------------------------------------------------
prueba_kw <- kruskal.test(TASA ~ TIPO_IFI, data = ncmv)

resultado_kw <- fila_prueba(
  "OE4", "Kruskal-Wallis: tasa según tipo de IFI",
  "las distribuciones de los cuatro tipos son iguales",
  unname(prueba_kw$statistic), prueba_kw$p.value,
  gl = unname(prueba_kw$parameter)
)
guardar_tabla(bind_rows(resultado_anova, resultado_kw), "pruebas_tipo_ifi", DIR_G3)

# Interpretación: H0: la distribución de la tasa es igual en los cuatro tipos de IFI;
# H1: al menos uno difiere. Con H = 2 014,88, gl = 3 y p < 0,001 se rechaza H0. Es la
# prueba que decide y coincide con el ANOVA: la tasa difiere según el tipo de entidad
# que otorga el crédito.


# ----------------------------------------------------------------------------
# 5. Comparaciones por pares con U de Mann-Whitney
# ----------------------------------------------------------------------------
# Los valores p se ajustan con Bonferroni por haber seis comparaciones.
pares <- pairwise.wilcox.test(ncmv$TASA, ncmv$TIPO_IFI,
                              p.adjust.method = "bonferroni", exact = FALSE)

comparaciones_pares <- as.data.frame(as.table(pares$p.value)) %>%
  filter(!is.na(Freq)) %>%
  transmute(comparacion = paste(Var1, "-", Var2),
            valor_p_ajustado = sapply(Freq, formatear_p),
            significativa = Freq < ALFA)
guardar_tabla(comparaciones_pares, "comparaciones_mann_whitney", DIR_G3)

# Interpretación: las seis comparaciones son significativas tras el ajuste de
# Bonferroni (p < 0,001). Cada tipo de IFI difiere de los demás, y no solo las CMAC
# frente al resto. Edpyme debe leerse con cuidado, porque es una sola entidad.
