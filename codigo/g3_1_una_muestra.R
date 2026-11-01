# ============================================================================
# g3_1_una_muestra.R
# g-iii. Inferencia estadística
# Parte 1. Intervalos de confianza y pruebas de hipótesis para una muestra (OE2)
# ============================================================================

source("codigo/g3_0_funciones.R")

ncmv <- leer_datos_limpios()
tasa <- ncmv$TASA
n    <- length(tasa)

tasa_bcrp <- leer_tasa_bcrp()
creditos_fuera_lima <- sum(ncmv$ZONA == ZONA_RESTO)


# ----------------------------------------------------------------------------
# 1. Verificación de supuestos
# ----------------------------------------------------------------------------
# Normalidad de la tasa.
normalidad_tasa <- prueba_normalidad(tasa, "Todos los créditos")
guardar_tabla(normalidad_tasa, "normalidad_tasa", DIR_G3)

grafico_qq_tasa <- grafico_qq(ncmv, "TASA", "Gráfico Q-Q de la tasa de interés")
guardar_grafico(grafico_qq_tasa, "f15_qq_tasa", ancho = 6.5, alto = 4.5,
                directorio = DIR_G3)

# Interpretación: Shapiro-Wilk se aplicó a una muestra aleatoria de 5 000 créditos,
# el máximo que admite la función. Con W = 0,7617 y p < 0,001 se rechaza la
# normalidad, y el gráfico Q-Q confirma que la cola derecha se separa de la recta.
# Por el teorema central del límite, con n = 8 565 la media muestral es
# aproximadamente normal, de modo que el intervalo y la prueba t siguen siendo
# válidos. Aun así, al no cumplirse la normalidad, se aplica también Wilcoxon.

# Condiciones de la aproximación normal para la proporción.
condiciones_proporcion <- tibble(
  condicion = c("n * p0", "n * (1 - p0)", "n * p estimada", "n * (1 - p estimada)"),
  valor = round(c(n * P0_FUERA_DE_LIMA, n * (1 - P0_FUERA_DE_LIMA),
                  creditos_fuera_lima, n - creditos_fuera_lima), 1)
) %>%
  mutate(cumple = valor >= MIN_CASOS_PROPORCION)
guardar_tabla(condiciones_proporcion, "condiciones_proporcion_una_muestra", DIR_G3)

# Interpretación: todos los productos superan ampliamente 5 casos, de modo que la
# aproximación normal es adecuada para el intervalo y la prueba de la proporción.


# ----------------------------------------------------------------------------
# 2. Intervalos de confianza
# ----------------------------------------------------------------------------
ic_media <- t.test(tasa, conf.level = NIVEL_CONFIANZA)
ic_prop  <- ic_proporcion(creditos_fuera_lima, n)

intervalos <- tibble(
  parametro = c("Tasa media (%)", "Proporción fuera de Lima y Callao"),
  estimacion = c(unname(ic_media$estimate), ic_prop[["estimacion"]]),
  ic95_inferior = c(ic_media$conf.int[1], ic_prop[["inferior"]]),
  ic95_superior = c(ic_media$conf.int[2], ic_prop[["superior"]])
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
guardar_tabla(intervalos, "intervalos_una_muestra", DIR_G3)

# Interpretación: con 95 % de confianza, la tasa media del NCMV en 2024 está entre
# 9,769 % y 9,822 %, y entre 31,21 % y 33,19 % de los créditos corresponde a
# viviendas fuera de Lima y Callao. Ambos intervalos son muy estrechos por el gran
# tamaño de la muestra.


# ----------------------------------------------------------------------------
# 3. Pruebas de hipótesis para la media
# ----------------------------------------------------------------------------
# La tasa se compara con la serie del BCRP y, como sensibilidad, con las cifras de
# la SBS difundidas por la prensa.
referencias <- c("BCRP, promedio 2024 (saldos)" = tasa_bcrp,
                 "SBS, enero de 2024" = TASA_SBS_ENERO,
                 "SBS, octubre de 2024" = TASA_SBS_OCTUBRE)

pruebas_media <- list()
for (i in seq_along(referencias)) {
  mu0      <- referencias[[i]]
  etiqueta <- names(referencias)[i]
  prueba_t <- t.test(tasa, mu = mu0, conf.level = NIVEL_CONFIANZA)
  prueba_w <- wilcox.test(tasa, mu = mu0, exact = FALSE)

  pruebas_media[[i]] <- bind_rows(
    fila_prueba("OE2", paste("t para una muestra:", etiqueta),
                paste("media =", con_coma(mu0, 3)), unname(prueba_t$statistic),
                prueba_t$p.value, gl = unname(prueba_t$parameter),
                ic_inferior = prueba_t$conf.int[1],
                ic_superior = prueba_t$conf.int[2]),
    fila_prueba("OE2", paste("Wilcoxon para una muestra:", etiqueta),
                paste("mediana =", con_coma(mu0, 3)), unname(prueba_w$statistic),
                prueba_w$p.value)
  )
}
resultados_media <- bind_rows(pruebas_media)
guardar_tabla(resultados_media, "pruebas_media_una_muestra", DIR_G3)

# Interpretación: H0: la tasa media es igual a la referencia; H1: es distinta. Se
# rechaza H0 frente a las tres referencias (t = 182,70, 52,02 y 104,39; p < 0,001),
# y Wilcoxon coincide. La tasa media del NCMV supera en 2,44 puntos al promedio del
# BCRP (7,353 %), en 0,70 a la cifra de la SBS de enero (9,1 %) y en 1,40 a la de
# octubre (8,4 %). Estas referencias no miden exactamente lo mismo: el BCRP promedia
# los saldos de toda la cartera, incluidos créditos antiguos, mientras que el NCMV
# son créditos nuevos de un programa social. Por eso la comparación sitúa al NCMV
# frente al mercado, pero no mide un sobrecosto exacto. Wilcoxon supone simetría,
# que la tasa no cumple, por lo que su resultado se lee como respaldo de t.


# ----------------------------------------------------------------------------
# 4. Prueba de hipótesis para la proporción
# ----------------------------------------------------------------------------
prueba_prop <- prueba_z_proporcion(creditos_fuera_lima, n, P0_FUERA_DE_LIMA)

resultado_proporcion <- fila_prueba(
  "OE2", "z para una proporción: créditos fuera de Lima y Callao",
  paste("p =", con_coma(P0_FUERA_DE_LIMA, 4)), prueba_prop[["z"]],
  prueba_prop[["valor_p"]], ic_inferior = ic_prop[["inferior"]],
  ic_superior = ic_prop[["superior"]]
)
guardar_tabla(resultado_proporcion, "prueba_proporcion_una_muestra", DIR_G3)

# Interpretación: H0: p = 0,6433, que es la parte de la población que vive fuera de
# Lima y Callao según el censo de 2017; H1: p es distinta. Con z = -62,08 y
# p < 0,001 se rechaza H0: el 32,2 % de los créditos está muy por debajo del 64,33 %
# de la población. El programa llega menos a las regiones de lo que corresponde a
# su peso poblacional. La población no equivale a la demanda efectiva de crédito,
# que también depende de los ingresos y la oferta de vivienda, y este análisis no
# permite separar esos factores.
