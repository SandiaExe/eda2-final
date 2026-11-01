# ============================================================================
# g3_0_funciones.R
# g-iii. Inferencia estadística
# Constantes y funciones comunes: verificación de supuestos y registro de pruebas
# ============================================================================

source("codigo/00_configuracion.R")


# ----------------------------------------------------------------------------
# 1. Constantes
# ----------------------------------------------------------------------------
# shapiro.test() admite como máximo 5 000 observaciones. En grupos más grandes se
# usa una muestra aleatoria reproducible.
N_MAX_SHAPIRO <- 5000
SEMILLA       <- 2026

# Población censada en 2017 (INEI, 2018).
POBLACION_NACIONAL <- 29381884
POBLACION_LIMA     <- 9485405
POBLACION_CALLAO   <- 994494
P0_FUERA_DE_LIMA   <- 1 - (POBLACION_LIMA + POBLACION_CALLAO) / POBLACION_NACIONAL

# Tasas de referencia del mercado, en porcentaje. Las de la SBS provienen de
# Gestión (Reyes, 2024) e Infobae (Montesinos, 2024).
TASA_SBS_ENERO   <- 9.1
TASA_SBS_OCTUBRE <- 8.4

# Umbral que define un crédito de tasa alta en la comparación de proporciones.
UMBRAL_TASA_ALTA <- 10

# Mínimo de casos por categoría para la aproximación normal de proporciones.
MIN_CASOS_PROPORCION <- 5

# Mínimo de frecuencia esperada por celda en la prueba Chi-cuadrado.
MIN_ESPERADA_CHI <- 5


# ----------------------------------------------------------------------------
# 2. Lectura de la referencia del BCRP
# ----------------------------------------------------------------------------
# Lee la serie mensual PN07848NM y devuelve su promedio de 2024.
leer_tasa_bcrp <- function(ruta = ARCHIVO_BCRP) {
  serie <- read.csv(ruta, skip = 2, header = FALSE, col.names = c("mes", "tasa"),
                    fileEncoding = "latin1")
  mean(serie$tasa)
}


# ----------------------------------------------------------------------------
# 3. Verificación de supuestos
# ----------------------------------------------------------------------------
# Prueba de Shapiro-Wilk. H0: los datos provienen de una distribución normal.
prueba_normalidad <- function(x, grupo = "General") {
  usados <- x
  if (length(x) > N_MAX_SHAPIRO) {
    set.seed(SEMILLA)
    usados <- sample(x, N_MAX_SHAPIRO)
  }
  prueba <- shapiro.test(usados)
  tibble(grupo = grupo, n = length(x), n_usado = length(usados),
         W = round(unname(prueba$statistic), 4),
         valor_p = formatear_p(prueba$p.value),
         asimetria = round(skewness(x, type = 2), 3),
         curtosis = round(kurtosis(x, type = 2), 3),
         cumple_normalidad = prueba$p.value >= ALFA)
}

# Aplica Shapiro-Wilk a cada grupo.
normalidad_por_grupo <- function(datos, variable, grupo) {
  resultados <- list()
  for (nivel in levels(datos[[grupo]])) {
    valores <- datos[[variable]][datos[[grupo]] == nivel]
    resultados[[nivel]] <- prueba_normalidad(valores, nivel)
  }
  bind_rows(resultados)
}

# Prueba de Levene centrada en la mediana. H0: las varianzas son iguales.
prueba_levene <- function(y, grupo) {
  desviaciones <- abs(y - ave(y, grupo, FUN = median))
  tabla <- anova(lm(desviaciones ~ grupo))
  tibble(F_levene = round(tabla$`F value`[1], 3), gl1 = tabla$Df[1],
         gl2 = tabla$Df[2], valor_p = formatear_p(tabla$`Pr(>F)`[1]),
         cumple_homogeneidad = tabla$`Pr(>F)`[1] >= ALFA)
}

# Gráfico Q-Q para evaluar visualmente la normalidad.
grafico_qq <- function(datos, variable, titulo, grupo = NULL) {
  grafico <- ggplot(datos, aes(sample = .data[[variable]])) +
    stat_qq(color = COLOR_PRINCIPAL, size = 0.7, alpha = 0.5) +
    stat_qq_line(color = COLOR_REFERENCIA, linewidth = 0.7) +
    labs(title = titulo, x = "Cuantiles teóricos", y = "Cuantiles observados") +
    tema_informe()
  if (!is.null(grupo)) {
    grafico <- grafico + facet_wrap(~ .data[[grupo]], scales = "free_y")
  }
  grafico
}


# ----------------------------------------------------------------------------
# 4. Proporciones con aproximación normal
# ----------------------------------------------------------------------------
# Intervalo de confianza para una proporción.
ic_proporcion <- function(exitos, n, confianza = NIVEL_CONFIANZA) {
  p <- exitos / n
  z <- qnorm(1 - (1 - confianza) / 2)
  error <- z * sqrt(p * (1 - p) / n)
  c(estimacion = p, inferior = p - error, superior = p + error)
}

# Prueba z para una proporción (bilateral).
prueba_z_proporcion <- function(exitos, n, p0) {
  p <- exitos / n
  z <- (p - p0) / sqrt(p0 * (1 - p0) / n)
  c(z = z, valor_p = 2 * pnorm(-abs(z)))
}

# Intervalo de confianza para la diferencia de dos proporciones.
ic_dif_proporciones <- function(x1, n1, x2, n2, confianza = NIVEL_CONFIANZA) {
  p1 <- x1 / n1
  p2 <- x2 / n2
  z <- qnorm(1 - (1 - confianza) / 2)
  error <- z * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  c(estimacion = p1 - p2, inferior = p1 - p2 - error, superior = p1 - p2 + error)
}

# Prueba z para la diferencia de dos proporciones, con proporción combinada.
prueba_z_dos_proporciones <- function(x1, n1, x2, n2) {
  combinada <- (x1 + x2) / (n1 + n2)
  z <- (x1 / n1 - x2 / n2) /
    sqrt(combinada * (1 - combinada) * (1 / n1 + 1 / n2))
  c(z = z, valor_p = 2 * pnorm(-abs(z)))
}


# ----------------------------------------------------------------------------
# 5. Registro de resultados
# ----------------------------------------------------------------------------
# Indica la decisión sobre H0 según el valor p.
decidir <- function(valor_p, alfa = ALFA) {
  if (valor_p < alfa) "Se rechaza H0" else "No se rechaza H0"
}

# Escribe un número con coma decimal.
con_coma <- function(valor, decimales) {
  gsub(".", ",", sprintf(paste0("%.", decimales, "f"), valor), fixed = TRUE)
}

# Muestra valores p muy pequeños de forma legible.
formatear_p <- function(valor_p) {
  if (valor_p < 0.001) "< 0,001" else con_coma(valor_p, 3)
}

# Arma una fila de la tabla de resultados de una prueba.
fila_prueba <- function(objetivo, prueba, hipotesis_nula, estadistico, valor_p,
                        gl = NA, ic_inferior = NA, ic_superior = NA) {
  tibble(objetivo = objetivo, prueba = prueba, hipotesis_nula = hipotesis_nula,
         estadistico = round(estadistico, 3), gl = round(gl, 1),
         valor_p = formatear_p(valor_p),
         ic95_inferior = round(ic_inferior, 3),
         ic95_superior = round(ic_superior, 3),
         decision = decidir(valor_p))
}
