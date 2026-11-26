# ============================================================================
# g4_0_funciones.R
# g-iv. Modelos de regresión
# Constantes y funciones comunes: preparación, ajuste y validación de modelos
# ============================================================================

# Reutiliza las pruebas de normalidad y de Levene y el formato de valores p de g-iii.
source("codigo/g3_0_funciones.R")


# ----------------------------------------------------------------------------
# 1. Constantes
# ----------------------------------------------------------------------------
DIR_G4 <- file.path(DIR_SALIDAS, "g4")

# Los montos se expresan en unidades de S/ 10 000 para que los coeficientes sean
# legibles; el plazo, en años (PLAZO_ANIOS, creada en g-i).
ESCALA_MONTO <- 10000

# Grupos de valores ajustados para evaluar la homogeneidad de los residuos con
# la prueba de Levene.
N_GRUPOS_AJUSTADOS <- 5

# Un residuo estandarizado mayor a 3 en valor absoluto se considera extremo.
UMBRAL_RESIDUO <- 3

# Nombres legibles de los términos de los modelos.
ETIQUETAS_TERMINOS <- c(
  "(Intercept)"        = "Intercepto",
  "CREDITO_10K"        = "Crédito (S/ 10 000)",
  "PLAZO_ANIOS"        = "Plazo (años)",
  "CUOTA_10K"          = "Cuota inicial (S/ 10 000)",
  "TIPO_IFICMAC"       = "CMAC",
  "TIPO_IFIFinanciera" = "Financiera",
  "TIPO_IFIEdpyme"     = "Edpyme",
  "ZONAResto del país" = "Resto del país"
)


# ----------------------------------------------------------------------------
# 2. Preparación de los datos
# ----------------------------------------------------------------------------
# Agrega los montos reescalados. Banco y Lima y Callao quedan como categorías de
# referencia porque son los primeros niveles de sus factores.
preparar_datos_modelo <- function() {
  leer_datos_limpios() %>%
    mutate(CREDITO_10K = MONTO_CREDITO / ESCALA_MONTO,
           CUOTA_10K   = MONTO_CUOTA_INICIAL / ESCALA_MONTO)
}


# ----------------------------------------------------------------------------
# 3. Resultados de un modelo
# ----------------------------------------------------------------------------
# Coeficientes con su prueba t e intervalo de confianza.
# H0: el coeficiente es 0; H1: es distinto de 0.
tabla_coeficientes <- function(modelo, nombre) {
  coeficientes <- summary(modelo)$coefficients
  intervalos   <- confint(modelo, level = NIVEL_CONFIANZA)
  tibble(modelo = nombre,
         termino = unname(ETIQUETAS_TERMINOS[rownames(coeficientes)]),
         coeficiente = round(coeficientes[, "Estimate"], 4),
         error_estandar = round(coeficientes[, "Std. Error"], 4),
         t = round(coeficientes[, "t value"], 2),
         valor_p = sapply(coeficientes[, "Pr(>|t|)"], formatear_p),
         ic95_inferior = round(intervalos[, 1], 4),
         ic95_superior = round(intervalos[, 2], 4),
         decision = sapply(coeficientes[, "Pr(>|t|)"], decidir))
}

# Bondad de ajuste y significancia global.
# H0: todos los coeficientes, salvo el intercepto, son 0; H1: al menos uno no lo es.
tabla_ajuste <- function(modelo, nombre) {
  resumen <- summary(modelo)
  prueba_f <- resumen$fstatistic
  valor_p <- pf(prueba_f[["value"]], prueba_f[["numdf"]], prueba_f[["dendf"]],
                lower.tail = FALSE)
  n <- nobs(modelo)
  tibble(modelo = nombre, n = n,
         R2 = round(resumen$r.squared, 4),
         R2_ajustado = round(resumen$adj.r.squared, 4),
         error_estandar_residual = round(resumen$sigma, 4),
         F = round(prueba_f[["value"]], 2), gl1 = prueba_f[["numdf"]],
         gl2 = prueba_f[["dendf"]], valor_p = formatear_p(valor_p),
         decision = decidir(valor_p))
}


# ----------------------------------------------------------------------------
# 4. Validación de los residuos
# ----------------------------------------------------------------------------
# Normalidad (Shapiro-Wilk), homogeneidad entre grupos de valores ajustados
# (Levene) y residuos estandarizados extremos.
validar_residuos <- function(modelo, nombre) {
  residuos <- residuals(modelo)
  grupos <- cut(fitted(modelo),
                breaks = quantile(fitted(modelo),
                                  probs = seq(0, 1, length.out = N_GRUPOS_AJUSTADOS + 1)),
                include.lowest = TRUE)
  normalidad   <- prueba_normalidad(residuos, nombre)
  homogeneidad <- prueba_levene(residuos, grupos)
  extremos <- sum(abs(rstandard(modelo)) > UMBRAL_RESIDUO)
  n <- nobs(modelo)
  tibble(modelo = nombre, W = normalidad$W, p_normalidad = normalidad$valor_p,
         cumple_normalidad = normalidad$cumple_normalidad,
         F_levene = round(homogeneidad$F_levene, 2),
         p_levene = homogeneidad$valor_p,
         cumple_homogeneidad = homogeneidad$cumple_homogeneidad,
         residuos_extremos = extremos,
         pct_extremos = round(100 * extremos / n, 2))
}

# Residuos frente a valores ajustados: evalúa linealidad y varianza constante.
grafico_residuos <- function(modelo) {
  datos <- tibble(ajustado = fitted(modelo), residuo = residuals(modelo))
  ggplot(datos, aes(x = ajustado, y = residuo)) +
    geom_point(color = COLOR_PRINCIPAL, alpha = 0.25, size = 0.8) +
    geom_hline(yintercept = 0, color = COLOR_REFERENCIA, linetype = "dashed") +
    labs(x = "Tasa ajustada (%)", y = "Residuo (puntos)") +
    tema_informe()
}

# Gráfico Q-Q de los residuos.
grafico_qq_residuos <- function(modelo) {
  grafico_qq(tibble(residuo = residuals(modelo)), "residuo", NULL)
}
