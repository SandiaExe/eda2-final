# ============================================================================
# g2_2_medidas_descriptivas.R
# g-ii. Análisis descriptivo
# Parte 2. Medidas de tendencia central, dispersión, posición, asimetría y
# curtosis
# ============================================================================

source("codigo/00_configuracion.R")

ncmv <- leer_datos_limpios()


# ----------------------------------------------------------------------------
# 1. Funciones
# ----------------------------------------------------------------------------
# Devuelve el valor más frecuente.
calcular_moda <- function(x) {
  frecuencias <- table(x)
  as.numeric(names(frecuencias)[which.max(frecuencias)])
}

# Calcula las medidas descriptivas de una variable numérica. La asimetría y la
# curtosis (exceso sobre 0) usan los estimadores muestrales ajustados (type = 2).
medidas_resumen <- function(x) {
  cuantiles <- quantile(x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  tibble(
    n = length(x),
    media = mean(x), mediana = median(x), moda = calcular_moda(x),
    minimo = min(x), maximo = max(x), rango = max(x) - min(x),
    varianza = var(x), desv_estandar = sd(x),
    cv_pct = 100 * sd(x) / mean(x),
    P10 = cuantiles[[1]], Q1 = cuantiles[[2]], Q3 = cuantiles[[4]],
    P90 = cuantiles[[5]], RIC = cuantiles[[4]] - cuantiles[[2]],
    asimetria = skewness(x, type = 2), curtosis = kurtosis(x, type = 2)
  )
}

# Aplica las medidas a todas las variables numéricas, dentro de los grupos que
# se indiquen.
medidas_por_grupo <- function(datos, grupo = NULL) {
  resultados <- list()
  for (variable in VARIABLES_NUMERICAS) {
    if (is.null(grupo)) {
      resultados[[variable]] <- bind_cols(variable = variable,
                                          medidas_resumen(datos[[variable]]))
    } else {
      resultados[[variable]] <- datos %>%
        group_by(grupo = .data[[grupo]]) %>%
        summarise(medidas_resumen(.data[[variable]]), .groups = "drop") %>%
        mutate(variable = variable, .before = 1)
    }
  }
  bind_rows(resultados) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}


# ----------------------------------------------------------------------------
# 2. Medidas generales
# ----------------------------------------------------------------------------
medidas_generales <- medidas_por_grupo(ncmv)
guardar_tabla(medidas_generales, "medidas_generales", DIR_G2)

# Interpretación: la tasa tiene media de 9,795 %, mediana de 9,48 % y moda de
# 8,90 %. Su desviación estándar es 1,237 y su CV, 12,63 %, el menor de las cinco
# variables. La mitad central de los créditos paga entre 9,00 % y 10,03 %. Su
# asimetría (2,448) y su curtosis (8,937) indican cola derecha y valores extremos.
# La cuota inicial se comporta igual (2,391 y 9,271) y es la más dispersa (CV de
# 66,1 %). El monto del crédito y el valor de la vivienda son casi simétricos
# (-0,059 y -0,151) y planos (curtosis cercana a -1,1). La moda tiene poco sentido
# en montos continuos: resume repeticiones puntuales, no el centro de los datos.


# ----------------------------------------------------------------------------
# 3. Medidas por tipo de IFI
# ----------------------------------------------------------------------------
medidas_tipo_ifi <- medidas_por_grupo(ncmv, "TIPO_IFI")
guardar_tabla(medidas_tipo_ifi, "medidas_por_tipo_ifi", DIR_G2)

# Interpretación: la tasa mediana va de 9,35 % en bancos a 13,00 % en CMAC, 3,65
# puntos más; financieras (9,55 %) y Edpyme (10,50 %) quedan en medio. La varianza
# de las CMAC (2,833) es unas 7 veces la de los bancos (0,397). Las CMAC también
# otorgan créditos más pequeños y cortos: mediana de S/ 80 500 y 120 meses, frente a
# S/ 197 213 y 240 meses de los bancos. En las financieras, la media (10,219 %) se
# aleja de la mediana (9,55 %) porque hay dos perfiles: Crediscotia tiene tasas
# cercanas a 12 % y Financiera Efectiva, a 9,5 %. Edpyme es una sola entidad
# (Vivela), de modo que ese grupo no representa a un sector.


# ----------------------------------------------------------------------------
# 4. Medidas por zona geográfica
# ----------------------------------------------------------------------------
medidas_zona <- medidas_por_grupo(ncmv, "ZONA")
guardar_tabla(medidas_zona, "medidas_por_zona", DIR_G2)

# Interpretación: la tasa media es 9,380 % en Lima y Callao y 10,670 % en el resto
# del país, 1,290 puntos más; las medianas son 9,20 % y 10,17 %. La dispersión
# también es mayor fuera de Lima (desviación estándar de 1,535 frente a 0,777). Las
# zonas difieren además en el financiamiento: fuera de Lima, la mediana del crédito
# es S/ 120 791 (S/ 219 500 en Lima), la del plazo 180 meses (240 en Lima) y la de
# la vivienda S/ 171 654 (S/ 279 200 en Lima). Por ello, la brecha de tasa puede no
# deberse solo a la ubicación, lo que se evaluará en las etapas siguientes.
