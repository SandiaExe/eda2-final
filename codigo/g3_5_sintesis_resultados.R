# ============================================================================
# g3_5_sintesis_resultados.R
# g-iii. Inferencia estadística
# Parte 5. Interpretación contextualizada de los resultados
# ============================================================================

source("codigo/g3_0_funciones.R")


# ----------------------------------------------------------------------------
# 1. Resumen de todas las pruebas
# ----------------------------------------------------------------------------
ARCHIVOS_RESULTADOS <- c("pruebas_media_una_muestra",
                         "prueba_proporcion_una_muestra", "prueba_t_zona",
                         "prueba_mann_whitney_zona", "prueba_proporciones_zona",
                         "pruebas_tipo_ifi", "prueba_chi_cuadrado")

resultados <- list()
for (archivo in ARCHIVOS_RESULTADOS) {
  ruta <- file.path(DIR_G3, paste0(archivo, ".csv"))
  resultados[[archivo]] <- read_csv(ruta, col_types = cols(.default = col_character()))
}
resumen_pruebas <- bind_rows(resultados)
guardar_tabla(resumen_pruebas, "resumen_pruebas", DIR_G3)

# Interpretación: las 13 pruebas rechazan H0 con p < 0,001. Con 8 565 créditos, una
# prueba puede detectar diferencias muy pequeñas, de modo que importa más su magnitud:
# los 1,29 puntos entre zonas y los 3,40 puntos entre CMAC y bancos.


# ----------------------------------------------------------------------------
# 2. Supuestos y prueba que sustenta cada conclusión
# ----------------------------------------------------------------------------
decisiones <- tibble(
  objetivo = c("OE2, media", "OE2, proporción", "OE3, medias", "OE3, proporciones",
               "OE4", "OE5"),
  supuesto_verificado = c("Normalidad", "Casos por categoría >= 5",
                          "Normalidad y homogeneidad", "Casos por categoría >= 5",
                          "Normalidad y homogeneidad", "Frecuencias esperadas >= 5"),
  cumple = c("No", "Sí", "No", "Sí", "No", "Sí"),
  prueba_de_decision = c("Wilcoxon para una muestra, con t por el tamaño de muestra",
                         "z para una proporción",
                         "U de Mann-Whitney, con t de Welch como apoyo",
                         "z para dos proporciones", "Kruskal-Wallis",
                         "Chi-cuadrado de independencia")
)
guardar_tabla(decisiones, "decision_por_supuestos", DIR_G3)

# Interpretación: la tasa no cumple normalidad ni homogeneidad en ningún análisis, por
# lo que las pruebas no paramétricas sustentan las conclusiones sobre ella. Las
# pruebas paramétricas llegan a la misma decisión, lo que da solidez al resultado.


# ----------------------------------------------------------------------------
# 3. Selección de las pruebas no paramétricas
# ----------------------------------------------------------------------------
no_parametricas <- tibble(
  prueba = c("Chi-cuadrado", "Wilcoxon", "U de Mann-Whitney", "Kruskal-Wallis",
             "Friedman"),
  aplicada = c("Sí", "Sí", "Sí", "Sí", "No"),
  motivo = c("Asociación entre dos variables cualitativas (OE5)",
             "Comparar la tasa con un valor de referencia (OE2)",
             "Comparar la tasa entre dos grupos independientes (OE3)",
             "Comparar la tasa entre cuatro grupos independientes (OE4)",
             "Requiere medidas repetidas y cada crédito se observa una sola vez")
)
guardar_tabla(no_parametricas, "seleccion_no_parametricas", DIR_G3)

# Interpretación: la guía pide elegir las pruebas según el caso. Friedman no se aplica
# porque compara mediciones repetidas de los mismos sujetos o bloques, y los créditos
# de este conjunto de datos son independientes.


# ----------------------------------------------------------------------------
# 4. Respuesta a las preguntas de investigación
# ----------------------------------------------------------------------------
# ¿La tasa difiere según el tipo de IFI? Sí. Es la mayor diferencia encontrada:
# las CMAC cobran en promedio 3,40 puntos más que los bancos.
#
# ¿Difiere entre Lima y Callao y el resto del país? Sí. Fuera de esa zona la tasa
# media es 1,29 puntos mayor, y 54,97 % de los créditos supera el 10 %, frente a
# 11,12 % en Lima y Callao.
#
# ¿Existe asociación entre el tipo de IFI y la zona? Sí. Las CMAC se concentran fuera
# de Lima y Callao. Parte de la brecha entre zonas puede deberse a qué entidades
# atienden en cada una, aunque en g-ii la brecha se mantuvo dentro de cada tipo de IFI.
#
# Implicancia para el FMV: las familias de regiones, sobre todo las atendidas por
# CMAC, afrontan mayores tasas en un mismo programa social. Además, el programa llega
# menos a las regiones (32,2 % de los créditos frente a 64,33 % de la población).
# Estos resultados apoyan revisar la tasa preferente, la cobertura de riesgo y la
# gestión comercial con las IFI no bancarias.
#
# Limitaciones: la inferencia compara grupos sin controlar el monto, el plazo ni el
# riesgo del cliente, y el archivo no incluye datos del ingreso ni del historial de
# las familias. Por eso, estas diferencias son asociaciones y no prueban que la
# entidad o la zona causen la mayor tasa. Las referencias externas de la tasa miden
# realidades distintas y la de población proviene del censo de 2017. Los créditos
# cubren casi todo el año, de modo que la inferencia describe el proceso de
# colocación de 2024, más que una muestra aleatoria de otra población.
