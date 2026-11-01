# ============================================================================
# g3_2_dos_muestras.R
# g-iii. Inferencia estadística
# Parte 2. Intervalos de confianza y pruebas de hipótesis para dos muestras (OE3)
# ============================================================================

source("codigo/g3_0_funciones.R")

ncmv <- leer_datos_limpios()
tasa_resto <- ncmv$TASA[ncmv$ZONA == ZONA_RESTO]
tasa_lima  <- ncmv$TASA[ncmv$ZONA == ZONA_LIMA]


# ----------------------------------------------------------------------------
# 1. Verificación de supuestos para la diferencia de medias
# ----------------------------------------------------------------------------
normalidad_zona <- normalidad_por_grupo(ncmv, "TASA", "ZONA")
guardar_tabla(normalidad_zona, "normalidad_tasa_zona", DIR_G3)

grafico_qq_zona <- grafico_qq(ncmv, "TASA", "Gráfico Q-Q de la tasa según la zona",
                              grupo = "ZONA")
guardar_grafico(grafico_qq_zona, "f16_qq_tasa_zona", ancho = 9, alto = 4.2,
                directorio = DIR_G3)

# Interpretación: se rechaza la normalidad en ambas zonas (W = 0,7688 y W = 0,8210;
# p < 0,001). El gráfico Q-Q muestra colas derechas pesadas y escalones, porque
# muchas tasas se repiten en valores redondos, como 10 % y 12 % fuera de Lima.

homogeneidad_zona <- prueba_levene(ncmv$TASA, ncmv$ZONA)
guardar_tabla(homogeneidad_zona, "homogeneidad_tasa_zona", DIR_G3)

# Interpretación: H0: las varianzas son iguales. Con F = 762,38 y p < 0,001 se
# rechaza la homogeneidad: la tasa varía más fuera de Lima y Callao.

# Si las varianzas difieren se usa t de Welch; si son iguales, t combinada.
varianzas_iguales <- homogeneidad_zona$cumple_homogeneidad


# ----------------------------------------------------------------------------
# 2. Diferencia de medias: intervalo de confianza y prueba t
# ----------------------------------------------------------------------------
prueba_t <- t.test(tasa_resto, tasa_lima, var.equal = varianzas_iguales,
                   conf.level = NIVEL_CONFIANZA)

nombre_t <- if (varianzas_iguales) "t con varianzas iguales" else "t de Welch"

resultado_t <- fila_prueba(
  "OE3", paste0(nombre_t, ": tasa media, resto menos Lima y Callao"),
  "diferencia de medias = 0", unname(prueba_t$statistic), prueba_t$p.value,
  gl = unname(prueba_t$parameter), ic_inferior = prueba_t$conf.int[1],
  ic_superior = prueba_t$conf.int[2]
)
guardar_tabla(resultado_t, "prueba_t_zona", DIR_G3)

# Interpretación: H0: la diferencia de medias es 0; H1: es distinta. Como las
# varianzas difieren, se usa t de Welch (t = 41,67; gl = 3 444,9; p < 0,001), y se
# rechaza H0. Con 95 % de confianza, la tasa media fuera de Lima y Callao supera a la
# de esa zona entre 1,229 y 1,350 puntos. El tamaño de muestra sostiene esta prueba
# aunque no haya normalidad.

medias_zona <- ncmv %>%
  group_by(ZONA) %>%
  summarise(n = n(), media = mean(TASA),
            ic_inferior = t.test(TASA)$conf.int[1],
            ic_superior = t.test(TASA)$conf.int[2], .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 3)))
guardar_tabla(medias_zona, "ic_media_tasa_por_zona", DIR_G3)

# Interpretación: los intervalos no se superponen: [9,360; 9,400] en Lima y Callao y
# [10,613; 10,727] en el resto del país, lo que concuerda con la prueba t.


# ----------------------------------------------------------------------------
# 3. Prueba U de Mann-Whitney
# ----------------------------------------------------------------------------
prueba_u <- wilcox.test(tasa_resto, tasa_lima, exact = FALSE)

resultado_u <- fila_prueba(
  "OE3", "U de Mann-Whitney: tasa, resto frente a Lima y Callao",
  "las distribuciones son iguales", unname(prueba_u$statistic), prueba_u$p.value
)
guardar_tabla(resultado_u, "prueba_mann_whitney_zona", DIR_G3)

# Interpretación: H0: las distribuciones de la tasa son iguales en ambas zonas. Con
# U = 13 395 346 y p < 0,001 se rechaza H0. Es la prueba que decide al no cumplirse
# la normalidad, y coincide con t. U / (n1 * n2) = 0,836 indica que, al tomar un
# crédito de cada zona, en cerca de 84 % de los pares la tasa es mayor fuera de Lima
# y Callao. Como las zonas tienen formas distintas, la prueba compara las
# distribuciones, no estrictamente las medianas.


# ----------------------------------------------------------------------------
# 4. Diferencia de proporciones de créditos con tasa alta
# ----------------------------------------------------------------------------
tasa_alta <- ncmv %>%
  group_by(ZONA) %>%
  summarise(creditos = n(), tasa_alta = sum(TASA > UMBRAL_TASA_ALTA),
            otros = creditos - tasa_alta,
            proporcion = round(tasa_alta / creditos, 4), .groups = "drop")
guardar_tabla(tasa_alta, "proporcion_tasa_alta_zona", DIR_G3)

# Interpretación: se define como tasa alta la mayor a 10 %, valor redondo cercano al
# tercer cuartil (10,03 %). La supera 54,97 % de los créditos fuera de Lima y Callao
# y 11,12 % de los de esa zona.

# Condiciones: al menos 5 casos en cada categoría y zona.
condicion_casos <- all(c(tasa_alta$tasa_alta, tasa_alta$otros) >=
                         MIN_CASOS_PROPORCION)
cat("\nCumple la condición de casos por categoría:", condicion_casos, "\n")

# Interpretación: cada categoría supera con amplitud los 5 casos en ambas zonas, de
# modo que la aproximación normal es adecuada.

x_resto <- tasa_alta$tasa_alta[tasa_alta$ZONA == ZONA_RESTO]
n_resto <- tasa_alta$creditos[tasa_alta$ZONA == ZONA_RESTO]
x_lima  <- tasa_alta$tasa_alta[tasa_alta$ZONA == ZONA_LIMA]
n_lima  <- tasa_alta$creditos[tasa_alta$ZONA == ZONA_LIMA]

ic_dif  <- ic_dif_proporciones(x_resto, n_resto, x_lima, n_lima)
prueba_dif <- prueba_z_dos_proporciones(x_resto, n_resto, x_lima, n_lima)

resultado_prop <- fila_prueba(
  "OE3", "z para dos proporciones: tasa mayor a 10 %, resto menos Lima y Callao",
  "diferencia de proporciones = 0", prueba_dif[["z"]], prueba_dif[["valor_p"]],
  ic_inferior = ic_dif[["inferior"]], ic_superior = ic_dif[["superior"]]
)
guardar_tabla(resultado_prop, "prueba_proporciones_zona", DIR_G3)

# Interpretación: H0: la diferencia de proporciones es 0; H1: es distinta. Con
# z = 43,64 y p < 0,001 se rechaza H0. Con 95 % de confianza, la proporción de
# créditos con tasa alta es entre 41,8 y 45,9 puntos porcentuales mayor fuera de Lima
# y Callao. La diferencia va más allá de la media: un crédito fuera de esa zona tiene
# unas cinco veces más frecuencia de tasa alta.
