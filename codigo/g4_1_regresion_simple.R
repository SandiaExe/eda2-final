# ============================================================================
# g4_1_regresion_simple.R
# g-iv. Modelos de regresión
# Parte 1. Regresión lineal simple (OE6)
# ============================================================================

source("codigo/g4_0_funciones.R")

ncmv <- preparar_datos_modelo()


# ----------------------------------------------------------------------------
# 1. Elección de la variable independiente
# ----------------------------------------------------------------------------
correlaciones <- cor(ncmv[, VARIABLES_NUMERICAS])
tabla_correlaciones <- as_tibble(round(correlaciones, 3), rownames = "variable")
guardar_tabla(tabla_correlaciones, "correlaciones_variables", DIR_G4)

# Se elige el predictor con mayor correlación absoluta con la tasa.
correlacion_tasa <- correlaciones["TASA", setdiff(VARIABLES_NUMERICAS, "TASA")]
predictor <- names(which.max(abs(correlacion_tasa)))
cat("\nPredictor elegido:", predictor, "\n")

# Interpretación: el monto del crédito tiene la mayor correlación absoluta con la
# tasa (r = -0,555), seguido del valor de la vivienda (-0,543) y el plazo (-0,478).
# La cuota inicial casi no se relaciona con la tasa (-0,086). La relación es
# negativa: los créditos más grandes tienden a tener tasas menores.


# ----------------------------------------------------------------------------
# 2. Linealidad: diagrama de dispersión con la recta ajustada
# ----------------------------------------------------------------------------
grafico_simple <- ggplot(ncmv, aes(x = CREDITO_10K, y = TASA)) +
  geom_point(color = COLOR_PRINCIPAL, alpha = 0.2, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, level = NIVEL_CONFIANZA,
              color = COLOR_ATIPICO, linewidth = 1) +
  labs(title = "Tasa de interés según el monto del crédito",
       x = "Monto del crédito (S/ 10 000)", y = "Tasa (%)") +
  tema_informe()
guardar_grafico(grafico_simple, "f20_regresion_simple", directorio = DIR_G4)

# Interpretación: la nube de puntos baja de izquierda a derecha, de modo que una
# recta resume razonablemente la tendencia. La dispersión es mayor en los créditos
# pequeños, donde aparecen franjas horizontales de tasas repetidas entre 12,5 % y
# 15 %, como 14,98 % y 12,68 %. Casi todas corresponden a CMAC (424 de los 449
# créditos con tasa de 12,5 % o más), junto con los atípicos documentados en g-i.


# ----------------------------------------------------------------------------
# 3. Ajuste del modelo
# ----------------------------------------------------------------------------
modelo_simple <- lm(TASA ~ CREDITO_10K, data = ncmv)

coeficientes_simple <- tabla_coeficientes(modelo_simple, "Simple")
guardar_tabla(coeficientes_simple, "coeficientes_modelo_simple", DIR_G4)

# Interpretación: la recta estimada es TASA = 11,514 - 0,0945 × CREDITO_10K.
# H0: la pendiente es 0; H1: es distinta de 0. Con t = -61,73 y p < 0,001 se
# rechaza H0. Por cada S/ 10 000 adicionales de crédito, la tasa estimada baja en
# promedio 0,0945 puntos, con un intervalo del 95 % de [-0,0975; -0,0915]; por
# ejemplo, entre un crédito de S/ 100 000 y uno de S/ 200 000 la diferencia es de
# casi 1 punto. El intercepto corresponde a un crédito de S/ 0, fuera del rango
# observado, por lo que no tiene interpretación práctica.


# ----------------------------------------------------------------------------
# 4. Validación de los residuos
# ----------------------------------------------------------------------------
validacion_simple <- validar_residuos(modelo_simple, "Simple")
guardar_tabla(validacion_simple, "validacion_residuos_simple", DIR_G4)

guardar_grafico(grafico_residuos(modelo_simple), "f21_residuos_simple",
                directorio = DIR_G4)
guardar_grafico(grafico_qq_residuos(modelo_simple), "f22_qq_residuos_simple",
                ancho = 7, alto = 5, directorio = DIR_G4)

# Interpretación: los residuos no son normales (W = 0,8443; p < 0,001): el gráfico
# Q-Q muestra una cola derecha larga, es decir, créditos con tasas mucho mayores que
# las que predice la recta. La varianza tampoco es constante (Levene entre cinco
# grupos de valores ajustados: F = 370,65; p < 0,001): la dispersión crece con la
# tasa ajustada, es decir, en los créditos pequeños. En el gráfico de residuos se ven
# franjas paralelas, señal de que falta una variable que separe grupos de entidades.
# Hay 253 residuos estandarizados mayores a 3 en valor absoluto (2,95 %). Con
# 8 565 créditos, la pendiente estimada sigue siendo confiable por el tamaño de
# muestra, pero sus errores estándar deben leerse con cautela.


# ----------------------------------------------------------------------------
# 5. Bondad de ajuste y significancia global
# ----------------------------------------------------------------------------
ajuste_simple <- tabla_ajuste(modelo_simple, "Simple")
guardar_tabla(ajuste_simple, "ajuste_modelo_simple", DIR_G4)

# Interpretación: H0: el modelo no explica la tasa (pendiente igual a 0); H1: sí la
# explica. Con F = 3 811,00 (gl = 1 y 8 563) y p < 0,001 se rechaza H0. El monto
# del crédito explica el 30,80 % de la variación de la tasa (R² ajustado = 30,79 %),
# y el error estándar residual es de 1,03 puntos. Es una relación clara, pero deja
# sin explicar casi el 70 % de la variación: hacen falta más variables.
