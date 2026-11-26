# ============================================================================
# g4_3_regresion_multiple.R
# g-iv. Modelos de regresión
# Parte 3. Regresión lineal múltiple: ajuste, validación y significancia (OE6)
# ============================================================================

source("codigo/g4_0_funciones.R")

ncmv <- preparar_datos_modelo()

# Variables elegidas en la Parte 2.
variables_finales <- read_csv(file.path(DIR_G4, "variables_modelo_final.csv"),
                              show_col_types = FALSE)$variable


# ----------------------------------------------------------------------------
# 1. Ajuste del modelo
# ----------------------------------------------------------------------------
formula_multiple <- reformulate(variables_finales, response = "TASA")
modelo_multiple  <- lm(formula_multiple, data = ncmv)
print(formula_multiple)


# ----------------------------------------------------------------------------
# 2. Validación de los residuos
# ----------------------------------------------------------------------------
validacion_multiple <- validar_residuos(modelo_multiple, "Múltiple")
guardar_tabla(validacion_multiple, "validacion_residuos_multiple", DIR_G4)

guardar_grafico(grafico_residuos(modelo_multiple), "f23_residuos_multiple",
                directorio = DIR_G4)
guardar_grafico(grafico_qq_residuos(modelo_multiple), "f24_qq_residuos_multiple",
                ancho = 7, alto = 5, directorio = DIR_G4)

# Interpretación: los residuos siguen sin ser normales (W = 0,9050; p < 0,001) ni
# tener varianza constante (Levene: F = 363,21; p < 0,001). El gráfico de residuos
# muestra dos grupos: los bancos, financieras y Edpyme, con tasas ajustadas entre 9 %
# y 11,5 %, y las CMAC, entre 12 % y 13,3 %, cuyos residuos son mucho más dispersos.
# Las franjas diagonales provienen de tasas que se repiten en valores fijos. Frente
# al modelo simple, los residuos extremos bajan de 253 a 95 (1,11 %), lo que indica
# un mejor ajuste. Como en g-iii, el tamaño de muestra respalda la estimación de los
# coeficientes, pero sus errores estándar y valores p son aproximados.


# ----------------------------------------------------------------------------
# 3. Coeficientes y su significancia
# ----------------------------------------------------------------------------
coeficientes_multiple <- tabla_coeficientes(modelo_multiple, "Múltiple")
guardar_tabla(coeficientes_multiple, "coeficientes_modelo_multiple", DIR_G4)

# Interpretación: la ecuación estimada es TASA = 10,367 - 0,0302 × CREDITO_10K -
# 0,0232 × PLAZO_ANIOS + 2,659 × CMAC + 0,512 × Financiera + 0,954 × Edpyme +
# 0,447 × Resto del país. Para cada coeficiente, H0: es 0; H1: es distinto de 0.
# Los siete coeficientes son significativos (p < 0,001). Manteniendo constantes las
# demás variables:
# - Cada S/ 10 000 adicionales de crédito se asocian con 0,030 puntos menos de tasa,
#   [-0,033; -0,028]. Es un tercio de lo estimado en el modelo simple (0,0945): parte
#   de esa relación se debía a que las CMAC otorgan créditos más pequeños.
# - Cada año adicional de plazo se asocia con 0,023 puntos menos [-0,027; -0,020].
# - Frente a un banco, las CMAC tienen 2,659 puntos más [2,597; 2,721], las Edpyme
#   0,954 y las financieras 0,512. El tipo de IFI marca las mayores diferencias.
# - Fuera de Lima y Callao, la tasa es 0,447 puntos mayor [0,410; 0,485].
# El intercepto (10,367 %) corresponde a un crédito de S/ 0 a 0 años, fuera del
# rango observado, por lo que no tiene interpretación práctica.


# ----------------------------------------------------------------------------
# 4. Bondad de ajuste y significancia global
# ----------------------------------------------------------------------------
ajuste_multiple <- tabla_ajuste(modelo_multiple, "Múltiple")
guardar_tabla(ajuste_multiple, "ajuste_modelo_multiple", DIR_G4)

# Interpretación: H0: ningún predictor explica la tasa; H1: al menos uno sí. Con
# F = 2 984,49 (gl = 6 y 8 558) y p < 0,001 se rechaza H0. El modelo explica el
# 67,66 % de la variación de la tasa (R² ajustado = 67,64 %), más del doble que el
# modelo simple (30,79 %), y el error estándar residual baja de 1,03 a 0,70 puntos.


# ----------------------------------------------------------------------------
# 5. Representación gráfica del modelo
# ----------------------------------------------------------------------------
# Tasa observada frente a la ajustada, por tipo de IFI.
observados <- ncmv %>% mutate(ajustada = fitted(modelo_multiple))
grafico_observados <- ggplot(observados, aes(x = ajustada, y = TASA, color = TIPO_IFI)) +
  geom_point(alpha = 0.35, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_REFERENCIA,
              linetype = "dashed") +
  scale_color_manual(values = COLORES_TIPO_IFI, name = "Tipo de IFI") +
  labs(title = "Tasa observada frente a la tasa ajustada",
       x = "Tasa ajustada (%)", y = "Tasa observada (%)") +
  tema_informe() +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
guardar_grafico(grafico_observados, "f25_observados_ajustados", directorio = DIR_G4)

# Interpretación: en los bancos, los puntos se agrupan cerca de la diagonal, es
# decir, el modelo predice bien su tasa. En las CMAC, el modelo asigna tasas de 12 %
# a 13,3 %, pero las observadas van de 8,55 % a 23,89 %: el modelo capta el nivel
# medio de cada tipo de IFI, no la variación dentro de las CMAC.

# Coeficientes con su intervalo de confianza del 95 %, sin el intercepto.
coeficientes_grafico <- coeficientes_multiple %>%
  filter(termino != "Intercepto") %>%
  mutate(termino = factor(termino, levels = rev(termino)))
grafico_coeficientes <- ggplot(coeficientes_grafico,
                               aes(x = coeficiente, y = termino)) +
  geom_vline(xintercept = 0, color = COLOR_REFERENCIA, linetype = "dashed") +
  geom_errorbar(aes(xmin = ic95_inferior, xmax = ic95_superior),
                width = 0.2, orientation = "y", color = COLOR_PRINCIPAL) +
  geom_point(color = COLOR_PRINCIPAL, size = 2.5) +
  geom_text(aes(label = con_coma(coeficiente, 3)), vjust = -1, size = 3.2) +
  labs(title = "Coeficientes del modelo múltiple",
       x = "Cambio en la tasa (puntos)", y = NULL) +
  tema_informe()
guardar_grafico(grafico_coeficientes, "f26_coeficientes_multiple", alto = 4.5,
                directorio = DIR_G4)

# Interpretación: ningún intervalo incluye el cero. La diferencia asociada a las
# CMAC (2,659 puntos) supera con amplitud al resto. Los coeficientes del crédito y del plazo se
# ven pequeños porque están medidos por cada S/ 10 000 y por cada año: entre un
# crédito de S/ 100 000 a 10 años y uno de S/ 250 000 a 25 años, la diferencia
# estimada es de 0,80 puntos (15 × 0,030 + 15 × 0,023).
