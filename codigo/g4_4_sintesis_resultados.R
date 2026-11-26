# ============================================================================
# g4_4_sintesis_resultados.R
# g-iv. Modelos de regresión
# Parte 4. Comparación de modelos e interpretación contextualizada (OE6)
# ============================================================================

source("codigo/g4_0_funciones.R")

ncmv <- preparar_datos_modelo()


# ----------------------------------------------------------------------------
# 1. Comparación del modelo simple y el múltiple
# ----------------------------------------------------------------------------
ARCHIVOS_AJUSTE <- c("ajuste_modelo_simple", "ajuste_modelo_multiple")

ajustes <- list()
for (archivo in ARCHIVOS_AJUSTE) {
  ajustes[[archivo]] <- read_csv(file.path(DIR_G4, paste0(archivo, ".csv")),
                                 col_types = cols(.default = col_character()))
}
comparacion_modelos <- bind_rows(ajustes) %>%
  select(modelo, R2, R2_ajustado, error_estandar_residual, F, gl1, gl2, valor_p)
guardar_tabla(comparacion_modelos, "comparacion_modelos", DIR_G4)

# Interpretación: ambos modelos son significativos (p < 0,001), pero el múltiple
# explica bastante más: su R² ajustado sube de 30,79 % a 67,64 % y el error estándar
# residual baja de 1,03 a 0,70 puntos. Agregar el plazo, el tipo de IFI y la zona al
# monto del crédito mejora claramente la explicación de la tasa.


# ----------------------------------------------------------------------------
# 2. Diferencias de tasa antes y después de ajustar por las demás variables
# ----------------------------------------------------------------------------
# La diferencia bruta se estima con un modelo que solo contiene el factor; la
# ajustada, con el modelo múltiple de la Parte 3.
variables_finales <- read_csv(file.path(DIR_G4, "variables_modelo_final.csv"),
                              show_col_types = FALSE)$variable
modelo_multiple <- lm(reformulate(variables_finales, response = "TASA"), data = ncmv)

FACTORES <- c("ZONA", "TIPO_IFI")
diferencias <- list()
for (factor_analizado in FACTORES) {
  modelo_bruto <- lm(reformulate(factor_analizado, response = "TASA"), data = ncmv)
  diferencias[[paste(factor_analizado, "bruta")]] <-
    tabla_coeficientes(modelo_bruto, "Bruta")
}
diferencias[["ajustada"]] <- tabla_coeficientes(modelo_multiple, "Ajustada")

TERMINOS_COMPARADOS <- c("Resto del país", "CMAC", "Financiera", "Edpyme")
comparacion_diferencias <- bind_rows(diferencias) %>%
  filter(termino %in% TERMINOS_COMPARADOS) %>%
  select(termino, estimacion = modelo, diferencia = coeficiente,
         ic95_inferior, ic95_superior) %>%
  arrange(factor(termino, levels = TERMINOS_COMPARADOS),
          factor(estimacion, levels = c("Bruta", "Ajustada")))
guardar_tabla(comparacion_diferencias, "diferencias_brutas_ajustadas", DIR_G4)

grafico_diferencias <- comparacion_diferencias %>%
  mutate(termino = factor(termino, levels = rev(TERMINOS_COMPARADOS)),
         estimacion = factor(estimacion, levels = c("Bruta", "Ajustada"))) %>%
  ggplot(aes(x = diferencia, y = termino, color = estimacion)) +
  geom_vline(xintercept = 0, color = COLOR_REFERENCIA, linetype = "dashed") +
  geom_errorbar(aes(xmin = ic95_inferior, xmax = ic95_superior), width = 0.25,
                orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(Bruta = COLOR_REFERENCIA,
                                Ajustada = COLOR_PRINCIPAL), name = NULL) +
  labs(title = "Diferencias de tasa brutas y ajustadas",
       x = "Diferencia de tasa (puntos)", y = NULL,
       caption = "Referencias: Banco y Lima y Callao.") +
  tema_informe() +
  theme(legend.position = "top", legend.justification = "left")
guardar_grafico(grafico_diferencias, "f27_diferencias_brutas_ajustadas", alto = 4.5,
                directorio = DIR_G4)

# Interpretación: todas las diferencias se reducen al ajustar, pero ninguna
# desaparece y ningún intervalo incluye el cero:
# - Zona: la brecha entre el resto del país y Lima y Callao baja de 1,290 a 0,447
#   puntos (65 % menos). La mayor parte de la brecha se relaciona con que fuera de
#   Lima se concentran las CMAC (OE5) y los créditos más pequeños y cortos (g-ii).
# - CMAC frente a banco: baja de 3,402 a 2,659 puntos (22 % menos). Sigue siendo la
#   mayor diferencia, aun comparando créditos de igual monto, plazo y zona.
# - Financiera y Edpyme: bajan de 0,754 a 0,512 y de 1,311 a 0,954 puntos.
# La diferencia bruta de zona coincide con la de g-iii (1,290); su intervalo cambia
# levemente porque aquí se supone una varianza común y allí se usó Welch.


# ----------------------------------------------------------------------------
# 3. Supuestos verificados en los modelos
# ----------------------------------------------------------------------------
validaciones <- bind_rows(
  read_csv(file.path(DIR_G4, "validacion_residuos_simple.csv"), show_col_types = FALSE),
  read_csv(file.path(DIR_G4, "validacion_residuos_multiple.csv"), show_col_types = FALSE)
)
supuestos <- tibble(
  supuesto = c("Linealidad", "Independencia", "Normalidad de los residuos",
               "Varianza constante de los residuos"),
  verificacion = c("Diagrama de dispersión y residuos frente a ajustados",
                   "Diseño de los datos: cada crédito es un registro distinto",
                   "Shapiro-Wilk y gráfico Q-Q",
                   "Levene entre grupos de valores ajustados"),
  cumple = c("Aproximadamente", "Sí, con reserva por IFI",
             if (any(validaciones$cumple_normalidad)) "Sí" else "No",
             if (any(validaciones$cumple_homogeneidad)) "Sí" else "No")
)
guardar_tabla(supuestos, "supuestos_regresion", DIR_G4)

# Interpretación: la linealidad se cumple de forma aproximada y la independencia es
# razonable porque cada fila es un crédito distinto, aunque los créditos de una misma
# IFI comparten sus políticas de tasa. La normalidad y la varianza constante de los
# residuos no se cumplen. En consecuencia, los coeficientes siguen siendo una buena
# estimación de las diferencias promedio, pero sus errores estándar y valores p son
# aproximados. Como en g-iii, el tamaño de muestra (8 565 créditos) sostiene la
# inferencia sobre los coeficientes.


# ----------------------------------------------------------------------------
# 4. Respuesta al OE6
# ----------------------------------------------------------------------------
# ¿En qué medida el monto, el plazo, la cuota inicial, el tipo de IFI y la zona
# explican la tasa? En conjunto explican cerca de dos tercios de su variación
# (R² ajustado = 67,64 %). El tipo de IFI es el factor que más pesa: los créditos de
# las CMAC tienen en promedio 2,66 puntos más que los de bancos comparables. El monto y el
# plazo se asocian con tasas menores, y la cuota inicial no aporta una vez
# consideradas las demás variables. La zona conserva una diferencia de 0,45 puntos.
#
# Implicancia para el FMV: la brecha geográfica de 1,29 puntos observada en g-iii se
# explica en gran parte por el tipo de entidad que atiende en cada zona y por el
# tamaño de los créditos. Por ello, las medidas para reducir la tasa en regiones
# deberían enfocarse sobre todo en las condiciones de las CMAC, sin descuidar la
# diferencia que persiste por la zona.
#
# Limitaciones: el modelo describe asociaciones y no prueba que la entidad o la zona
# causen una mayor tasa. No incluye el ingreso ni el riesgo del cliente, que podrían
# explicar parte del tercio no explicado. Los residuos no son normales ni tienen
# varianza constante, por lo que los valores p son aproximados. Además, las
# financieras son solo dos entidades y las Edpyme una (Vivela), de modo que sus
# coeficientes reflejan a esas entidades más que a un sector.
