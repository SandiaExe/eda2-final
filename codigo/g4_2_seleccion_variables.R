# ============================================================================
# g4_2_seleccion_variables.R
# g-iv. Modelos de regresión
# Parte 2. Selección de variables para la regresión lineal múltiple (OE6)
# ============================================================================

source("codigo/g4_0_funciones.R")

ncmv <- preparar_datos_modelo()


# ----------------------------------------------------------------------------
# 1. Variables candidatas
# ----------------------------------------------------------------------------
# Predictores planteados en el OE6. El valor de la vivienda se descarta de entrada:
# en g-i se verificó que es casi igual a la suma del crédito y la cuota inicial,
# de modo que repetiría la información de ambas.
CANDIDATAS <- c("CREDITO_10K", "PLAZO_ANIOS", "CUOTA_10K", "TIPO_IFI", "ZONA")

redundancia_vivienda <- tibble(
  correlacion_vivienda_credito = round(cor(ncmv$MONTO_VALOR_VIVIENDA,
                                           ncmv$MONTO_CREDITO), 3),
  correlacion_vivienda_suma = round(cor(ncmv$MONTO_VALOR_VIVIENDA,
                                        ncmv$MONTO_CREDITO + ncmv$MONTO_CUOTA_INICIAL), 4)
)
guardar_tabla(redundancia_vivienda, "redundancia_valor_vivienda", DIR_G4)

# Interpretación: el valor de la vivienda tiene una correlación de 0,945 con el
# crédito y de 0,9985 con la suma del crédito y la cuota inicial. Incluirlo junto a
# esas dos variables haría casi imposible separar sus efectos, por lo que se excluye.


# ----------------------------------------------------------------------------
# 2. Relación entre los predictores numéricos
# ----------------------------------------------------------------------------
# Una correlación alta entre predictores dificulta separar sus efectos.
correlacion_predictores <- as_tibble(
  round(cor(ncmv[, c("CREDITO_10K", "PLAZO_ANIOS", "CUOTA_10K")]), 3),
  rownames = "variable"
)
guardar_tabla(correlacion_predictores, "correlacion_predictores", DIR_G4)

# Interpretación: las correlaciones entre los predictores numéricos son bajas o
# moderadas. La mayor es la del crédito con el plazo (0,477): los créditos grandes
# suelen pactarse a más años. Ninguna es tan alta como para impedir incluirlos juntos.


# ----------------------------------------------------------------------------
# 3. Eliminación hacia atrás
# ----------------------------------------------------------------------------
# Se parte del modelo con todas las candidatas y, en cada paso, se evalúa cada
# variable con la prueba F parcial (H0: la variable no aporta al modelo; H1: sí
# aporta). Si la de mayor valor p supera α, se elimina y se repite el proceso.
seleccionadas <- CANDIDATAS
pasos <- list()
paso <- 1
repeat {
  formula_actual <- reformulate(seleccionadas, response = "TASA")
  pruebas <- drop1(lm(formula_actual, data = ncmv), test = "F")[-1, ]
  peor <- rownames(pruebas)[which.max(pruebas$`Pr(>F)`)]
  p_peor <- max(pruebas$`Pr(>F)`)
  pasos[[paso]] <- tibble(paso = paso, variable = rownames(pruebas),
                          gl = pruebas$Df, F = round(pruebas$`F value`, 2),
                          valor_p = sapply(pruebas$`Pr(>F)`, formatear_p),
                          eliminada = rownames(pruebas) == peor & p_peor > ALFA)
  if (p_peor <= ALFA) break
  seleccionadas <- setdiff(seleccionadas, peor)
  paso <- paso + 1
}
seleccion <- bind_rows(pasos)
guardar_tabla(seleccion, "seleccion_variables", DIR_G4)

variables_finales <- tibble(variable = seleccionadas)
guardar_tabla(variables_finales, "variables_modelo_final", DIR_G4)

# Interpretación: en el primer paso, la cuota inicial no aporta al modelo una vez
# considerados los demás predictores (F = 0,87; p = 0,351), así que se elimina. En
# el segundo paso, las cuatro variables restantes son significativas (p < 0,001):
# crédito, plazo, tipo de IFI (F = 2 379,56, la de mayor aporte) y zona. El modelo
# final explica la tasa con esas cuatro variables. Que la cuota inicial no aporte
# concuerda con su correlación casi nula con la tasa (-0,086) en la Parte 1.
