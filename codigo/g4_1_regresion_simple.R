# ============================================================================
# g4_1_regresion_simple.R - Selección de variables y regresión simple
# ============================================================================
source("codigo/g4_0_funciones.R", encoding = "UTF-8")
datos <- preparar_g4()
# 1. Selección y exploración
seleccion <- tibble(variable = c("TASA", "MONTO_CREDITO", "PLAZOS", "MONTO_CUOTA_INICIAL", "TIPO_IFI", "ZONA", "MONTO_VALOR_VIVIENDA", "IFI", "PLAZO_ANIOS", "ZONA_LIMA_CALLAO", "PCT_CUOTA_INICIAL"),
 uso = c("Respuesta", "M1-M4", "M2-M4 (años)", "M2-M4", "M3-M4", "M4", "Excluida", "Diagnóstico", "Transformación", "Excluida", "Exploración"),
 motivo = c("OE6", "OE6; escala S/ 10000", "OE6", "OE6; escala S/ 10000", "OE6; referencia Banco", "OE6; referencia Lima y Callao", "Redundancia potencial con crédito", "Agrupación institucional", "Equivalente a PLAZOS", "Equivalente a ZONA", "Derivada de montos"))
tabla_g4(seleccion, "seleccion_variables")
tabla_g4(tibble(n = nrow(datos), faltantes = sum(is.na(datos)), duplicados = sum(duplicated(select(datos, -fila)))), "validacion_base_modelado")
tabla_g4(count(datos, TIPO_IFI, ZONA), "niveles_factores")
tabla_g4(tibble::rownames_to_column(as.data.frame(cor(datos[, VARIABLES_NUMERICAS])), "variable"), "correlaciones_variables")
# Interpretación: las correlaciones orientan la selección; no demuestran causalidad.
# 2. Modelo simple: H0 pendiente = 0; H1 pendiente distinta de 0.
m <- construir_modelos(datos)$M1
tabla_g4(coef_g4(m), "coeficientes_modelo_simple")
tabla_g4(ajuste_g4(m), "resumen_modelo_simple")
interpretar_g4(m, "M1")
# Interpretación: la pendiente mide el cambio asociado a S/ 10000 adicionales.
# El intercepto corresponde a crédito cero, fuera del rango observado.
# 3. Representación gráfica (IC convencional de la media ajustada).
p <- ggplot(datos, aes(CREDITO_10K, TASA)) + geom_point(color = COLOR_PRINCIPAL, alpha = .15, size = .8) +
  geom_smooth(method = "lm", formula = y ~ x, level = NIVEL_CONFIANZA, color = COLOR_ATIPICO) +
  labs(x = "Monto del crédito (S/ 10 000)", y = "Tasa (%)") + tema_informe()
grafico_g4(p, "f20_regresion_simple")
# Interpretación: la banda representa incertidumbre convencional de la media, no predicción individual.
