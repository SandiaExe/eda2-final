# ============================================================================
# g4_2_regresion_multiple.R - Modelos progresivos y representación gráfica
# ============================================================================
source("codigo/g4_0_funciones.R", encoding = "UTF-8")
datos <- preparar_g4(); modelos <- construir_modelos(datos); m <- modelos$M4
# 1. Coeficientes y ajuste
for (nombre in names(modelos)[-1]) interpretar_g4(modelos[[nombre]], nombre)
tabla_g4(coef_g4(m), "coeficientes_modelo_multiple")
tabla_g4(bind_rows(lapply(modelos, coef_g4), .id = "modelo"), "coeficientes_modelos_progresivos")
# Interpretación: cada pendiente es una asociación manteniendo constantes los otros predictores.
# 2. Contrastes globales y por bloques; H0: todas las pendientes del bloque son cero.
resultados <- list()
for (nombre in names(modelos)) {
  mod <- modelos[[nombre]]
  for (rob in c(FALSE, TRUE)) {
    resultados[[length(resultados)+1]] <- contraste_g4(mod, names(coef(mod))[-1], rob) %>% mutate(modelo = nombre, bloque = "Global", metodo = if (rob) "HC3" else "Convencional")
  }
}
for (bloque in c("TIPO_IFI", "ZONA")) {
  for (rob in c(FALSE, TRUE)) resultados[[length(resultados)+1]] <- contraste_g4(m, grep(paste0("^", bloque), names(coef(m)), value = TRUE), rob) %>% mutate(modelo = "M4", bloque = bloque, metodo = if (rob) "HC3" else "Convencional")
}
tabla_g4(bind_rows(resultados), "significancia_global")
# Interpretación: el contraste conjunto evita confundir una categoría con todo el factor.
# 3. Observados frente a ajustados y coeficientes HC3.
limites <- range(c(datos$TASA, fitted(m)))
p <- ggplot(broom::augment(m), aes(.fitted, TASA)) +
  geom_point(color = COLOR_PRINCIPAL, alpha = .15) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_ATIPICO, linetype = 2) +
  coord_equal(xlim = limites, ylim = limites) +
  labs(x = "Tasa ajustada (%)", y = "Tasa observada (%)", caption = "Línea naranja: ajuste perfecto (y = x).") + tema_informe()
grafico_g4(p, "f21_observados_ajustados")
c <- filter(coef_g4(m, TRUE), term != "(Intercept)")
etiquetas <- c(CREDITO_10K = "Crédito (S/ 10 000)", PLAZO_ANIOS = "Plazo (años)",
  CUOTA_10K = "Cuota inicial (S/ 10 000)", TIPO_IFICMAC = "CMAC",
  TIPO_IFIFinanciera = "Financiera", TIPO_IFIEdpyme = "Edpyme",
  "ZONAResto del país" = "Resto del país")
stopifnot(all(c$term %in% names(etiquetas)))
c$etiqueta <- unname(etiquetas[c$term])
p <- ggplot(c, aes(estimate, reorder(etiqueta, estimate))) +
  geom_vline(xintercept = 0, linetype = 2, color = COLOR_REFERENCIA) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = reorder(etiqueta, estimate)), color = COLOR_PRINCIPAL) +
  geom_point(color = COLOR_PRINCIPAL) +
  labs(x = "Coeficiente e IC 95 % HC3 (pp)", y = NULL,
       caption = "Referencias: Banco y Lima y Callao.") + tema_informe()
grafico_g4(p, "f22_coeficientes_modelo_multiple")
# Interpretación: la diagonal indica ajuste perfecto; los IC HC3 no corrigen agrupación por IFI.
