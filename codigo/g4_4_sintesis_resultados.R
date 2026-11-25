# ============================================================================
# g4_4_sintesis_resultados.R - Comparación de modelos y respuesta al OE6
# ============================================================================
source("codigo/g4_0_funciones.R", encoding = "UTF-8")
datos <- preparar_g4(); modelos <- construir_modelos(datos)
# 1. Comparación de ajuste en la misma muestra
comparacion <- bind_rows(lapply(modelos, ajuste_g4), .id = "modelo")
tabla_g4(comparacion, "comparacion_modelos")
bloques <- list()
for (i in 2:length(modelos)) {
  anterior <- modelos[[i-1]]; actual <- modelos[[i]]
  nuevos <- setdiff(names(coef(actual)), names(coef(anterior)))
  for (rob in c(FALSE, TRUE)) bloques[[length(bloques)+1]] <- contraste_g4(actual, nuevos, rob) %>% mutate(comparacion = paste(names(modelos)[i-1], names(modelos)[i], sep = " vs "), metodo = if (rob) "HC3" else "Convencional")
}
tabla_g4(bind_rows(bloques), "contrastes_modelos_anidados")
p <- ggplot(comparacion, aes(modelo, adj.r.squared)) + geom_col(fill = COLOR_PRINCIPAL) + geom_text(aes(label = scales::percent(adj.r.squared, accuracy = .01)), vjust = -.5, size = 3.5) + scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) + labs(x = "Modelo", y = "R² ajustado") + tema_informe()
grafico_g4(p, "f26_comparacion_modelos")
# Interpretación: R² ajustado penaliza complejidad; RMSE y MAE son errores dentro de muestra.
# 2. Brecha geográfica: mismo sentido que G3, resto menos Lima.
a <- t.test(datos$TASA[datos$ZONA == ZONA_RESTO], datos$TASA[datos$ZONA == ZONA_LIMA], conf.level = NIVEL_CONFIANZA)
b <- filter(coef_g4(modelos$M4, TRUE), startsWith(term, "ZONA"))
brecha <- tibble(tipo = c("Bruta (Welch)", "Ajustada (HC3)"), estimate = c(unname(a$estimate[1]-a$estimate[2]), b$estimate), conf.low = c(a$conf.int[1], b$conf.low), conf.high = c(a$conf.int[2], b$conf.high))
tabla_g4(brecha, "brecha_geografica_ajustada")
p <- ggplot(brecha, aes(factor(tipo, levels = c("Bruta (Welch)", "Ajustada (HC3)")), estimate)) + geom_hline(yintercept = 0, linetype = 2) + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .15, color = COLOR_PRINCIPAL) + geom_point(size = 3, color = COLOR_ATIPICO) + labs(x = NULL, y = "Diferencia de tasas (pp), IC 95 %") + tema_informe()
grafico_g4(p, "f27_brecha_geografica_ajustada")
# Interpretación: el cambio de brecha tras ajuste no demuestra mediación ni causalidad.
# 3. Síntesis y trazabilidad
m <- modelos$M4
tabla_g4(coef_g4(m, TRUE) %>% mutate(modelo = "M4", alcance = "Asociación ajustada; HC3; no causal"), "sintesis_regresion")
interpretar_g4(m, "M4")
cat(sprintf("Brecha bruta: %.4f pp; ajustada: %.4f pp.\n", brecha$estimate[1], brecha$estimate[2]))
paquetes <- c(PAQUETES, PAQUETES_G4)
if (requireNamespace("clubSandwich", quietly = TRUE)) paquetes <- c(paquetes, "clubSandwich")
tabla_g4(tibble(paquete = paquetes, version = sapply(paquetes, function(p) as.character(packageVersion(p)))), "versiones_paquetes_g4")
writeLines(c(capture.output(sessionInfo()), "IA: ChatGPT (OpenAI); prompt R4 en prompts/."), file.path(DIR_G4, "entorno_g4.txt"))
# Limitaciones: registros administrativos 2024, variables de riesgo ausentes y posible dependencia por IFI.
# Las interpretaciones finales requieren revisar las salidas reales y los gráficos.
