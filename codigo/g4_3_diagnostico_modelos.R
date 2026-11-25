# ============================================================================
# g4_3_diagnostico_modelos.R - Supuestos, influencia e inferencia robusta
# ============================================================================
source("codigo/g4_0_funciones.R", encoding = "UTF-8")
datos <- preparar_g4(); modelos <- construir_modelos(datos)[c("M1", "M4")]
# 1. Diagnóstico de residuos y sensibilidad
res <- list(); infl <- list(); diag <- list(); sens <- list(); robustos <- list(); agrupados <- list()
for (nombre in names(modelos)) {
  m <- modelos[[nombre]]; r <- residuals(m); set.seed(SEMILLA)
  usados <- if (length(r) > N_MAX_SHAPIRO) sample(r, N_MAX_SHAPIRO) else r
  sw <- shapiro.test(usados); bp <- lmtest::bptest(m); cook <- cooks.distance(m)
  diag[[nombre]] <- tibble(modelo = nombre, n = length(r), n_shapiro = length(usados), W = unname(sw$statistic), p_shapiro = sw$p.value, BP = unname(bp$statistic), p_BP = bp$p.value, umbral_cook = 4/length(r), n_influyentes = sum(cook > 4/length(r)))
  res[[nombre]] <- tibble(modelo = nombre, fila = datos$fila, ajustado = fitted(m), residuo = r, estandarizado = rstandard(m), cook = cook)
  infl[[nombre]] <- res[[nombre]] %>% filter(cook > 4/nrow(datos) | abs(estandarizado) > 3)
  robustos[[nombre]] <- coef_g4(m, TRUE)
  # Sensibilidad: exclusión exploratoria de casos Cook > 4/n, nunca del modelo principal.
  reducido <- lm(formula(m), data = datos[cook <= 4/nrow(datos), ], contrasts = m$contrasts)
  sens[[nombre]] <- bind_rows(coef_g4(m, TRUE) %>% mutate(escenario = "Completo", n = nobs(m)), coef_g4(reducido, TRUE) %>% mutate(escenario = "Sin Cook > 4/n", n = nobs(reducido)))
  # Agrupación: CR2/Satterthwaite opcional; pocos grupos y desbalance limitan la inferencia.
  if (requireNamespace("clubSandwich", quietly = TRUE)) {
    agrupados[[nombre]] <- tryCatch(as.data.frame(clubSandwich::coef_test(m, vcov = "CR2", cluster = datos$IFI, test = "Satterthwaite")) %>% tibble::rownames_to_column("coeficiente"), error = function(e) tibble(estado = conditionMessage(e)))
  } else agrupados[[nombre]] <- tibble(estado = "CR2 no ejecutado: instalar clubSandwich opcionalmente; HC3 no corrige dependencia por IFI")
}
tabla_g4(bind_rows(diag), "diagnostico_supuestos")
tabla_g4(bind_rows(infl), "observaciones_influyentes")
tabla_g4(bind_rows(robustos, .id = "modelo"), "coeficientes_robustos")
tabla_g4(bind_rows(sens, .id = "modelo"), "sensibilidad_modelos")
tabla_g4(bind_rows(agrupados, .id = "modelo"), "sensibilidad_agrupacion_ifi")
tabla_g4(datos %>% group_by(IFI, TIPO_IFI) %>% summarise(n = n(), .groups = "drop"), "agrupacion_ifi")
# Interpretación: Shapiro y BP se contrastan con gráficos; HC3 corrige heterocedasticidad, no dependencia.
# 2. Multicolinealidad (GVIF ajustado no se compara directamente con VIF).
v <- car::vif(modelos$M4)
tabla_g4(tibble::rownames_to_column(as.data.frame(v), "variable"), "multicolinealidad_vif")
# 3. Gráficos de diagnóstico para ambos modelos.
r <- bind_rows(res)
p <- ggplot(r, aes(ajustado, residuo)) + geom_point(alpha = .2, color = COLOR_PRINCIPAL) + geom_hline(yintercept = 0, linetype = 2) + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = COLOR_ATIPICO) + facet_wrap(~modelo, scales = "free") + labs(x = "Ajustados (%)", y = "Residuos (pp)") + tema_informe()
grafico_g4(p, "f23_residuos_ajustados", 10, 5)
p <- ggplot(r, aes(sample = estandarizado)) + stat_qq(alpha = .3, color = COLOR_PRINCIPAL) + stat_qq_line(color = COLOR_ATIPICO) + facet_wrap(~modelo) + labs(x = "Cuantiles teóricos", y = "Residuos estandarizados") + tema_informe()
grafico_g4(p, "f24_qq_residuos", 10, 5)
umbral <- 4/nrow(datos)
r <- r %>% mutate(influencia = if_else(cook > umbral, "Por encima de 4/n", "Hasta 4/n"))
destacados <- r %>% group_by(modelo) %>% slice_max(cook, n = 3, with_ties = FALSE) %>% ungroup()
p <- ggplot(r, aes(fila, cook)) +
  geom_segment(aes(xend = fila, y = 0, yend = cook, color = influencia), alpha = .55, linewidth = .25) +
  geom_hline(yintercept = umbral, color = COLOR_REFERENCIA, linetype = 2) +
  geom_point(data = destacados, color = COLOR_ATIPICO, size = 1.5) +
  geom_text(data = destacados, aes(label = fila), vjust = -0.7, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Hasta 4/n" = COLOR_PRINCIPAL, "Por encima de 4/n" = COLOR_ATIPICO)) +
  scale_y_continuous(expand = expansion(mult = c(0, .13))) +
  facet_wrap(~modelo) +
  labs(x = "Fila del CSV limpio", y = "Distancia de Cook", color = NULL,
       caption = sprintf("Línea discontinua: 4/n = %.6f. Etiquetas: filas con mayor influencia.", umbral)) +
  tema_informe() + theme(legend.position = "bottom")
grafico_g4(p, "f25_distancia_cook", 10, 5)
# Interpretación: curvatura, dispersión variable y colas alejadas requieren evaluación visual.
# La influencia no demuestra que un registro sea erróneo.
