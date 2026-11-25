# ============================================================================
# g4_0_funciones.R - g-iv. Modelos de regresión: funciones comunes
# ============================================================================
source("codigo/00_configuracion.R", encoding = "UTF-8")
PAQUETES_G4 <- c("broom", "car", "lmtest", "sandwich")
instalar_y_cargar(PAQUETES_G4)
SEMILLA <- 2026
ESCALA_MONTO <- 10000
N_MAX_SHAPIRO <- 5000
# 1. Preparación: no se modifica el CSV original.
preparar_g4 <- function() {
  d <- leer_datos_limpios()
  vars <- c(VARIABLES_NUMERICAS, "TIPO_IFI", "ZONA", "IFI")
  stopifnot(all(vars %in% names(d)), nrow(d) == 8565,
            !anyNA(d[, vars]), all(sapply(d[, VARIABLES_NUMERICAS], function(x) all(is.finite(x)))))
  d %>% mutate(fila = row_number(), CREDITO_10K = MONTO_CREDITO / ESCALA_MONTO,
               CUOTA_10K = MONTO_CUOTA_INICIAL / ESCALA_MONTO,
               PLAZO_ANIOS = PLAZOS / 12)
}
# 2. Modelos anidados, misma base y referencias explícitas.
construir_modelos <- function(d) {
  formulas <- list(M1 = TASA ~ CREDITO_10K,
                   M2 = TASA ~ CREDITO_10K + PLAZO_ANIOS + CUOTA_10K,
                   M3 = TASA ~ CREDITO_10K + PLAZO_ANIOS + CUOTA_10K + TIPO_IFI,
                   M4 = TASA ~ CREDITO_10K + PLAZO_ANIOS + CUOTA_10K + TIPO_IFI + ZONA)
  lapply(formulas, function(f) lm(f, data = d, na.action = na.fail,
    contrasts = if ("TIPO_IFI" %in% all.vars(f)) {
      if ("ZONA" %in% all.vars(f)) list(TIPO_IFI = "contr.treatment", ZONA = "contr.treatment") else list(TIPO_IFI = "contr.treatment")
    } else NULL))
}
tabla_g4 <- function(x, nombre) guardar_tabla(x, nombre, DIR_G4)
grafico_g4 <- function(x, nombre, ancho = 8, alto = 5) guardar_grafico(x, nombre, ancho, alto, DIR_G4)
coef_g4 <- function(m, robusto = FALSE) {
  if (!robusto) return(broom::tidy(m, conf.int = TRUE, conf.level = NIVEL_CONFIANZA))
  v <- sandwich::vcovHC(m, type = "HC3")
  b <- coef(m); se <- sqrt(diag(v)); t <- b/se; q <- qt(1-ALFA/2, df.residual(m))
  tibble(term = names(b), estimate = unname(b), std.error = unname(se),
         statistic = unname(t), p.value = unname(2*pt(-abs(t), df.residual(m))),
         conf.low = unname(b-q*se), conf.high = unname(b+q*se))
}
ajuste_g4 <- function(m) {
  broom::glance(m) %>% mutate(RMSE = sqrt(mean(residuals(m)^2)), MAE = mean(abs(residuals(m))))
}
# H0: los coeficientes del bloque son conjuntamente cero.
contraste_g4 <- function(m, terminos, robusto = FALSE) {
  L <- diag(length(coef(m)))[match(terminos, names(coef(m))), , drop = FALSE]
  colnames(L) <- names(coef(m))
  v <- if (robusto) sandwich::vcovHC(m, type = "HC3") else vcov(m)
  a <- car::linearHypothesis(m, L, vcov. = v, test = "F")
  tibble(gl1 = a$Df[2], gl2 = a$Res.Df[2], F = a$F[2], valor_p = a$`Pr(>F)`[2])
}
# Interpretaciones numéricas generadas desde el ajuste, no escritas de antemano.
interpretar_g4 <- function(m, nombre) {
  a <- ajuste_g4(m)
  cat(sprintf("%s: R² = %.4f; R² ajustado = %.4f; RMSE = %.4f pp.\n", nombre, a$r.squared, a$adj.r.squared, a$RMSE))
  for (i in seq_along(coef(m))) {
    cat(sprintf("%s: coeficiente %.6f pp (asociación ajustada, no causal).\n", names(coef(m))[i], coef(m)[i]))
  }
}
