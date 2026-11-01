# ============================================================================
# g3_4_chi_cuadrado.R
# g-iii. Inferencia estadística
# Parte 4. Prueba Chi-cuadrado de independencia entre tipo de IFI y zona (OE5)
# ============================================================================

source("codigo/g3_0_funciones.R")

ncmv <- leer_datos_limpios()


# ----------------------------------------------------------------------------
# 1. Tabla de contingencia y verificación de supuestos
# ----------------------------------------------------------------------------
contingencia <- table(TIPO_IFI = ncmv$TIPO_IFI, ZONA = ncmv$ZONA)
prueba_chi   <- chisq.test(contingencia, correct = FALSE)

# Pasa una matriz de la prueba a formato de tabla.
matriz_a_tabla <- function(matriz) {
  as.data.frame.matrix(matriz) %>%
    mutate(TIPO_IFI = rownames(matriz), .before = 1) %>%
    as_tibble()
}

guardar_tabla(matriz_a_tabla(contingencia), "chi_observadas", DIR_G3)
guardar_tabla(matriz_a_tabla(round(prueba_chi$expected, 1)), "chi_esperadas", DIR_G3)

# Supuesto: todas las frecuencias esperadas deben ser al menos 5.
minima_esperada <- min(prueba_chi$expected)
cumple_esperadas <- minima_esperada >= MIN_ESPERADA_CHI
cat("\nFrecuencia esperada mínima:", round(minima_esperada, 1),
    "- cumple:", cumple_esperadas, "\n")

# Interpretación: la menor frecuencia esperada es 50,6, muy por encima de 5, y cada
# crédito pertenece a una sola celda, de modo que la prueba es válida.


# ----------------------------------------------------------------------------
# 2. Prueba Chi-cuadrado de independencia
# ----------------------------------------------------------------------------
resultado_chi <- fila_prueba(
  "OE5", "Chi-cuadrado de independencia: tipo de IFI y zona",
  "el tipo de IFI y la zona son independientes",
  unname(prueba_chi$statistic), prueba_chi$p.value,
  gl = unname(prueba_chi$parameter)
)
guardar_tabla(resultado_chi, "prueba_chi_cuadrado", DIR_G3)

# Interpretación: H0: el tipo de IFI y la zona son independientes; H1: están
# asociados. Con chi-cuadrado = 900,45, gl = 3 y p < 0,001 se rechaza H0. El tipo de
# entidad que coloca el crédito depende de la zona.


# ----------------------------------------------------------------------------
# 3. Residuos estandarizados
# ----------------------------------------------------------------------------
# Un valor mayor a 2 en valor absoluto señala una celda que se aleja de la
# independencia.
guardar_tabla(matriz_a_tabla(round(prueba_chi$stdres, 2)), "chi_residuos", DIR_G3)

# Interpretación: los bancos están sobrerrepresentados en Lima y Callao (residuo de
# 25,09) y las CMAC, fuera de esa zona (29,30), donde colocaron 567 créditos frente
# a 222 esperados. Las Edpyme también pesan más fuera de Lima (4,73). Las financieras
# no se alejan de la independencia (0,62).

composicion <- ncmv %>%
  count(ZONA, TIPO_IFI, name = "creditos") %>%
  group_by(ZONA) %>%
  mutate(porcentaje = 100 * creditos / sum(creditos)) %>%
  ungroup() %>%
  mutate(TIPO_IFI = factor(TIPO_IFI, levels = rev(NIVELES_TIPO_IFI)))

grafico_composicion <- ggplot(composicion,
                              aes(x = porcentaje, y = ZONA, fill = TIPO_IFI)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = if_else(porcentaje >= 10,
                                paste0(format(round(porcentaje, 1),
                                              decimal.mark = ",", trim = TRUE),
                                       " %"), "")),
            position = position_stack(vjust = 0.5), size = 3.2) +
  scale_fill_manual(values = COLORES_TIPO_IFI, name = "Tipo de IFI",
                    breaks = NIVELES_TIPO_IFI) +
  scale_x_continuous(labels = label_number(suffix = " %"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Composición de los créditos por tipo de IFI según la zona",
       x = "Porcentaje de los créditos de la zona", y = NULL) +
  tema_informe() +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank())
guardar_grafico(grafico_composicion, "f19_composicion_zona", alto = 3.6,
                directorio = DIR_G3)

# Interpretación: fuera de Lima y Callao, la participación de las CMAC (20,6 %) es
# casi diez veces la de esa zona (2,1 %). Esta asociación confirma la sospecha del
# problema: comparar zonas también compara, en parte, tipos de entidad. Por ello, las
# diferencias entre zonas y entre tipos no deben leerse como efectos aislados.
