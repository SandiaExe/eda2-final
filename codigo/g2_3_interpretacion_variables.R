# ============================================================================
# g2_3_interpretacion_variables.R
# g-ii. Análisis descriptivo
# Parte 3. Interpretación de los resultados por variable
# ============================================================================

source("codigo/00_configuracion.R")

ncmv <- leer_datos_limpios()
medidas <- read_csv(file.path(DIR_G2, "medidas_generales.csv"),
                    show_col_types = FALSE)

# Umbrales de asimetría según la convención de Bulmer (1979).
UMBRAL_ASIMETRIA_LEVE   <- 0.5
UMBRAL_ASIMETRIA_FUERTE <- 1


# ----------------------------------------------------------------------------
# 1. Funciones
# ----------------------------------------------------------------------------
# Clasifica la forma de la distribución según su coeficiente de asimetría.
clasificar_asimetria <- function(valor) {
  magnitud <- abs(valor)
  if (magnitud < UMBRAL_ASIMETRIA_LEVE) {
    return("aproximadamente simétrica")
  }
  sentido <- if (valor > 0) "positiva" else "negativa"
  if (magnitud < UMBRAL_ASIMETRIA_FUERTE) {
    paste("asimetría moderada", sentido)
  } else {
    paste("asimetría fuerte", sentido)
  }
}

# Clasifica el apuntamiento según el exceso de curtosis.
clasificar_curtosis <- function(valor) {
  if (valor > 0) {
    "leptocúrtica"
  } else if (valor < 0) {
    "platicúrtica"
  } else {
    "mesocúrtica"
  }
}


# ----------------------------------------------------------------------------
# 2. Clasificación de las variables numéricas
# ----------------------------------------------------------------------------
clasificacion <- vector("list", nrow(medidas))
for (i in seq_len(nrow(medidas))) {
  fila <- medidas[i, ]
  clasificacion[[i]] <- tibble(
    variable   = fila$variable,
    forma      = clasificar_asimetria(fila$asimetria),
    apuntamiento = clasificar_curtosis(fila$curtosis),
    medida_central = if_else(abs(fila$asimetria) < UMBRAL_ASIMETRIA_LEVE,
                             "media", "mediana")
  )
}
tabla_clasificacion <- bind_rows(clasificacion)
guardar_tabla(tabla_clasificacion, "clasificacion_variables", DIR_G2)

# Interpretación por variable:
# TASA: asimetría fuerte positiva y leptocúrtica. La mediana (9,48 %) la representa
# mejor que la media. La mayoría paga cerca de 9,5 %, pero una minoría, sobre todo
# de CMAC, paga bastante más. Es la variable de respuesta y no se ajusta a una forma
# simétrica, por lo que deben verificarse los supuestos antes de cada prueba.
# MONTO_CREDITO: aproximadamente simétrica y platicúrtica. La media (S/ 181 797) es
# representativa, aunque la dispersión es alta (CV de 39,95 %): el programa atiende
# desde créditos pequeños hasta cerca de S/ 320 000.
# MONTO_CUOTA_INICIAL: asimetría fuerte positiva y leptocúrtica. La mediana
# (S/ 32 260) resume mejor a la familia típica; unos pocos créditos tienen cuotas
# muy altas, de hasta S/ 265 000.
# PLAZOS: asimetría moderada negativa y platicúrtica. La mediana y la moda son 240
# meses (20 años). Se concentra en plazos redondos de 10, 15, 20 y 25 años.
# MONTO_VALOR_VIVIENDA: aproximadamente simétrica y platicúrtica. Media (S/ 239 720)
# y mediana (S/ 238 141) casi coinciden, y el máximo es S/ 355 100.
# TIPO_IFI: variable nominal dominada por los bancos (85,88 %). Su categoría de
# mayor frecuencia también es la de menor tasa media.
# ZONA: variable nominal; 67,80 % de los créditos está en Lima y Callao.
# IFI: variable nominal muy concentrada: BCP coloca 34,06 % de los créditos.


# ----------------------------------------------------------------------------
# 3. Tasa según tipo de IFI y zona en conjunto
# ----------------------------------------------------------------------------
tasa_ifi_zona <- ncmv %>%
  group_by(TIPO_IFI, ZONA) %>%
  summarise(creditos = n(), tasa_media = round(mean(TASA), 2),
            tasa_mediana = median(TASA), .groups = "drop")
guardar_tabla(tasa_ifi_zona, "tasa_por_tipo_ifi_y_zona", DIR_G2)

# Interpretación: en todos los tipos de IFI la tasa media es mayor fuera de Lima y
# Callao. La combinación de tasa media más baja es banco en Lima y Callao (9,28 %)
# y la más alta, CMAC en el resto del país (12,92 %).

brecha_zona <- tasa_ifi_zona %>%
  select(TIPO_IFI, ZONA, tasa_media) %>%
  pivot_wider(names_from = ZONA, values_from = tasa_media) %>%
  mutate(brecha_resto_menos_lima = .data[[ZONA_RESTO]] - .data[[ZONA_LIMA]])
guardar_tabla(brecha_zona, "brecha_zona_por_tipo_ifi", DIR_G2)

# Interpretación: la brecha entre zonas va de 0,29 puntos en CMAC a 1,75 en
# financieras, y es de 0,69 en bancos. Dentro de un mismo tipo de IFI, la brecha
# suele ser menor que la brecha general de 1,29 puntos (salvo en financieras).

composicion_zona <- ncmv %>%
  count(ZONA, TIPO_IFI, name = "creditos") %>%
  group_by(ZONA) %>%
  mutate(porcentaje_zona = round(100 * creditos / sum(creditos), 1)) %>%
  ungroup()
guardar_tabla(composicion_zona, "composicion_tipo_ifi_por_zona", DIR_G2)

# Interpretación: las CMAC representan 20,6 % de los créditos fuera de Lima y Callao
# y solo 2,1 % en esa zona. Los bancos pasan de 92,4 % a 72,2 %. Fuera de Lima pesan
# más las entidades de tasa más alta, lo que ayuda a explicar parte de la brecha
# general. La brecha no desaparece dentro de cada tipo, de modo que el tipo de IFI
# por sí solo no la explica por completo. Es una lectura descriptiva, no causal.

grafico_ifi_zona <- ggplot(tasa_ifi_zona,
                           aes(x = TIPO_IFI, y = tasa_media, fill = ZONA)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  geom_text(aes(label = format(tasa_media, decimal.mark = ",", nsmall = 2,
                               trim = TRUE)),
            position = position_dodge(width = 0.72), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = COLORES_ZONA, name = NULL) +
  scale_y_continuous(labels = label_number(decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Tasa media del NCMV según tipo de IFI y zona, 2024",
       x = NULL, y = "Tasa media (%)") +
  tema_informe() +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank())
guardar_grafico(grafico_ifi_zona, "f14_tasa_tipo_ifi_zona", alto = 4.5,
                directorio = DIR_G2)

# Interpretación: el gráfico muestra que el tipo de IFI marca diferencias mayores
# que la zona: entre bancos y CMAC la tasa media difiere 3,35 puntos en Lima y
# Callao y 2,95 en el resto del país, mientras que entre zonas, dentro de un mismo
# tipo, difiere entre 0,29 y 1,75 puntos.
