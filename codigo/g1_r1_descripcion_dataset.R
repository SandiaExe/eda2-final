# ============================================================================
# g1_r1_descripcion_dataset.R
# g-i. Preparación y exploración de datos
# r1. Descripción del dataset (variables, tamaño, tipo de datos)
# ============================================================================

source("codigo/00_configuracion.R")


# ----------------------------------------------------------------------------
# 1. Lectura y tamaño del archivo original
# ----------------------------------------------------------------------------
datos_crudos <- leer_datos_crudos()

tamano <- tibble(
  medida = c("Filas del archivo", "Columnas", "Filas vacías",
             "Filas con información"),
  valor  = c(nrow(datos_crudos), ncol(datos_crudos),
             sum(es_fila_vacia(datos_crudos)),
             sum(!es_fila_vacia(datos_crudos)))
)
guardar_tabla(tamano, "t01_tamano_archivo")

# Interpretación: el archivo tiene 13 507 filas y 14 columnas, pero 4 160 filas
# (30,8 %) están completamente vacías. Solo 9 347 filas contienen información,
# por lo que las filas vacías deben eliminarse antes de cualquier cálculo.


# ----------------------------------------------------------------------------
# 2. Variables y tipos de datos
# ----------------------------------------------------------------------------
# Tipo estadístico de cada variable, según el diccionario de datos del FMV.
TIPO_ESTADISTICO <- c(
  FECHA_DESEMBOLSO     = "Temporal (aaaammdd)",
  PRODUCTO             = "Cualitativa nominal",
  DEPARTAMENTO         = "Cualitativa nominal",
  PROVINCIA            = "Cualitativa nominal",
  DISTRITO             = "Cualitativa nominal",
  UBIGEO               = "Cualitativa nominal (código)",
  IFI                  = "Cualitativa nominal",
  TIPO_IFI             = "Cualitativa nominal",
  MONTO_CREDITO        = "Cuantitativa continua (S/)",
  MONTO_CUOTA_INICIAL  = "Cuantitativa continua (S/)",
  PLAZOS               = "Cuantitativa discreta (meses)",
  TASA                 = "Cuantitativa continua (%)",
  MONTO_VALOR_VIVIENDA = "Cuantitativa continua (S/)",
  FECHA_CORTE          = "Temporal (aaaammdd)"
)

datos_con_info <- datos_crudos[!es_fila_vacia(datos_crudos), ]

# Recorre las columnas y arma la ficha de cada variable.
ficha <- vector("list", ncol(datos_con_info))
for (i in seq_along(datos_con_info)) {
  variable <- names(datos_con_info)[i]
  columna  <- datos_con_info[[i]]
  ficha[[i]] <- tibble(
    variable         = variable,
    tipo_en_R        = class(columna)[1],
    tipo_estadistico = TIPO_ESTADISTICO[[variable]],
    valores_faltantes = sum(is.na(columna)),
    valores_distintos = n_distinct(columna)
  )
}
ficha_variables <- bind_rows(ficha)
guardar_tabla(ficha_variables, "t02_ficha_variables")

# Interpretación: hay 7 variables cualitativas, 5 cuantitativas y 2 temporales.
# Ninguna columna tiene valores faltantes una vez descartadas las filas vacías.
# Las fechas están como números (aaaammdd) y deben convertirse a tipo fecha.
# UBIGEO se lee como texto porque es un código, no una cantidad. FECHA_CORTE
# tiene un solo valor, de modo que no aporta información al análisis. El
# diccionario de datos difiere del archivo en tres puntos: declara PRODUCTO como
# numérico, pero es texto; menciona Mi Terreno, que no aparece en 2024; y describe
# siete tipos de IFI, de los que solo cuatro colocaron créditos.


# ----------------------------------------------------------------------------
# 3. Registros por producto
# ----------------------------------------------------------------------------
por_producto <- datos_con_info %>%
  distinct() %>%
  group_by(PRODUCTO) %>%
  summarise(
    creditos        = n(),
    porcentaje      = round(100 * n() / nrow(distinct(datos_con_info)), 1),
    tasa_mediana    = median(TASA),
    vivienda_mediana = median(MONTO_VALOR_VIVIENDA),
    .groups = "drop"
  ) %>%
  arrange(desc(creditos))
guardar_tabla(por_producto, "t03_registros_por_producto")

# Interpretación: NMIV concentra 8 565 créditos (91,7 %). Los productos atienden
# segmentos distintos: FCTP tiene la tasa mediana más alta (15,00 %) y la vivienda
# mediana más barata (S/ 96 805), mientras que S-CRC tiene la vivienda mediana más
# cara (S/ 428 539). NMIV se ubica entre ambos (9,48 % y S/ 238 141). Estas
# diferencias confirman que los productos no deben analizarse en conjunto.

grafico_producto <- ggplot(por_producto,
                           aes(x = reorder(PRODUCTO, creditos), y = creditos)) +
  geom_col(fill = COLOR_PRINCIPAL, width = 0.6) +
  geom_text(aes(label = paste0(format(creditos, big.mark = " ", trim = TRUE), " (",
                               format(porcentaje, decimal.mark = ",", trim = TRUE),
                               " %)")),
            hjust = -0.1, size = 3.8) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Créditos colocados por producto, 2024",
       x = "Producto", y = "Número de créditos") +
  tema_informe()
guardar_grafico(grafico_producto, "f01_creditos_por_producto", alto = 3.5)

# Interpretación: el gráfico muestra el fuerte predominio de NMIV frente a FCTP
# (4,5 %) y S-CRC (3,8 %). Limitar el estudio a NMIV conserva la mayor parte de
# las colocaciones y evita confundir el efecto del producto con el de la IFI o
# la zona.
