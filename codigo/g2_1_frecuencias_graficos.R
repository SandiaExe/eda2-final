# ============================================================================
# g2_1_frecuencias_graficos.R
# g-ii. Análisis descriptivo
# Parte 1. Tablas de frecuencia y gráficos
# ============================================================================

source("codigo/00_configuracion.R")

ncmv <- leer_datos_limpios()

# Número de departamentos que se muestran por separado.
TOP_DEPARTAMENTOS <- 10


# ----------------------------------------------------------------------------
# 1. Funciones
# ----------------------------------------------------------------------------
# Agrega porcentajes y frecuencias acumuladas a una tabla con columna fi.
agregar_acumuladas <- function(tabla) {
  tabla %>%
    mutate(hi_pct = round(100 * fi / sum(fi), 2),
           Fi = cumsum(fi),
           Hi_pct = round(100 * Fi / sum(fi), 2))
}

# Tabla de frecuencias de una variable cualitativa.
tabla_frecuencias <- function(datos, variable, ordenar = TRUE) {
  tabla <- datos %>%
    count(categoria = .data[[variable]], name = "fi")
  if (ordenar) {
    tabla <- arrange(tabla, desc(fi))
  }
  agregar_acumuladas(mutate(tabla, categoria = as.character(categoria)))
}

# Tabla de frecuencias por intervalos, con el número de clases de Sturges.
tabla_frecuencias_agrupada <- function(x) {
  cortes <- pretty(x, n = nclass.Sturges(x))
  tibble(intervalo = cut(x, breaks = cortes, right = FALSE,
                         include.lowest = TRUE, dig.lab = 6)) %>%
    count(intervalo, name = "fi", .drop = FALSE) %>%
    mutate(intervalo = as.character(intervalo)) %>%
    agregar_acumuladas()
}


# ----------------------------------------------------------------------------
# 2. Tablas de frecuencia de las variables cualitativas
# ----------------------------------------------------------------------------
VARIABLES_CUALITATIVAS <- c("TIPO_IFI", "ZONA", "IFI")

for (variable in VARIABLES_CUALITATIVAS) {
  guardar_tabla(tabla_frecuencias(ncmv, variable),
                paste0("frecuencias_", tolower(variable)), DIR_G2)
}

# Interpretación: TIPO_IFI: predominan los bancos (85,88 %). ZONA: 67,80 % de los
# créditos está en Lima y Callao. IFI: las 16 entidades concentran mucho la oferta;
# BCP coloca 34,06 % y, junto con BBVA e Interbank, llega a 65,62 %. CMAC Maynas
# tiene un solo crédito, por lo que no permite conclusiones por entidad.

# Los departamentos fuera de los diez primeros se agrupan en «Otros».
frecuencias_departamento <- tabla_frecuencias(ncmv, "DEPARTAMENTO")
principales <- frecuencias_departamento$categoria[1:TOP_DEPARTAMENTOS]
departamentos <- ncmv %>%
  mutate(categoria = if_else(DEPARTAMENTO %in% principales,
                             DEPARTAMENTO, "OTROS")) %>%
  count(categoria, name = "fi") %>%
  mutate(orden = if_else(categoria == "OTROS", 0L, fi)) %>%
  arrange(desc(orden)) %>%
  select(-orden) %>%
  agregar_acumuladas()
guardar_tabla(departamentos, "frecuencias_departamento", DIR_G2)

# Interpretación: Lima concentra 63,25 % de los créditos, seguida por Lambayeque
# (7,15 %), Piura (5,76 %) y La Libertad (5,10 %). Los diez primeros departamentos
# reúnen 97,38 %; los otros 15 solo 2,62 %. El programa tiene presencia nacional,
# pero muy concentrada en la costa.

por_mes <- agregar_acumuladas(count(ncmv, MES, name = "fi"))
guardar_tabla(por_mes, "frecuencias_mes", DIR_G2)

# Interpretación: la colocación mensual suele estar entre 572 y 843 créditos, con
# dos excepciones consecutivas: agosto (247; 2,88 %) y setiembre (1 169; 13,65 %).
# El archivo no permite conocer la causa de esa variación, por lo que no se le
# atribuye una explicación.


# ----------------------------------------------------------------------------
# 3. Tablas de frecuencia de las variables cuantitativas
# ----------------------------------------------------------------------------
for (variable in VARIABLES_NUMERICAS) {
  guardar_tabla(tabla_frecuencias_agrupada(ncmv[[variable]]),
                paste0("frecuencias_", tolower(variable)), DIR_G2)
}

# Interpretación: el número de clases sigue la regla de Sturges. TASA: 50,45 % de
# los créditos está entre 9 % y 10 %, y 89,76 % por debajo de 11 %. MONTO_CREDITO:
# se reparte en un rango amplio, con la mayoría entre S/ 100 000 y S/ 300 000.
# MONTO_CUOTA_INICIAL: 66,23 % es menor a S/ 40 000. PLAZOS: se agrupa en valores
# redondos, con 38,31 % en el intervalo de 240 a 260 meses. MONTO_VALOR_VIVIENDA:
# 10,65 % está en el último intervalo, junto al máximo de S/ 355 100.


# ----------------------------------------------------------------------------
# 4. Gráficos de las variables cualitativas
# ----------------------------------------------------------------------------
# Sectores por tipo de IFI.
tipo_ifi <- tabla_frecuencias(ncmv, "TIPO_IFI", ordenar = FALSE) %>%
  mutate(categoria = factor(categoria, levels = NIVELES_TIPO_IFI),
         etiqueta = paste0(categoria, " (",
                           format(hi_pct, decimal.mark = ",", nsmall = 2,
                                  trim = TRUE), " %)"))

grafico_sectores <- ggplot(tipo_ifi, aes(x = 1, y = fi, fill = categoria)) +
  geom_col(width = 1, color = "white", linewidth = 0.6) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = COLORES_TIPO_IFI, name = "Tipo de IFI",
                    labels = setNames(tipo_ifi$etiqueta,
                                      as.character(tipo_ifi$categoria))) +
  labs(title = "Créditos del NCMV según el tipo de IFI, 2024") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.margin = margin(10, 20, 10, 20))
guardar_grafico(grafico_sectores, "f03_sectores_tipo_ifi", ancho = 7, alto = 4.5,
                directorio = DIR_G2)

# Interpretación: los bancos dominan la colocación con 85,88 % de los créditos. Las
# CMAC (8,06 %), las financieras (4,23 %) y las Edpyme (1,83 %) suman 14,12 %. Los
# grupos son muy desiguales, algo que debe considerarse al compararlos.

# Barras por IFI.
ifi <- ncmv %>%
  count(IFI, TIPO_IFI, name = "fi") %>%
  arrange(fi)

grafico_ifi <- ggplot(ifi, aes(x = fi, y = reorder(IFI, fi), fill = TIPO_IFI)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = format(fi, big.mark = " ", trim = TRUE)),
            hjust = -0.15, size = 3.2) +
  scale_x_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = COLORES_TIPO_IFI, name = "Tipo de IFI") +
  labs(title = "Créditos del NCMV por entidad, 2024",
       x = "Número de créditos", y = NULL) +
  tema_informe() +
  theme(panel.grid.major.y = element_blank())
guardar_grafico(grafico_ifi, "f04_barras_ifi", ancho = 8, alto = 5.5,
                directorio = DIR_G2)

# Interpretación: las seis entidades con más créditos son bancos. BCP duplica a
# BBVA e Interbank, casi iguales entre sí. CMAC Huancayo es la primera entidad no
# bancaria, con 365 créditos. Banco de Comercio es el único banco con volumen
# menor al de las CMAC principales.

# Barras por departamento.
grafico_departamento <- departamentos %>%
  mutate(categoria = factor(categoria, levels = rev(categoria))) %>%
  ggplot(aes(x = fi, y = categoria)) +
  geom_col(fill = COLOR_PRINCIPAL, width = 0.72) +
  geom_text(aes(label = format(fi, big.mark = " ", trim = TRUE)),
            hjust = -0.15, size = 3.2) +
  scale_x_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Créditos del NCMV por departamento, 2024",
       x = "Número de créditos", y = NULL) +
  tema_informe() +
  theme(panel.grid.major.y = element_blank())
guardar_grafico(grafico_departamento, "f05_barras_departamento", ancho = 8,
                alto = 4.8, directorio = DIR_G2)

# Interpretación: el gráfico confirma la gran distancia entre Lima (5 417) y
# Lambayeque (612), el segundo departamento. Esto justifica reunir el análisis
# geográfico en dos zonas en lugar de comparar 25 departamentos muy desiguales.

# Barras por mes.
MESES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct",
           "Nov", "Dic")

grafico_mes <- por_mes %>%
  mutate(mes = factor(MESES[MES], levels = MESES)) %>%
  ggplot(aes(x = mes, y = fi)) +
  geom_col(fill = COLOR_PRINCIPAL, width = 0.72) +
  geom_text(aes(label = format(fi, big.mark = " ", trim = TRUE)),
            vjust = -0.5, size = 3.2) +
  scale_y_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Créditos del NCMV desembolsados por mes, 2024",
       x = NULL, y = "Número de créditos") +
  tema_informe() +
  theme(panel.grid.major.x = element_blank())
guardar_grafico(grafico_mes, "f06_barras_mes", alto = 4.2, directorio = DIR_G2)

# Interpretación: la serie es estable salvo la caída de agosto y el pico de
# setiembre. El segundo semestre reúne 52,49 % de los créditos, de modo que la
# colocación se reparte de forma casi pareja entre ambos semestres.


# ----------------------------------------------------------------------------
# 5. Gráficos de las variables cuantitativas
# ----------------------------------------------------------------------------
# Histograma de cada variable con su media y su mediana.
for (i in seq_along(VARIABLES_NUMERICAS)) {
  variable <- VARIABLES_NUMERICAS[i]
  cortes   <- pretty(ncmv[[variable]], n = nclass.Sturges(ncmv[[variable]]))
  media    <- mean(ncmv[[variable]])
  mediana  <- median(ncmv[[variable]])

  histograma <- ggplot(ncmv, aes(x = .data[[variable]])) +
    geom_histogram(breaks = cortes, closed = "left", fill = COLOR_PRINCIPAL,
                   color = "white", linewidth = 0.3) +
    geom_vline(aes(xintercept = media, linetype = "Media"),
               color = COLOR_REFERENCIA, linewidth = 0.7) +
    geom_vline(aes(xintercept = mediana, linetype = "Mediana"),
               color = COLOR_REFERENCIA, linewidth = 0.7) +
    scale_linetype_manual(values = c(Media = "solid", Mediana = "dashed"),
                          name = NULL) +
    scale_x_continuous(labels = label_number(big.mark = " ")) +
    scale_y_continuous(labels = label_number(big.mark = " "),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = paste("Distribución de", ETIQUETAS_VARIABLES[[variable]]),
         x = ETIQUETAS_VARIABLES[[variable]], y = "Número de créditos") +
    tema_informe() +
    theme(legend.position = "top", legend.justification = "left")
  nombre <- sprintf("f%02d_histograma_%s", 6 + i, tolower(variable))
  guardar_grafico(histograma, nombre, alto = 4.2, directorio = DIR_G2)
}

# Interpretación: TASA tiene un pico marcado entre 9 % y 10 % y una cola derecha
# larga, por lo que la media (9,80 %) supera a la mediana (9,48 %). Sus barras
# sobre 15 % casi no se ven por tener muy pocos casos. MONTO_CREDITO y
# MONTO_VALOR_VIVIENDA son planos y con media y mediana muy cercanas.
# MONTO_CUOTA_INICIAL tiene cola derecha, como la tasa. PLAZOS muestra picos en
# valores redondos, propios de una variable discreta, y su media (217,3 meses) es
# menor que su mediana (240).

# Diagramas de caja de la tasa por tipo de IFI y por zona.
grafico_caja_ifi <- ggplot(ncmv, aes(x = TIPO_IFI, y = TASA, fill = TIPO_IFI)) +
  geom_boxplot(width = 0.6, outlier.size = 0.8, outlier.alpha = 0.5,
               show.legend = FALSE) +
  scale_fill_manual(values = COLORES_TIPO_IFI) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  labs(title = "Tasa de interés del NCMV según el tipo de IFI, 2024",
       x = NULL, y = "Tasa (%)") +
  tema_informe() +
  theme(panel.grid.major.x = element_blank())
guardar_grafico(grafico_caja_ifi, "f12_cajas_tasa_tipo_ifi", alto = 4.5,
                directorio = DIR_G2)

# Interpretación: la caja de las CMAC está por encima de todas las demás y es la
# más alta, lo que indica mayor nivel y mayor variación. La de los bancos es la más
# baja y estrecha. Las cajas de los grupos tienen alturas muy distintas, señal de
# que las varianzas podrían no ser homogéneas; se verificará en g-iii.

grafico_caja_zona <- ggplot(ncmv, aes(x = ZONA, y = TASA, fill = ZONA)) +
  geom_boxplot(width = 0.5, outlier.size = 0.8, outlier.alpha = 0.5,
               show.legend = FALSE) +
  scale_fill_manual(values = COLORES_ZONA) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  labs(title = "Tasa de interés del NCMV según la zona geográfica, 2024",
       x = NULL, y = "Tasa (%)") +
  tema_informe() +
  theme(panel.grid.major.x = element_blank())
guardar_grafico(grafico_caja_zona, "f13_cajas_tasa_zona", ancho = 6.5, alto = 4.5,
                directorio = DIR_G2)

# Interpretación: el resto del país presenta una mediana de tasa mayor y una caja
# más amplia que Lima y Callao, además de las tasas extremas. Es un indicio visual
# de diferencia entre zonas, que se contrastará formalmente en g-iii.
