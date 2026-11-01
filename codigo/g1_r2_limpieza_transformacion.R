# ============================================================================
# g1_r2_limpieza_transformacion.R
# g-i. Preparación y exploración de datos
# r2. Limpieza, transformación y codificación
# ============================================================================

source("codigo/00_configuracion.R")


# ----------------------------------------------------------------------------
# 1. Limpieza
# ----------------------------------------------------------------------------
datos_crudos <- leer_datos_crudos()

sin_vacias     <- datos_crudos[!es_fila_vacia(datos_crudos), ]
sin_duplicados <- distinct(sin_vacias)
ncmv           <- filter(sin_duplicados, PRODUCTO == PRODUCTO_ESTUDIO)

flujo_limpieza <- tibble(
  paso = c("Archivo original", "Sin filas vacías", "Sin duplicados exactos",
           "Solo Nuevo Crédito Mivivienda (NMIV)"),
  filas = c(nrow(datos_crudos), nrow(sin_vacias), nrow(sin_duplicados),
            nrow(ncmv))
) %>%
  mutate(filas_eliminadas = lag(filas, default = filas[1]) - filas)
guardar_tabla(flujo_limpieza, "t04_flujo_limpieza")

# Duplicados eliminados por producto.
duplicados_producto <- sin_vacias %>%
  count(PRODUCTO, name = "con_duplicados") %>%
  left_join(count(sin_duplicados, PRODUCTO, name = "sin_duplicados"),
            by = "PRODUCTO") %>%
  mutate(eliminados = con_duplicados - sin_duplicados)
guardar_tabla(duplicados_producto, "t05_duplicados_por_producto")

# Interpretación: se eliminan 4 160 filas vacías y 11 duplicados exactos (8 de
# NMIV y 3 de FCTP). Luego se descartan los 771 créditos de otros productos. La
# base de análisis queda en 8 565 créditos, el 91,7 % de los 9 336 válidos.


# ----------------------------------------------------------------------------
# 2. Validación de consistencia
# ----------------------------------------------------------------------------
# Los porcentajes se redondean para no contar como incumplimiento un valor que
# difiere del límite solo por la precisión decimal del cálculo (7,4999...).
pct_cuota <- round(100 * ncmv$MONTO_CUOTA_INICIAL / ncmv$MONTO_VALOR_VIVIENDA, 2)
pct_financiamiento <- round(100 * ncmv$MONTO_CREDITO / ncmv$MONTO_VALOR_VIVIENDA, 2)
suma_supera_valor <- ncmv$MONTO_CREDITO + ncmv$MONTO_CUOTA_INICIAL >
  ncmv$MONTO_VALOR_VIVIENDA

validacion <- tibble(
  regla = c("Valores faltantes en alguna columna",
            "Montos o tasa menores o iguales a cero",
            "Plazo fuera de 60 a 300 meses",
            "Cuota inicial menor a 7,5 % de la vivienda",
            "Financiamiento mayor a 90 % de la vivienda",
            "Crédito + cuota inicial mayor al valor de la vivienda",
            "Fecha de desembolso fuera de 2024"),
  casos = c(sum(!complete.cases(ncmv)),
            sum(ncmv$MONTO_CREDITO <= 0 | ncmv$MONTO_CUOTA_INICIAL <= 0 |
                  ncmv$MONTO_VALOR_VIVIENDA <= 0 | ncmv$TASA <= 0),
            sum(ncmv$PLAZOS < PLAZO_MIN_MESES | ncmv$PLAZOS > PLAZO_MAX_MESES),
            sum(pct_cuota < PCT_CUOTA_INICIAL_MIN),
            sum(pct_financiamiento > PCT_FINANCIAMIENTO_MAX),
            sum(suma_supera_valor),
            sum(ncmv$FECHA_DESEMBOLSO %/% 10000 != 2024))
)
guardar_tabla(validacion, "t06_validacion_consistencia")

# Interpretación: no hay faltantes, montos o tasas no positivos, plazos fuera de
# rango ni fechas fuera de 2024. Solo 6 créditos superan el 90 % de financiamiento
# (máximo 90,75 %) y en 3 la suma de crédito y cuota inicial excede el valor de la
# vivienda en hasta unos S/ 2 450. Son como máximo 9 registros (0,1 %) y los
# límites usados son los de 2025 (no los de 2024), por lo que no se eliminan.


# ----------------------------------------------------------------------------
# 3. Transformación y codificación
# ----------------------------------------------------------------------------
# Nombres comerciales de las IFI que el archivo registra con otra denominación.
NOMBRES_IFI <- c(Credito = "BCP", Continental = "BBVA",
                 Comercio = "Banco de Comercio",
                 "Financiera Efectiva S. A" = "Financiera Efectiva",
                 "EMPRESA DE CREDITO VIVELA S.A." = "Vivela")

ncmv_limpio <- ncmv %>%
  mutate(
    FECHA = as.Date(as.character(FECHA_DESEMBOLSO), format = "%Y%m%d"),
    MES   = as.integer(format(FECHA, "%m")),
    PROVINCIA = if_else(PROVINCIA == "CANIETE", "CAÑETE", PROVINCIA),
    IFI = if_else(IFI %in% names(NOMBRES_IFI), NOMBRES_IFI[IFI], IFI),
    TIPO_IFI = factor(TIPO_IFI, levels = NIVELES_TIPO_IFI),
    ZONA = factor(if_else(DEPARTAMENTO %in% DEPARTAMENTOS_LIMA,
                          ZONA_LIMA, ZONA_RESTO),
                  levels = c(ZONA_LIMA, ZONA_RESTO)),
    ZONA_LIMA_CALLAO = as.integer(ZONA == ZONA_LIMA),
    PLAZO_ANIOS = PLAZOS / 12,
    PCT_CUOTA_INICIAL = 100 * MONTO_CUOTA_INICIAL / MONTO_VALOR_VIVIENDA
  ) %>%
  select(FECHA, MES, DEPARTAMENTO, PROVINCIA, DISTRITO, UBIGEO, ZONA,
         ZONA_LIMA_CALLAO, IFI, TIPO_IFI, all_of(VARIABLES_NUMERICAS),
         PLAZO_ANIOS, PCT_CUOTA_INICIAL)

distribucion_zona <- ncmv_limpio %>%
  count(ZONA, name = "creditos") %>%
  mutate(porcentaje = round(100 * creditos / sum(creditos), 1))
guardar_tabla(distribucion_zona, "t07_distribucion_zona")

# Interpretación: FECHA convierte el texto aaaammdd a tipo fecha y MES permite
# ver la evolución del año. TIPO_IFI y ZONA pasan a factores: Banco y Lima y
# Callao son las categorías de referencia para los modelos. ZONA_LIMA_CALLAO es
# su versión binaria (1 = Lima y Callao). Se corrige CANIETE y se usan los nombres
# comerciales de las IFI. Se excluyen PRODUCTO y FECHA_CORTE por ser constantes.
# La muestra es desbalanceada: 67,8 % en Lima y Callao y 32,2 % en el resto.


# ----------------------------------------------------------------------------
# 4. Sensibilidad de la definición de ZONA
# ----------------------------------------------------------------------------
# Compara Lima y Callao por departamento con Lima Metropolitana (provincias de
# Lima y Callao) para evaluar si la definición cambia los resultados.
es_metropolitana <- ncmv_limpio$PROVINCIA %in% c("LIMA", "CALLAO")

sensibilidad_zona <- tibble(
  definicion = c("Por departamento (usada)", "Lima Metropolitana"),
  creditos_lima = c(sum(ncmv_limpio$ZONA == ZONA_LIMA),
                    sum(es_metropolitana)),
  tasa_media_lima = c(mean(ncmv_limpio$TASA[ncmv_limpio$ZONA == ZONA_LIMA]),
                      mean(ncmv_limpio$TASA[es_metropolitana])),
  tasa_media_resto = c(mean(ncmv_limpio$TASA[ncmv_limpio$ZONA == ZONA_RESTO]),
                       mean(ncmv_limpio$TASA[!es_metropolitana]))
) %>%
  mutate(across(starts_with("tasa"), ~ round(.x, 3)))
guardar_tabla(sensibilidad_zona, "t08_sensibilidad_zona")

# Interpretación: la definición por departamento incluye 82 créditos de Cañete,
# Huaura, Barranca y Huaral (1,4 % de la zona). La diferencia de tasa media entre
# zonas casi no cambia: 1,290 puntos por departamento y 1,268 con Lima
# Metropolitana. Se mantiene la definición por departamento, igual que en el
# Avance 1, porque los resultados no dependen de esta decisión.


# ----------------------------------------------------------------------------
# 5. Valores atípicos (regla de Tukey)
# ----------------------------------------------------------------------------
resumen_atipicos <- vector("list", length(VARIABLES_NUMERICAS))
for (i in seq_along(VARIABLES_NUMERICAS)) {
  variable <- VARIABLES_NUMERICAS[i]
  valores  <- ncmv_limpio[[variable]]
  limites  <- limites_tukey(valores)
  resumen_atipicos[[i]] <- tibble(
    variable = variable,
    limite_inferior = limites[["inferior"]],
    limite_superior = limites[["superior"]],
    atipicos_bajos  = sum(valores < limites[["inferior"]]),
    atipicos_altos  = sum(valores > limites[["superior"]]),
    porcentaje = round(100 * mean(valores < limites[["inferior"]] |
                                    valores > limites[["superior"]]), 1)
  )
}
tabla_atipicos <- bind_rows(resumen_atipicos)
guardar_tabla(tabla_atipicos, "t09_atipicos_por_variable")

# Interpretación: TASA tiene 613 atípicos (7,2 %): 612 superan 11,575 % y uno está
# por debajo de 7,455 %. La cuota inicial tiene 475 atípicos altos (5,5 %) y el
# plazo, 283 bajos (3,3 %), menores a 90 meses. El monto del crédito y el valor de
# la vivienda no presentan atípicos.

# Atípicos de TASA según el tipo de IFI.
limites_tasa <- limites_tukey(ncmv_limpio$TASA)
atipicos_tipo_ifi <- ncmv_limpio %>%
  mutate(atipico = TASA < limites_tasa[["inferior"]] |
           TASA > limites_tasa[["superior"]]) %>%
  group_by(TIPO_IFI) %>%
  summarise(creditos = n(), atipicos = sum(atipico),
            porcentaje_del_tipo = round(100 * mean(atipico), 1),
            .groups = "drop") %>%
  mutate(porcentaje_de_atipicos = round(100 * atipicos / sum(atipicos), 1))
guardar_tabla(atipicos_tipo_ifi, "t10_atipicos_tasa_tipo_ifi")

# Interpretación: los atípicos de TASA no se reparten al azar. Las CMAC reúnen 509
# de los 613 (83,0 %), y el 73,8 % de sus créditos resulta atípico frente al 0,1 %
# de los bancos. Estos valores reflejan una diferencia real entre tipos de IFI, no
# errores de registro, y eliminarlos borraría el fenómeno que se quiere estudiar.

# Créditos con las tasas más altas.
tasas_extremas <- ncmv_limpio %>%
  arrange(desc(TASA)) %>%
  slice_head(n = 5) %>%
  select(IFI, DEPARTAMENTO, TASA, MONTO_CREDITO, PLAZOS)
guardar_tabla(tasas_extremas, "t11_tasas_mas_altas")

# Interpretación: las cuatro tasas más altas (18,76 % a 23,89 %) son de CMAC
# Huancayo, en créditos de hasta S/ 80 000 y plazos de 96 a 120 meses, fuera de
# Lima y Callao. La quinta (16,00 %) es de CMAC Ica. Son poco frecuentes pero
# coherentes con créditos pequeños y cortos de entidades no bancarias.

grafico_atipicos <- ncmv_limpio %>%
  select(all_of(VARIABLES_NUMERICAS)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  mutate(variable = factor(ETIQUETAS_VARIABLES[variable],
                           levels = ETIQUETAS_VARIABLES)) %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(fill = COLOR_PRINCIPAL, alpha = 0.5,
               outlier.color = COLOR_ATIPICO, outlier.size = 0.8) +
  facet_wrap(~ variable, scales = "free_y", nrow = 1) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(title = "Valores atípicos de las variables numéricas del NCMV",
       x = NULL, y = NULL) +
  tema_informe()
guardar_grafico(grafico_atipicos, "f02_boxplots_atipicos", ancho = 10, alto = 4.5)

# Interpretación: la tasa y la cuota inicial tienen una cola larga hacia valores
# altos, mientras que el plazo la tiene hacia valores bajos. Decisión: se conservan
# todos los atípicos porque son créditos reales y válidos dentro del programa. Al
# no ser simétrica la tasa, en la inferencia se verificarán los supuestos antes de
# cada prueba.


# ----------------------------------------------------------------------------
# 6. Exportación de los datos limpios
# ----------------------------------------------------------------------------
dir.create(DIR_DATOS, recursive = TRUE, showWarnings = FALSE)
write_csv(ncmv_limpio, ARCHIVO_LIMPIO)
cat("\nDatos limpios:", nrow(ncmv_limpio), "filas y", ncol(ncmv_limpio),
    "columnas en", ARCHIVO_LIMPIO, "\n")

# Interpretación: la base limpia tiene 8 565 créditos y 17 variables, sin valores
# faltantes. Es el archivo de entrada de las etapas g-ii y g-iii.
