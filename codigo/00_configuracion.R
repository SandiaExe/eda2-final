# ============================================================================
# 00_configuracion.R
# Configuración común del proyecto: paquetes, constantes y funciones auxiliares.
# Curso: Análisis Exploratorio de Datos II - Avance 2
#
# Todos los scripts cargan este archivo. Deben ejecutarse con la carpeta raíz
# del proyecto como directorio de trabajo, por ejemplo:
#   Rscript codigo/g1_r1_descripcion_dataset.R
# ============================================================================


# ----------------------------------------------------------------------------
# 1. Paquetes
# ----------------------------------------------------------------------------
PAQUETES <- c("readr", "dplyr", "tidyr", "ggplot2", "scales")

# Instala los paquetes que falten y luego los carga.
instalar_y_cargar <- function(paquetes) {
  for (paquete in paquetes) {
    if (!requireNamespace(paquete, quietly = TRUE)) {
      install.packages(paquete, repos = "https://cloud.r-project.org")
    }
    suppressPackageStartupMessages(library(paquete, character.only = TRUE))
  }
}

instalar_y_cargar(PAQUETES)


# ----------------------------------------------------------------------------
# 2. Constantes
# ----------------------------------------------------------------------------
# Rutas
ARCHIVO_CSV    <- "Colocaciones_de_crédito_Mivivienda_2024.csv"
DIR_SALIDAS    <- "salidas"
DIR_G1         <- file.path(DIR_SALIDAS, "g1")
DIR_DATOS      <- file.path(DIR_SALIDAS, "datos")
ARCHIVO_LIMPIO <- file.path(DIR_DATOS, "ncmv_2024_limpio.csv")

# Parámetros estadísticos
ALFA            <- 0.05
NIVEL_CONFIANZA <- 1 - ALFA
COEF_IQR        <- 1.5    # Regla de Tukey para valores atípicos

# Delimitación del estudio
PRODUCTO_ESTUDIO    <- "NMIV"
DEPARTAMENTOS_LIMA  <- c("LIMA", "CALLAO")
ZONA_LIMA           <- "Lima y Callao"
ZONA_RESTO          <- "Resto del país"
NIVELES_TIPO_IFI    <- c("Banco", "CMAC", "Financiera", "Edpyme")

# Condiciones del producto (Fondo MIVIVIENDA S.A., 2025b)
PLAZO_MIN_MESES       <- 60
PLAZO_MAX_MESES       <- 300
PCT_CUOTA_INICIAL_MIN <- 7.5
PCT_FINANCIAMIENTO_MAX <- 90

# Variables numéricas del análisis
VARIABLES_NUMERICAS <- c("TASA", "MONTO_CREDITO", "MONTO_CUOTA_INICIAL",
                         "PLAZOS", "MONTO_VALOR_VIVIENDA")
ETIQUETAS_VARIABLES <- c(TASA = "Tasa (%)", MONTO_CREDITO = "Crédito (S/)",
                         MONTO_CUOTA_INICIAL = "Cuota inicial (S/)",
                         PLAZOS = "Plazo (meses)",
                         MONTO_VALOR_VIVIENDA = "Vivienda (S/)")

# Colores de los gráficos
COLOR_PRINCIPAL <- "#1F6F8B"
COLOR_ATIPICO   <- "#C8553D"


# ----------------------------------------------------------------------------
# 3. Funciones auxiliares
# ----------------------------------------------------------------------------
# Lee el CSV original conservando UBIGEO como texto (código de 6 dígitos).
leer_datos_crudos <- function(ruta = ARCHIVO_CSV) {
  read_delim(
    ruta,
    delim = ";",
    col_types = cols(
      PRODUCTO = col_character(), DEPARTAMENTO = col_character(),
      PROVINCIA = col_character(), DISTRITO = col_character(),
      UBIGEO = col_character(), IFI = col_character(),
      TIPO_IFI = col_character(), .default = col_double()
    ),
    show_col_types = FALSE
  )
}

# Identifica las filas en las que todas las columnas están vacías.
es_fila_vacia <- function(datos) {
  rowSums(!is.na(datos)) == 0
}

# Devuelve los límites de la regla de Tukey (Q1 - 1,5 IQR y Q3 + 1,5 IQR).
limites_tukey <- function(x, coef = COEF_IQR) {
  cuartiles <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  rango_iq  <- cuartiles[[2]] - cuartiles[[1]]
  c(inferior = cuartiles[[1]] - coef * rango_iq,
    superior = cuartiles[[2]] + coef * rango_iq)
}

# Guarda una tabla en CSV y la muestra en consola.
guardar_tabla <- function(tabla, nombre, directorio = DIR_G1) {
  dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
  write_csv(tabla, file.path(directorio, paste0(nombre, ".csv")))
  cat("\n---", nombre, "---\n")
  print(as.data.frame(tabla), row.names = FALSE)
}

# Guarda un gráfico de ggplot2 en PNG.
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5,
                            directorio = DIR_G1) {
  dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(directorio, paste0(nombre, ".png")), grafico,
         width = ancho, height = alto, dpi = 300, bg = "white")
}

# Tema común para todos los gráficos.
tema_informe <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}
