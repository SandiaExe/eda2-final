# ============================================================================
# g1_3_herramientas.R
# g-i. Preparación y exploración de datos
# Parte 3. Herramientas utilizadas: R e Inteligencia Artificial
# ============================================================================

source("codigo/00_configuracion.R")


# ----------------------------------------------------------------------------
# 1. Lenguaje y paquetes de R
# ----------------------------------------------------------------------------
USO_PAQUETE <- c(
  readr   = "Lectura del CSV y exportación de tablas",
  dplyr   = "Filtrado, transformación y resumen de datos",
  tidyr   = "Reorganización de datos para gráficos",
  ggplot2 = "Construcción de todos los gráficos",
  scales  = "Formato de los ejes numéricos",
  e1071   = "Cálculo de la asimetría y la curtosis"
)

herramientas_r <- vector("list", length(PAQUETES) + 1)
herramientas_r[[1]] <- tibble(
  herramienta = "R",
  version = paste(R.version$major, R.version$minor, sep = "."),
  uso = "Lenguaje para la preparación, el análisis y los gráficos"
)
for (i in seq_along(PAQUETES)) {
  paquete <- PAQUETES[i]
  herramientas_r[[i + 1]] <- tibble(
    herramienta = paquete,
    version = as.character(packageVersion(paquete)),
    uso = USO_PAQUETE[[paquete]]
  )
}


# ----------------------------------------------------------------------------
# 2. Inteligencia artificial
# ----------------------------------------------------------------------------
herramienta_ia <- tibble(
  herramienta = "Claude Code (Anthropic)",
  version = "Claude Sonnet 5.5 y Claude Opus 5.5",
  uso = "Generación asistida del código en R a partir de los prompts de la carpeta prompts/"
)

tabla_herramientas <- bind_rows(bind_rows(herramientas_r), herramienta_ia)
guardar_tabla(tabla_herramientas, "t12_herramientas")

# Interpretación: el análisis se realiza en R 4.6.1 con seis paquetes. La IA apoya
# la generación del código a partir de un prompt por cada etapa; los resultados se
# verifican al ejecutar los scripts sobre los datos y la interpretación es
# responsabilidad del equipo.


# ----------------------------------------------------------------------------
# 3. Reproducibilidad
# ----------------------------------------------------------------------------
entorno <- tibble(
  elemento = c("Sistema operativo", "Plataforma", "Fecha de ejecución"),
  valor = c(utils::osVersion, R.version$platform, as.character(Sys.Date()))
)
guardar_tabla(entorno, "t13_entorno_ejecucion")

# Citas de R y de los paquetes para las referencias del informe.
archivo_citas <- file.path(DIR_G1, "citas_software.txt")
citas <- format(citation(), style = "text")
for (paquete in PAQUETES) {
  citas <- c(citas, format(citation(paquete), style = "text"))
}
writeLines(citas, archivo_citas)
cat("\nCitas guardadas en", archivo_citas, "\n")

# Interpretación: registrar el sistema y las versiones permite reproducir el
# análisis. Las citas generadas por R sirven de base para las referencias del
# informe, y deben adaptarse al formato APA 7.ª edición.
