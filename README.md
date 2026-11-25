# Colocaciones del Nuevo Crédito Mivivienda 2024

Proyecto de Análisis Exploratorio de Datos II (USIL). Estudia cómo varía la tasa de interés de los créditos Mivivienda de 2024 según la zona geográfica, las entidades financieras y las características del crédito.

## Requisitos

- R 4.2 o posterior y una sesión configurada para UTF-8.
- Los scripts instalan desde CRAN los paquetes que falten. La primera ejecución requiere internet y permisos de escritura en la biblioteca de R.
- G1–G3 usan `readr`, `dplyr`, `tidyr`, `ggplot2`, `scales` y `e1071`. G4 también usa `broom`, `car`, `lmtest` y `sandwich`. Instala `clubSandwich` para reproducir el análisis de sensibilidad CR2 por entidad financiera incluido en la entrega.

## Reproducir G1, G2, G3 y G4

Mientras se revisa el Pull Request, descarga la rama de entrega por HTTPS.

```bash
git clone --branch entrega-final-g4 https://github.com/joseaguerob/eda2-final.git
cd eda2-final
```

Abre R o RStudio en la raíz del repositorio. Si deseas reproducir también CR2, ejecuta primero `install.packages("clubSandwich", repos = "https://cloud.r-project.org")`. Después ejecuta:

```r
scripts <- sort(list.files("codigo", "^g[1-4]_[1-5]_.*[.]R$", full.names = TRUE))
for (script in scripts) {
  message("Ejecutando ", script)
  source(script, encoding = "UTF-8")
}
```

| Etapa | Trabajo | Salidas |
| --- | --- | --- |
| G1 | Descripción y limpieza de los datos | `salidas/g1/` y `salidas/datos/ncmv_2024_limpio.csv` |
| G2 | Frecuencias, gráficos y medidas descriptivas | `salidas/g2/` |
| G3 | Intervalos de confianza y pruebas de hipótesis | `salidas/g3/` |
| G4 | Regresión simple y múltiple, diagnóstico, sensibilidad y comparación | `salidas/g4/` |

Los archivos auxiliares se cargan desde los scripts que los utilizan. La ejecución vuelve a escribir las salidas generadas, incluida la base limpia. Si quieres conservar las salidas publicadas para compararlas, ejecuta los scripts en una copia aparte.

## Ejecutar solo G4

Con la base limpia generada por G1 disponible en `salidas/datos/`, ejecuta desde la raíz:

```r
scripts <- sort(list.files("codigo", "^g4_[1-4]_.*[.]R$", full.names = TRUE))
for (script in scripts) {
  message("Ejecutando ", script)
  source(script, encoding = "UTF-8")
}
```

Los modelos principales usan todos los créditos de la base limpia. Los intervalos HC3 corrigen por heterocedasticidad. La sensibilidad CR2 examina la posible dependencia de observaciones dentro de cada entidad financiera.

## Material de entrega

- [Informe final](Informe_Final_EDAII_Mivivienda_2024.docx), [avance 1](Avance_01_EDAII.docx) y [avance 2](Avance_02_EDAII.docx).
- [Prompts R1–R4](prompts/) y [resultados G4 de la fase 4.2](salidas/g4/), incluidas las versiones de R y de los paquetes usados.
- El CSV de colocaciones y su diccionario de datos se encuentran en la raíz. La serie de tasas hipotecarias del BCRP está en `datos/`.
