# Colocaciones del Nuevo Crédito Mivivienda 2024

Proyecto del curso Análisis Exploratorio de Datos II (USIL). Analiza si la tasa de interés del Nuevo Crédito Mivivienda de 2024 difiere según el tipo de entidad financiera y la zona geográfica.

## Requisitos

- R 4.2 o superior.
- Conexión a internet la primera vez: los paquetes que falten se instalan solos.

## Cómo reproducirlo

```bash
git clone git@github.com:SandiaExe/eda2-final.git
cd eda2-final
```

Abre R en esa carpeta (debe ser el directorio de trabajo) y ejecuta todos los scripts en orden:

```r
scripts <- sort(list.files("codigo", "^g[123]_[1-5]", full.names = TRUE))
for (s in scripts) source(s, local = new.env(), encoding = "UTF-8")
```

Las tablas y los gráficos se regeneran en `salidas/`.

## Contenido

- `Avance_01_EDAII.docx` y `Avance_02_EDAII.docx`: informes.
- `codigo/`: scripts en R. Se ejecutan en orden: g1 prepara los datos, g2 los describe y g3 hace la inferencia.
- `prompts/`: prompts r1, r2 y r3, uno por cada etapa.
- `salidas/`: tablas (CSV), gráficos (PNG) y la base limpia.
- `Colocaciones_de_crédito_Mivivienda_2024.csv` y su diccionario (`.xlsx`): datos originales del Fondo MIVIVIENDA.
- `datos/`: serie de la tasa hipotecaria del BCRP.
