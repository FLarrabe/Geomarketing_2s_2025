#=============================================================================
# 1) CARGAR LIBRERÍAS
#=============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)
library(dplyr)

#=============================================================================
# 2) CONEXIÓN A BASE DE DATOS
#=============================================================================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "1234"
)

#=============================================================================
# 3) CONSULTA SQL: MATERNIDAD ADOLESCENTE Y ASISTENCIA EDUCACIONAL
#=============================================================================
sql_indicadores <- "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,
  COALESCE(ROUND(
    COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19 AND p.p19 >= 1
    )::numeric /
    NULLIF(COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19
    ), 0) * 100, 2), 0) AS maternidad_adolescente,
  COALESCE(ROUND(
    COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19 AND p.p13 = 1
    )::numeric /
    NULLIF(COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19
    ), 0) * 100, 2), 0) AS asistencia_educacional,
  COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19) AS total_mujeres_adolescentes
FROM public.personas AS p
JOIN public.hogares AS h ON p.hogar_ref_id = h.hogar_ref_id
JOIN public.viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN public.comunas AS c ON z.codigo_comuna = c.codigo_comuna
JOIN dpa.zonas_censales_rm AS zc ON z.geocodigo::text = zc.geocodigo::text
WHERE zc.urbano = 1 AND (
  zc.nom_provin = 'SANTIAGO' OR
  zc.nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
)
GROUP BY z.geocodigo, c.nom_comuna
ORDER BY maternidad_adolescente DESC;
"

df_indicadores <- dbGetQuery(con, sql_indicadores)

#=============================================================================
# 4) CARGAR GEOMETRÍA DE ZONAS CENSALES CON LOS MISMOS FILTROS
#=============================================================================
sql_geometria <- "
SELECT geocodigo::double precision AS geocodigo, geom
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'));
"

sf_zonas <- st_read(con, query = sql_geometria)

#=============================================================================
# 5) COMBINAR DATOS TABULARES Y ESPACIALES
#=============================================================================
sf_mapa <- left_join(sf_zonas, df_indicadores, by = "geocodigo")

#=============================================================================
# 6) EDA
#=============================================================================
# Histograma maternidad adolescente
ggplot(df_indicadores, aes(x = maternidad_adolescente)) +
  geom_histogram(bins = 30, fill = '#226e6e', color = 'white') +
  geom_vline(aes(xintercept = median(maternidad_adolescente, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de % Maternidad Adolescente",
       x = "% Maternidad Adolescente",
       y = "Frecuencia") +
  theme_minimal()

# Histograma asistencia educacional
ggplot(df_indicadores, aes(x = asistencia_educacional)) +
  geom_histogram(bins = 30, fill = '#6e6b22', color = 'white') +
  geom_vline(aes(xintercept = median(asistencia_educacional, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de % Asistencia Educacional",
       x = "% Asistencia Educacional",
       y = "Frecuencia") +
  theme_minimal()


#=============================================================================
# 6.1) CUADRANTES DE DISPERSIÓN (Scatterplott)
#=============================================================================
# Calcular Terciles
q_maternidad <- quantile(sf_mapa$maternidad_adolescente, probs = c(0.25, 0.75), na.rm = TRUE)
q_educacion <- quantile(sf_mapa$asistencia_educacional, probs = c(0.25, 0.75), na.rm = TRUE)

# Clasificar en niveles (bajo, medio, alto)
sf_mapa$nivel_maternidad <- with(sf_mapa, ifelse(
  maternidad_adolescente <= q_maternidad[1], "Baja",
  ifelse(maternidad_adolescente >= q_maternidad[2], "Alta", "Media")
))

sf_mapa$nivel_educacion <- with(sf_mapa, ifelse(
  asistencia_educacional <= q_educacion[1], "Baja",
  ifelse(asistencia_educacional >= q_educacion[2], "Alta", "Media")
))

# Crear 9 Clasificaciones 
sf_mapa$cuadrante <- paste("Maternidad", sf_mapa$nivel_maternidad, "/ Educación", sf_mapa$nivel_educacion)

# Paleta de 9 tonos de azul
colores_cuadrantes <- c(
  "Maternidad Alta / Educación Alta" = "#08306B",
  "Maternidad Alta / Educación Media" = "#08519C",
  "Maternidad Alta / Educación Baja" = "#2171B5",
  "Maternidad Media / Educación Alta" = "#4292C6",
  "Maternidad Media / Educación Media" = "#6BAED6",
  "Maternidad Media / Educación Baja" = "#9ECAE1",
  "Maternidad Baja / Educación Alta" = "#C6DBEF",
  "Maternidad Baja / Educación Media" = "#DEEBF7",
  "Maternidad Baja / Educación Baja" = "#F7FBFF"
)

# gráfico Scatterplot
grafico_cuadrantes <- ggplot(sf_mapa, aes(x = maternidad_adolescente, y = asistencia_educacional, color = cuadrante)) +
  geom_point(size = 0.6, alpha = 0.8) +
  geom_vline(xintercept = q_maternidad[1], linetype = "dotted", color = "darkred") +
  geom_vline(xintercept = q_maternidad[2], linetype = "dotted", color = "darkred") +
  geom_hline(yintercept = q_educacion[1], linetype = "dotted", color = "darkred") +
  geom_hline(yintercept = q_educacion[2], linetype = "dotted", color = "darkred") +
  geom_smooth(method = "glm", se = FALSE, color = "darkred", linetype = "longdash", size= 0.7) + #linea de tendencia, metodo de ajuste de regresion lineal
  scale_color_manual(values = colores_cuadrantes) +
  labs(
    x = "Tasa de Maternidad Adolescente (%)",
    y = "Tasa de Asistencia Educacional (%)",
    title = "Dispersión: Maternidad Adolescente vs Asistencia Educacional",
    color = "Clasificación"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )

# Mostrar gráfico
print(grafico_cuadrantes)

#=============================================================================
# 7) MAPAS TEMÁTICOS INDIVIDUALES
#=============================================================================
cuartiles_maternidad <- quantile(sf_mapa$maternidad_adolescente, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
cuartiles_educacion <- quantile(sf_mapa$asistencia_educacional, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

sf_mapa$maternidad_cuartil <- cut(sf_mapa$maternidad_adolescente,
                                  breaks = cuartiles_maternidad,
                                  labels = c("Q1 (Bajo)", "Q2", "Q3", "Q4 (Alto)"),
                                  include.lowest = TRUE)

sf_mapa$educacion_cuartil <- cut(sf_mapa$asistencia_educacional,
                                 breaks = cuartiles_educacion,
                                 labels = c("Q1 (Bajo)", "Q2", "Q3", "Q4 (Alto)"),
                                 include.lowest = TRUE)

map_maternidad_cuartiles <- ggplot(sf_mapa) +
  geom_sf(aes(fill = maternidad_cuartil), color = "gray80", size = 0.1) +
  scale_fill_manual(
    name = "Maternidad Adolescente\n(Cuartiles)",
    values = c("Q1 (Bajo)" = "#eff3ff", "Q2" = "#bdd7e7", "Q3" = "#6baed6", "Q4 (Alto)" = "#2171b5")
  ) +
  labs(
    title = "Tasa de Maternidad Adolescente (15-19 años)",
    subtitle = "Clasificación por cuartiles"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

map_educacion_cuartiles <- ggplot(sf_mapa) +
  geom_sf(aes(fill = educacion_cuartil), color = "gray80", size = 0.1) +
  scale_fill_manual(
    name = "Asistencia Educacional\n(Cuartiles)",
    values = c("Q1 (Bajo)" = "#ffeda0", "Q2" = "#feb24c", "Q3" = "#f03b20", "Q4 (Alto)" = "#bd0026")
  ) +
  labs(
    title = "Asistencia Educacional en Adolescentes",
    subtitle = "Clasificación por cuartiles"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

print(map_maternidad_cuartiles)
print(map_educacion_cuartiles)

#=============================================================================
# 8) MAPA BIVARIADO, CLASIFICADO POR TERCILES
#=============================================================================
# Calcular cortes personalizados (terciles) para cada variable
breaks_maternidad <- quantile(sf_mapa$maternidad_adolescente, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
breaks_educacion  <- quantile(sf_mapa$asistencia_educacional, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)

# Crear variables factor con 3 niveles cada una (1 = bajo, 3 = alto)
sf_mapa <- sf_mapa %>%
  mutate(
    maternidad_bin = cut(maternidad_adolescente,
                         breaks = breaks_maternidad,
                         labels = c("1", "2", "3"),
                         include.lowest = TRUE),
    educacion_bin = cut(asistencia_educacional,
                        breaks = breaks_educacion,
                        labels = c("1", "2", "3"),
                        include.lowest = TRUE)
  )

# Crear clasificación bivariada con los factores ya definidos
sf_mapa_bi <- bi_class(
  sf_mapa,
  x = maternidad_bin,
  y = educacion_bin,
  style = "quantile",  # no importa realmente aquí
  dim = 3
)

# Leer geometría de comunas para contexto
sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO');
"
sf_comunas <- st_read(con, query = sql_comunas)

# Definir bounding box del mapa
caja <- st_bbox(sf_mapa_bi)

# Crear mapa bivariado
mapa_bivariado_etiquetas <- ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_comunas, fill = NA, color = 'black', size = 0.3) +
  bi_scale_fill(pal = 'DkViolet', dim = 3) +
  labs(
    title = 'Mapa Bivariado: Maternidad Adolescente vs Asistencia Educacional',
    subtitle = 'Zonas urbanas de Santiago, Puente Alto y San Bernardo - CENSO 2017'
  ) +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']), ylim = c(caja['ymin'], caja['ymax']), expand = FALSE) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Crear leyenda
leyenda_bivariada <- bi_legend(
  pal = 'DkViolet', dim = 3,
  xlab = 'Maternidad Alta', ylab = 'Asistencia Alta', size = 8
)

# Componer mapa final con leyenda
mapa_final <- ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(leyenda_bivariada, x = 0.7, y = 0.05, width = 0.25, height = 0.25)

# Mostrar mapa
print(mapa_final)