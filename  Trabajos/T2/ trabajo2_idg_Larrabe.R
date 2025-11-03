## 1. Librerías ####
library(rakeR)
library(RPostgres)
library(DBI)
library(sf)
library(dplyr)
library(VIM)
library(ggplot2)
library(ggspatial)
install.packages("")
## 2. Entradas ####
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"
casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

## 3. PRE-PROCESAMIENTO ####
# 3.1 CENSO
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

# 3.2 CASEN
vars_base = c("estrato","esc","edad","sexo","e6a","o10","o28a_hr","o28a_min","o28b","ypc")
casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Extraer comuna y limpiar estrato
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Quitar etiquetas haven y forzar tipos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$o10 = as.numeric(unclass(casen$o10))
casen$o28a_hr = as.numeric(unclass(casen$o28a_hr))
casen$o28a_min = as.numeric(unclass(casen$o28a_min))
casen$o28b = as.numeric(unclass(casen$o28b))
casen$ypc = as.numeric(unclass(casen$ypc))

# Estadísticas previas a la imputación de datos
resumen_calidad <- data.frame(
  Variable = c("o10", "o28a_hr", "o28a_min", "o28b", "esc", "ypc"),
  NAs = sapply(casen[c("o10", "o28a_hr", "o28a_min", "o28b", "esc", "ypc")], 
               function(x) sum(is.na(x))),
  Media = sapply(casen[c("o10", "o28a_hr", "o28a_min", "o28b", "esc", "ypc")], 
                 function(x) round(mean(x, na.rm = TRUE), 2)),
  Mediana = sapply(casen[c("o10", "o28a_hr", "o28a_min", "o28b", "esc", "ypc")], 
                   function(x) median(x, na.rm = TRUE))
)

print("Resumen de Calidad de Datos - Variables Principales:")
print(resumen_calidad)

### 3.3.1 IMPUTACIÓN simple de esc por e6a ####
idx_na = which(is.na(casen$esc))
if(length(idx_na) > 0) {
  fit = lm(esc ~ e6a, data = casen[-idx_na,])
  pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])
  casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
}
print("Resumen de las estadisticas de escolaridad:")
print(summary(casen$esc))

### 3.3.2 IMPUTACIÓN DE DATOS CON KNN POR COMUNA ####
# Variables objetivo
vars_imputar <- c("o10", "o28a_hr", "o28a_min", "o28b")

# Predictores auxiliares
predictores_aux <- c("esc", "edad", "sexo", "e6a", "ypc")

# 1) Reemplazar valores negativos o fuera de rango por NA
casen <- casen %>%
  mutate(
    o10       = ifelse(!is.na(o10) & o10 < 0, NA, o10),
    o28a_hr   = ifelse(!is.na(o28a_hr) & o28a_hr < 0, NA, o28a_hr),
    o28a_min  = ifelse(!is.na(o28a_min) & o28a_min < 0, NA, o28a_min),
    o28b      = ifelse(!is.na(o28b) & o28b < 0, NA, o28b)
  )

# 2) Definir función general de imputación KNN por comuna
imputar_knn_por_comuna <- function(df, vars_imputar, pred_aux, k = 5, min_complete = 5) {
  stopifnot(all(c(vars_imputar, pred_aux) %in% names(df)))
  
  # Si no hay ningún NA en las variables objetivo → devolver tal cual
  if (all(colSums(is.na(df[vars_imputar])) == 0)) return(df)
  
  # Subconjunto con variables de interés
  knn_df <- df[, unique(c(vars_imputar, pred_aux)), drop = FALSE]
  
  n_valid <- sum(rowSums(!is.na(knn_df[vars_imputar])) > 0)
  n_complete_cases <- sum(complete.cases(knn_df))
  
  # Si no hay suficientes casos completos → usar mediana
  if (n_complete_cases < (k + 1) || n_valid < 2) {
    for (v in vars_imputar) {
      med <- median(df[[v]], na.rm = TRUE)
      if (is.finite(med)) df[[v]][is.na(df[[v]])] <- med
    }
    return(df)
  }
  
  # Imputar mediante KNN con control de errores
  res <- tryCatch({
    imp <- VIM::kNN(knn_df, k = k, imp_var = FALSE, weightDist = TRUE)
    df[, vars_imputar] <- imp[, vars_imputar]
    df
  }, error = function(e) {
    warning(sprintf("KNN falló en comuna %s: %s. Aplicando mediana como fallback.", unique(df$Comuna), e$message))
    for (v in vars_imputar) {
      med <- median(df[[v]], na.rm = TRUE)
      if (is.finite(med)) df[[v]][is.na(df[[v]])] <- med
    }
    df
  })
  
  return(res)
}

# 3) Aplicar imputación KNN por comuna
casen <- casen %>%
  group_by(Comuna) %>%
  group_modify(~ imputar_knn_por_comuna(.x, vars_imputar, predictores_aux, k = 5, min_complete = 5)) %>%
  ungroup()

# 4) Verificar NA restantes y aplicar mediana global como último recurso
na_after <- colSums(is.na(casen[, vars_imputar]))
print("NA restantes tras imputación por comuna:")
print(na_after)

for (v in vars_imputar) {
  if (sum(is.na(casen[[v]])) > 0) {
    med_global <- median(casen[[v]], na.rm = TRUE)
    if (is.finite(med_global)) casen[[v]][is.na(casen[[v]])] <- med_global
  }
}

print("NA tras fallback global:")
print(colSums(is.na(casen[, vars_imputar])))

# 5) calcular la variables derivada
casen$tiempo_viaje_minutos <- casen$o28a_hr * 60 + casen$o28a_min
casen$tiempo_transporte_semanal <- (casen$tiempo_viaje_minutos * casen$o28b) / 60
casen$carga_laboral_total <- casen$o10 + casen$tiempo_transporte_semanal

print("Resumen de carga_laboral_total:")
print(summary(casen$carga_laboral_total))

### 3.4 RE-CODIFICACIÓN
casen$edad_cat = cut(casen$edad, breaks = c(0,30,40,50,60,70,80,Inf), labels = age_levels, right = FALSE, include.lowest = TRUE)
casen$esc_cat = factor(with(casen, ifelse(esc == 0, esc_levels[1], ifelse(esc <= 8, esc_levels[2], ifelse(esc <= 12, esc_levels[3], esc_levels[4])))), levels = esc_levels)
casen$sexo_cat = factor(ifelse(casen$sexo == 2, sexo_levels[1], ifelse(casen$sexo == 1, sexo_levels[2], NA)), levels = sexo_levels)
casen$ID = as.character(seq_len(nrow(casen)))

## 4. MICROSIMULACIÓN ####
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  if(!zona %in% names(inds_list)) return(NULL)
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  w_frac  = weight(cons = cons_i, inds = inds_i, vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i, tmp[, c("ID", "carga_laboral_total")], by = "ID", all.x = TRUE)
})

sim_list = sim_list[!sapply(sim_list, is.null)]
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

zonas_carga_laboral = aggregate(carga_laboral_total ~ zone, data = sim_df, FUN  = function(x) median(x, na.rm = TRUE))
names(zonas_carga_laboral) <- c("geocodigo", "mediana_carga_laboral")

## 5. CONEXIÓN A BD ####
con = dbConnect(Postgres(), dbname = "censo_rm", host = "localhost", port = 5432, user = "postgres", password = "1234")

# Escribir tabla
dbWriteTable(con, name = DBI::SQL("output.zonas_carga_laboral"), 
             value = zonas_carga_laboral, row.names = FALSE, overwrite = TRUE)

query_gs = "SELECT * FROM dpa.zonas_censales_rm WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))"
zonas_gs = st_read(con, query = query_gs)
zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)

# Unir datos
zonas_gs_completo = left_join(zonas_gs, zonas_carga_laboral, by = "geocodigo")

# Escribir tabla espacial 
st_write(zonas_gs_completo, dsn = con, layer = DBI::SQL("output.zc_carga_laboral"))
dplyr::n_distinct(zonas_gs_completo$mediana_carga_laboral)

## 6. GENERAR MAPA CON CUARTILES ####
zonas_gs_completo <- zonas_gs_completo %>%
  mutate(cuartil = factor(ntile(mediana_carga_laboral, 4),
                          labels = c("Q1 (Baja)", "Q2 (Medio-Baja)", "Q3 (Medio-Alta)", "Q4 (Alta)")))

# 6.1 OBTENER CONTORNOS DE COMUNAS
comunas_contorno <- zonas_gs_completo %>%
  group_by(nom_comuna) %>%
  summarise() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

# 6.2 CALCULAR CENTROIDES PARA ETIQUETAS
centroides_comunas <- comunas_contorno %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  )

# 6.3 CREAR MAPA ELEGANTE SIN FONDO
mapa_elegante <- ggplot() +
  # Capa de zonas censales con colores de cuartiles
  geom_sf(
    data = zonas_gs_completo,
    aes(fill = cuartil),
    color = "white",  #gray80
    alpha = 0.85,
    size = 0.1
  ) +
  
  # Capa de bordes de comunas
  geom_sf(
    data = comunas_contorno,
    fill = NA,
    color = "black",
    size = 0.8,
    alpha = 0.9
  ) +
  
  # Etiquetas de comunas con fondo para legibilidad
  geom_label(
    data = centroides_comunas,
    aes(x = x, y = y, label = nom_comuna),
    size = 1.8,
    color = "black",
    fontface = "bold",
    fill = "white",
    alpha = 0.85,
    linewidth = 0.3,     # Borde del label
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  
  # Escala de colores
  scale_fill_brewer(
    palette = "RdYlBu",
    direction = -1,
    name = "CUARTILES DE CARGA LABORAL",
    na.value = "grey90",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      keywidth = unit(1.5, "cm"),
      keyheight = unit(0.3, "cm")
    )
  ) +
  
  # Escala gráfica
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_col = "black",
    text_cex = 0.9,
    text_face = "bold",
    height = unit(0.25, "cm"),
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    line_width = 1.5
  ) +
  
  # Brújula (Cruz del Norte)
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_x = unit(0.3, "cm"),
    pad_y = unit(0.3, "cm"),
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black",
      text_col = "black",
      line_width = 1.2
    )
  ) +
  
  # Títulos y etiquetas
  labs(
    title = "DISTRIBUCIÓN TERRITORIAL DE LA CARGA LABORAL",
    subtitle = "Región Metropolitana - Análisis por Zona Censal\nCarga Laboral = Horas Trabajo + Horas Transporte",
    caption = "Fuente: Microsimulación basada en CASEN 2022 y Censo 2017\nElaboración propia"
  ) +
  
  # Tema minimalista
  theme_void() +
  theme(
    # Títulos principales
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 18,
      margin = margin(b = 8, t = 10),
      color = "black",
      family = "sans"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 12,
      margin = margin(b = 15, t = 5),
      color = "black",
      lineheight = 1.2,
      family = "sans"
    ),
    plot.caption = element_text(
      hjust = 0.5, 
      size = 9,
      color = "grey40",
      margin = margin(t = 12, b = 5),
      family = "sans"
    ),
    
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold",
      size = 11,
      margin = margin(b = 8),
      color = "black",
      family = "sans"
    ),
    legend.text = element_text(
      size = 10,
      color = "black",
      family = "sans"
    ),
    legend.background = element_rect(
      fill = "white", 
      color = "black", 
      linewidth = 0.3
    ),
    legend.box.background = element_rect(
      fill = "white", 
      color = "black",
      linewidth = 0.3
    ),
    legend.margin = margin(t = 8, b = 8, l = 15, r = 15),
    legend.box.margin = margin(t = 5, b = 5),
    
    # Márgenes y fondo general
    plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# 6.4 GUARDAR MAPA
#ggsave("mapa_carga_laboral_elegante.png", mapa_elegante, width = 30,height = 25,units = "cm",dpi = 400, bg = "white")

print(mapa_elegante)
