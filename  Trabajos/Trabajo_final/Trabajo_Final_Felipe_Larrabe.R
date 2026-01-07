# ============================================================================
# ANÁLISIS DE OFERTA, DEMANDA Y LOCALIZACIÓN ÓPTIMA PARA COMERCIO DE TABACO
# EN EL GRAN SANTIAGO
# ============================================================================

# ---------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS -----------------------------------------------------
# ---------------------------------------------------------------------------
library(tidyverse)    # Manipulación y visualización de datos
library(haven)        # Lectura de archivos .dta (Stata)
library(sf)           # Datos espaciales (Simple Features)
library(osmdata)      # Descarga de datos de OpenStreetMap
library(osrm)         # Cálculo de isócronas y rutas
library(pROC)         # Curvas ROC y evaluación de modelos
library(rakeR)        # Microsimulación por método raking
library(tmap)         # Creación de mapas temáticos
library(DBI)          # Interfaz para bases de datos
library(RPostgres)    # Conector PostgreSQL
library(patchwork)    # Combinación de gráficos (opcional)
library(janitor)      # Limpieza de nombres de columnas

# Configuración general
options(scipen = 999)  # Desactiva notación científica
set.seed(123)          # Reproducibilidad

# Crear directorios necesarios
dirs <- c("resultados", "mapas", "modelos", "data/preprocesados")
for(dir in dirs) {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# 2. CARGA DE DATOS ---------------------------------------------------------
# ---------------------------------------------------------------------------
cat("=== CARGA DE DATOS ===\n")

# 2.1 Datos EPF (Encuesta de Presupuestos Familiares)
cat("Cargando datos EPF...\n")
epf_personas <- read_dta("data/EPF/base-personas-ix-epf-stata.dta")
epf_gastos <- read_dta("data/EPF/base-gastos-ix-epf-stata.dta")
epf_cantidades <- read_dta("data/EPF/base-cantidades-ix-epf-stata.dta")
epf_ccif <- read_dta("data/EPF/ccif-ix-epf-stata.dta")

# 2.2 Datos CASEN
cat("Cargando datos CASEN...\n")
if (file.exists("data/casen_rm.rds")) {
  casen_completo <- readRDS("data/casen_rm.rds")
} else {
  cat("Advertencia: Archivo CASEN no encontrado\n")
  casen_completo <- NULL
}

# 2.3 Datos Censales para microsimulación
cat("Cargando datos censales...\n")
if (file.exists("data/cons_censo_df.rds")) {
  cons_censo_df <- readRDS("data/cons_censo_df.rds")
} else {
  cat("Advertencia: Archivo censal no encontrado\n")
  cons_censo_df <- NULL
}

# 3. PREPROCESAMIENTO DE DATOS EPF ------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== PREPROCESAMIENTO DE DATOS EPF ===\n")

# 3.1 Definir valores inválidos
valores_invalidos <- c(-99, -88, -77)

# 3.2 Filtrar Gran Santiago y datos válidos
epf_personas_filtrado <- epf_personas %>%
  filter(
    macrozona == 2,  # Gran Santiago
    !edad %in% valores_invalidos,
    !edue %in% valores_invalidos,
    ing_disp_hog_hd_ai >= 0,
    !is.na(sexo)
  ) %>%
  mutate(
    id_persona = paste(folio, n_linea, sep = "_"),
    ing_pc = ing_disp_hog_hd_ai / npersonas
  )

# 3.3 Identificar consumidores de tabaco
cat("Identificando consumidores de tabaco...\n")
consumidores_tabaco <- epf_cantidades %>%
  filter(
    ccif == "02.3.1.01.01",  # Código para productos de tabaco
    macrozona == 2
  ) %>%
  mutate(id_persona = paste(folio, n_linea, sep = "_")) %>%
  group_by(id_persona) %>%
  summarise(
    gasto_tabaco = sum(gasto, na.rm = TRUE),
    .groups = "drop"
  )

# 3.4 Combinar datos de personas con gasto en tabaco
epf_completo <- epf_personas_filtrado %>%
  left_join(consumidores_tabaco, by = "id_persona") %>%
  mutate(
    gasto_tabaco = replace_na(gasto_tabaco, 0),
    es_consumidor = ifelse(gasto_tabaco > 0, 1, 0)
  )

# 3.5 Crear variable de otros fumadores en el hogar
cat("Creando variable de otros fumadores en el hogar...\n")
fumadores_por_hogar <- epf_completo %>%
  filter(es_consumidor == 1) %>%
  group_by(folio) %>%
  summarise(
    n_fumadores_hogar = n(),
    .groups = "drop"
  ) %>%
  mutate(otros_fumadores = ifelse(n_fumadores_hogar > 1, 1, 0))

# Unir con datos principales
epf_completo <- epf_completo %>%
  left_join(fumadores_por_hogar %>% select(folio, otros_fumadores), 
            by = "folio") %>%
  mutate(otros_fumadores = replace_na(otros_fumadores, 0))

# 3.6 Crear variables categóricas para análisis
epf_completo <- epf_completo %>%
  mutate(
    # Grupo de edad
    grupo_edad = cut(
      edad,
      breaks = c(0, 29, 44, 64, Inf),
      labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
    ),
    # Nivel educativo
    grupo_escolaridad = cut(
      edue,
      breaks = c(-Inf, 12, 14, 16, Inf),
      labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
      right = TRUE
    ),
    # Sexo como factor
    sexo_factor = factor(sexo, labels = c("Hombre", "Mujer")),
    # Transformación logarítmica del ingreso
    log_ing_pc = log(ing_pc + 1)
  )

# 3.7 Preparar datos para modelización (solo consumidores)
datos_modelo_continuo <- epf_completo %>%
  filter(es_consumidor == 1, gasto_tabaco > 0) %>%
  mutate(
    sqrt_gasto_tabaco = sqrt(gasto_tabaco),
    log_gasto_tabaco = log(gasto_tabaco + 1)
  )

# Winsorización para outliers
winsorizar <- function(x, probs = c(0.01, 0.99)) {
  limites <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, limites[1]), limites[2])
}

if (nrow(datos_modelo_continuo) > 0) {
  datos_modelo_continuo <- datos_modelo_continuo %>%
    mutate(
      gasto_tabaco_wins = winsorizar(gasto_tabaco),
      sqrt_gasto_tabaco_wins = sqrt(gasto_tabaco_wins)
    )
}

# 4. ANÁLISIS EXPLORATORIO --------------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== ANÁLISIS EXPLORATORIO ===\n")

# 4.1 Función para formatear ejes monetarios
formatear_monetario <- function(x) {
  ifelse(x >= 1e6, 
         paste0(round(x/1e6, 1), "M"), 
         scales::comma(x))
}

# 4.2 Distribución de variables clave (solo si hay datos)
if (nrow(epf_completo) > 0) {
  p1_dist_ingreso <- ggplot(epf_completo, aes(x = ing_pc)) +
    geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
    scale_x_continuous(labels = formatear_monetario, limits = c(0, 3000000)) +
    labs(title = "Distribución del Ingreso per Cápita",
         x = "Ingreso per Cápita (CLP)", y = "Frecuencia") +
    theme_minimal()
  
  print(p1_dist_ingreso)
}

if (nrow(datos_modelo_continuo) > 0) {
  p2_dist_gasto <- ggplot(datos_modelo_continuo, aes(x = gasto_tabaco)) +
    geom_histogram(fill = "darkred", alpha = 0.7, bins = 30) +
    scale_x_continuous(labels = formatear_monetario) +
    labs(title = "Distribución del Gasto en Tabaco",
         x = "Gasto Mensual en Tabaco (CLP)", y = "Frecuencia") +
    theme_minimal()
  
  print(p2_dist_gasto)
}

# 4.3 Estadísticas descriptivas
cat("\nEstadísticas descriptivas:\n")
cat("Total de personas en muestra:", nrow(epf_completo), "\n")
cat("Consumidores de tabaco:", sum(epf_completo$es_consumidor, na.rm = TRUE), "\n")
cat("Tasa de prevalencia:", 
    round(mean(epf_completo$es_consumidor, na.rm = TRUE) * 100, 1), "%\n")

if (nrow(datos_modelo_continuo) > 0) {
  cat("\n--- Entre Consumidores ---\n")
  cat("Gasto promedio:", round(mean(datos_modelo_continuo$gasto_tabaco, na.rm = TRUE), 0), "CLP\n")
  cat("Gasto mediano:", round(median(datos_modelo_continuo$gasto_tabaco, na.rm = TRUE), 0), "CLP\n")
  cat("Porcentaje con otros fumadores en hogar:",
      round(mean(datos_modelo_continuo$otros_fumadores, na.rm = TRUE) * 100, 1), "%\n")
}

# 5. MODELIZACIÓN -----------------------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== MODELIZACIÓN ===\n")

# 5.1 Modelo Logit (Participación en consumo)
cat("\n--- Modelo Logit (Participación) ---\n")

# Filtrar datos completos para el modelo
datos_logit <- epf_completo %>%
  filter(!is.na(edad), !is.na(grupo_escolaridad), !is.na(sexo_factor))

modelo_logit <- glm(
  es_consumidor ~ sexo_factor + edad + grupo_escolaridad + log_ing_pc,
  data = datos_logit,
  family = binomial(link = "logit")
)

# Resumen
cat("Resumen del modelo logit:\n")
print(summary(modelo_logit))

# 5.2 Evaluación del modelo logit
datos_logit$prob_predicha <- predict(modelo_logit, type = "response")
roc_logit <- roc(datos_logit$es_consumidor, datos_logit$prob_predicha)
cat("\nAUC (Área bajo curva ROC):", round(auc(roc_logit), 3), "\n")

# Umbral óptimo
umbral_optimo <- coords(roc_logit, "best", ret = "threshold")$threshold
cat("Umbral óptimo (Youden):", round(umbral_optimo, 3), "\n")

# 5.3 Modelo Lineal (Intensidad de gasto) - CON VARIABLE DE OTROS FUMADORES
cat("\n--- Modelo Lineal (Intensidad) ---\n")

if (nrow(datos_modelo_continuo) > 0) {
  # Modelo incluyendo otros_fumadores
  modelo_lineal <- lm(
    sqrt_gasto_tabaco ~ log_ing_pc + sexo_factor + grupo_edad + 
      grupo_escolaridad + otros_fumadores,
    data = datos_modelo_continuo
  )
  
  # Resumen
  cat("Resumen del modelo lineal (con otros_fumadores):\n")
  print(summary(modelo_lineal))
  
  # Guardar modelos
  saveRDS(modelo_logit, "modelos/modelo_logit_final.rds")
  saveRDS(modelo_lineal, "modelos/modelo_lineal_final.rds")
  cat("\nModelos guardados en carpeta 'modelos/'\n")
}


# 6. CARACTERIZACIÓN DE DEMANDA ---------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== CARACTERIZACIÓN DE DEMANDA ===\n")

if (!is.null(casen_completo) && exists("modelo_logit") && exists("modelo_lineal")) {
  cat("Procesando datos CASEN para predicción...\n")
  
  # Definir comunas del Gran Santiago
  comunas_gs <- c(
    "13101", "13102", "13103", "13104", "13105", "13106", "13107", "13108",
    "13109", "13110", "13111", "13112", "13113", "13114", "13115", "13116",
    "13117", "13118", "13119", "13120", "13121", "13122", "13123", "13124",
    "13125", "13126", "13127", "13128", "13129", "13130", "13131", "13132",
    "13201", "13401"
  )
  
  # Preparar datos CASEN
  casen_procesado <- casen_completo %>%
    mutate(
      comuna_id = substr(as.character(estrato), 1, 5),
      ing_pc = as.numeric(ypc),
      log_ing_pc = log(ing_pc + 1),
      sexo_factor = factor(ifelse(sexo == 2, "Mujer", "Hombre"),
                           levels = c("Hombre", "Mujer")),
      grupo_edad = cut(
        edad,
        breaks = c(0, 29, 44, 64, Inf),
        labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
      ),
      grupo_escolaridad = cut(
        esc,
        breaks = c(-Inf, 12, 14, 16, Inf),
        labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
        right = TRUE
      ),
      otros_fumadores = 0  # Valor por defecto para predicción
    ) %>%
    filter(
      comuna_id %in% comunas_gs,
      edad >= 15,
      !is.na(ing_pc),
      ing_pc > 0
    )
  
  # Predicción de participación
  casen_procesado$prob_consumo <- predict(modelo_logit, 
                                          newdata = casen_procesado, 
                                          type = "response")
  
  # Clasificar consumidores potenciales
  casen_procesado <- casen_procesado %>%
    mutate(consumidor_potencial = ifelse(prob_consumo >= umbral_optimo, 1, 0))
  
  # Predicción de gasto para consumidores
  consumidores_casen <- casen_procesado %>%
    filter(consumidor_potencial == 1) %>%
    mutate(
      gasto_predicho_sqrt = predict(modelo_lineal, newdata = .),
      gasto_predicho = (gasto_predicho_sqrt)^2
    )
  
  # 6.1 Microsimulación (si hay datos censales)
  if (!is.null(cons_censo_df)) {
    cat("Realizando microsimulación...\n")
    
    # Preparar datos para raking
    consumidores_casen <- consumidores_casen %>%
      mutate(
        id = row_number(),
        sexo_cat = ifelse(sexo_factor == "Mujer", "sexo_f", "sexo_m"),
        edad_cat = cut(
          edad,
          breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
          labels = paste0("edad_", c("0_30", "30_40", "40_50", "50_60", 
                                     "60_70", "70_80", "mayor_80")),
          right = FALSE
        ),
        esc_cat = cut(
          esc,
          breaks = c(-Inf, 0, 8, 12, Inf),
          labels = paste0("esco_", c("0", "1_8", "8_12", "mayor_12")),
          right = TRUE
        )
      )
    
    # Identificar columnas del censo
    columnas_censo <- colnames(cons_censo_df)
    sexo_cols <- grep("^sexo_", columnas_censo, value = TRUE)
    edad_cols <- grep("^edad_", columnas_censo, value = TRUE)
    esc_cols <- grep("^esco_", columnas_censo, value = TRUE)
    
    # Preparar restricciones
    cons_rake <- cons_censo_df %>%
      select(GEOCODIGO, all_of(c(sexo_cols, edad_cols, esc_cols)))
    
    # Datos individuales
    inds_rake <- consumidores_casen %>%
      select(id, sexo_cat, edad_cat, esc_cat) %>%
      rename(Sexo = sexo_cat, Edad = edad_cat, Escolaridad = esc_cat)
    
    # Raking
    weights_rake <- weight(
      cons = cons_rake,
      inds = inds_rake,
      vars = c("Sexo", "Edad", "Escolaridad")
    )
    
    # Integerización
    microdatos_expandidos <- integerise(
      weights = weights_rake,
      inds = inds_rake,
      seed = 123
    )
    
    # Combinar con gasto
    microdatos_completos <- microdatos_expandidos %>%
      left_join(
        consumidores_casen %>% select(id, gasto_predicho),
        by = c("id" = "id")
      )
    
    # Agregar por zona
    demanda_por_zona <- microdatos_completos %>%
      group_by(zone) %>%
      summarise(
        gasto_total_mensual = sum(gasto_predicho, na.rm = TRUE),
        n_consumidores = n(),
        gasto_promedio = mean(gasto_predicho, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(GEOCODIGO = zone) %>%
      mutate(GEOCODIGO = as.character(GEOCODIGO))
    
    # Guardar
    saveRDS(demanda_por_zona, "data/preprocesados/demanda_por_zona.rds")
    cat("Demanda calculada y guardada en: data/preprocesados/demanda_por_zona.rds\n")
  }
} else {
  cat("No se puede calcular demanda: datos CASEN o modelos no disponibles\n")
  demanda_por_zona <- NULL
}

# 7. CARACTERIZACIÓN DE OFERTA ----------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== CARACTERIZACIÓN DE OFERTA ACTUAL ===\n")

# 7.1 Conectar a base de datos para zonas censales
cat("Obteniendo zonas censales...\n")
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "1234"
)

query_zonas <- "SELECT * FROM dpa.zonas_censales_rm WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))"

zonas_censales <- st_read(con, query = query_zonas) %>%
  mutate(GEOCODIGO = as.character(geocodigo))

dbDisconnect(con)

# 7.2 Cargar archivos preprocesados de isócronas y oferta
if (file.exists("isocronas_santiago_final.rds")) {
  cat("Respaldo detectado. Cargando oferta y lista de isócronas...\n")
  respaldo_iso <- readRDS("isocronas_santiago_final.rds")
  isocronas_lista <- respaldo_iso$isocronas
  oferta_gs <- respaldo_iso$oferta
} else {
  cat("No hay respaldo. Descargando oferta de OSM...\n")
  # Transformar zonas a 4326 para el bounding box
  zonas_mapa <- st_transform(zonas_censales, 4326)
  bbox_gs <- st_bbox(zonas_mapa)
  
  query_gs <- opq(bbox = bbox_gs, timeout = 100) %>%
    add_osm_feature(key = "shop", 
                    value = c("tobacco", "convenience", "supermarket", "kiosk", "liquor")) %>%
    osmdata_sf()
  
  oferta_gs <- query_gs$osm_points %>% 
    filter(!is.na(shop)) %>% 
    st_transform(4326)
}

# 7.3 Generar cobertura con OSRM
# Si no se cargó 'isocronas_lista' en el paso anterior, se calcula
if (!exists("isocronas_lista") || is.null(isocronas_lista)) {
  cat(paste("Calculando isócronas para", nrow(oferta_gs), "locales...\n"))
  isocronas_lista <- map(1:nrow(oferta_gs), function(i) {
    if(i %% 50 == 0) cat(paste("Progreso:", i, "locales...\n"))
    tryCatch({
      osrmIsochrone(loc = oferta_gs[i, ], breaks = 15, osrm.profile = "foot")
    }, error = function(e) return(NULL))
  })
  
  # Guardar respaldo inmediato
  saveRDS(list(isocronas = isocronas_lista, oferta = oferta_gs), 
          "isocronas_santiago_final.rds")
}

# 7.4 Optimización de geometría y cruce espacial
# Intentar cargar el cruce ya procesado
if (file.exists("zonas_con_cobertura_final.rds")) {
  cat("Cargando zonas con cobertura ya procesada...\n")
  zonas_utm <- readRDS("zonas_con_cobertura_final.rds")
  
  # Si no existe isocronas_sf pero existe isocronas_lista, crearla
  if (!exists("isocronas_sf") && exists("isocronas_lista")) {
    isocronas_sf <<- isocronas_lista %>% 
      keep(~ !is.null(.x)) %>% 
      bind_rows() %>% 
      st_transform(32719)
    cat("isocronas_sf creada a partir de isocronas_lista.\n")
  }
} else {
  cat("Unificando polígonos y cruzando con centroides...\n")
  
  # Unificar isócronas válidas
  isocronas_sf <<- isocronas_lista %>%  # <<- para asignar al entorno global
    keep(~ !is.null(.x)) %>% 
    bind_rows() %>% 
    st_transform(32719)  # Transformar a UTM 19S para metros
  
  # Transformar zonas a UTM
  zonas_utm <- st_transform(zonas_censales, 32719)
  centroides_utm <- st_centroid(zonas_utm)
  
  # Calcular intersección
  interseccion <- st_intersects(centroides_utm, isocronas_sf, sparse = FALSE)
  zonas_utm$esta_cubierta <- apply(interseccion, 1, any)
  
  # Guardar estado final
  saveRDS(zonas_utm, "zonas_con_cobertura_final.rds")
  cat("Cobertura calculada y guardada\n")
}

# 7.5 Preparar datos de oferta para visualización
# Asegurarse de que oferta_gs esté en el CRS correcto
oferta_visualizacion <- oferta_gs %>%
  st_transform(st_crs(zonas_utm))

# 8. ANÁLISIS DE BRECHA -----------------------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== ANÁLISIS DE BRECHA OFERTA-DEMANDA ===\n")

# PRIMERO: Verificar y calcular esta_cubierta si es necesario
# Verificar si zonas_utm existe
if (!exists("zonas_utm")) {
  cat("ERROR: zonas_utm no existe. No se puede calcular brecha.\n")
  zonas_con_demanda <- NULL
} else {
  # Verificar si esta_cubierta existe en zonas_utm
  if (!"esta_cubierta" %in% colnames(zonas_utm)) {
    cat("Calculando cobertura (usando centroides)...\n")
    
    # Necesitamos cobertura_total o isocronas_sf
    if (exists("cobertura_total")) {
      # Asegurar mismo CRS
      cobertura_utm <- st_transform(cobertura_total, st_crs(zonas_utm))
      
      # Calcular centroides
      centroides_utm <- st_centroid(zonas_utm)
      
      # Calcular intersección CENTROIDE con cobertura
      interseccion <- st_intersects(centroides_utm, cobertura_utm, sparse = FALSE)
      zonas_utm$esta_cubierta <- apply(interseccion, 1, any)
      
      cat("Cobertura calculada. Zonas cubiertas:", sum(zonas_utm$esta_cubierta), 
          "de", nrow(zonas_utm), "\n")
    } else {
      cat("ADVERTENCIA: No hay datos de cobertura. Todas las zonas NO cubiertas.\n")
      zonas_utm$esta_cubierta <- FALSE
    }
  }
  
  # Inicializar zonas_con_demanda con valores por defecto
  zonas_con_demanda <- zonas_utm
  
  # Agregar columnas de demanda si están disponibles
  if (exists("demanda_por_zona") && !is.null(demanda_por_zona)) {
    
    # Verificar que demanda_por_zona tenga GEOCODIGO
    if (!"GEOCODIGO" %in% colnames(demanda_por_zona)) {
      cat("ADVERTENCIA: demanda_por_zona no tiene columna 'GEOCODIGO'. Buscando alternativas...\n")
      # Buscar columna alternativa para unir
      posibles_uniones <- intersect(colnames(demanda_por_zona), c("zone", "GEOCODIGO", "geocodigo", "codigo"))
      if (length(posibles_uniones) > 0) {
        columna_union <- posibles_uniones[1]
        cat("Usando columna", columna_union, "para la unión\n")
        # Renombrar a GEOCODIGO temporalmente
        demanda_por_zona <- demanda_por_zona %>%
          rename(GEOCODIGO = !!columna_union)
      } else {
        cat("ERROR: No hay columnas para unir demanda_por_zona con zonas_utm\n")
        demanda_por_zona <- NULL
      }
    }
    
    if (!is.null(demanda_por_zona) && "GEOCODIGO" %in% colnames(zonas_utm)) {
      # Asegurar que GEOCODIGO sea character en ambos
      zonas_utm <- zonas_utm %>%
        mutate(GEOCODIGO = as.character(GEOCODIGO))
      demanda_por_zona <- demanda_por_zona %>%
        mutate(GEOCODIGO = as.character(GEOCODIGO))
      
      # Verificar coincidencias
      cat("\n--- Verificación de coincidencias ---\n")
      comunes <- intersect(zonas_utm$GEOCODIGO, demanda_por_zona$GEOCODIGO)
      cat("Número de códigos comunes:", length(comunes), "\n")
      cat("Número de filas en demanda_por_zona:", nrow(demanda_por_zona), "\n")
      cat("Suma de gasto_total_mensual en demanda_por_zona:", 
          sum(demanda_por_zona$gasto_total_mensual, na.rm = TRUE), "CLP\n")
      
      # Realizar la unión
      zonas_con_demanda <- zonas_utm %>%
        left_join(demanda_por_zona, by = "GEOCODIGO", suffix = c("", "_demanda"))
      
      # Consolidar columnas
      if ("gasto_total_mensual_demanda" %in% colnames(zonas_con_demanda)) {
        zonas_con_demanda <- zonas_con_demanda %>%
          mutate(
            gasto_total_mensual = ifelse(
              is.na(gasto_total_mensual_demanda),
              gasto_total_mensual,
              gasto_total_mensual_demanda
            )
          ) %>%
          select(-gasto_total_mensual_demanda)
      }
      
      if ("n_consumidores_demanda" %in% colnames(zonas_con_demanda)) {
        zonas_con_demanda <- zonas_con_demanda %>%
          mutate(
            n_consumidores = ifelse(
              is.na(n_consumidores_demanda),
              n_consumidores,
              n_consumidores_demanda
            )
          ) %>%
          select(-n_consumidores_demanda)
      }
      
      cat("Unión completada. Filas en zonas_con_demanda:", nrow(zonas_con_demanda), "\n")
      
    } else {
      cat("ADVERTENCIA: No se puede unir demanda con zonas. Creando columnas con valor 0.\n")
      zonas_con_demanda <- zonas_con_demanda %>%
        mutate(
          gasto_total_mensual = 0,
          n_consumidores = 0
        )
    }
    
  } else {
    # Si no hay datos de demanda, crear columnas con 0
    cat("ADVERTENCIA: No hay datos de demanda disponibles.\n")
    zonas_con_demanda <- zonas_con_demanda %>%
      mutate(
        gasto_total_mensual = 0,
        n_consumidores = 0
      )
  }
  
  # Calcular brecha
  zonas_con_demanda <- zonas_con_demanda %>%
    mutate(
      brecha_clp = ifelse(esta_cubierta, 0, gasto_total_mensual)
    )
  
  # Diagnóstico: ver cuántas zonas no cubiertas tienen brecha > 0
  cat("\n--- Diagnóstico de zonas no cubiertas ---\n")
  zonas_no_cubiertas <- zonas_con_demanda %>% filter(!esta_cubierta)
  cat("Total de zonas no cubiertas:", nrow(zonas_no_cubiertas), "\n")
  cat("Zonas con brecha_clp > 0:", sum(zonas_no_cubiertas$brecha_clp > 0, na.rm = TRUE), "\n")
  cat("Suma total de brecha_clp:", sum(zonas_no_cubiertas$brecha_clp, na.rm = TRUE), "CLP\n")
  
  if (sum(zonas_no_cubiertas$brecha_clp > 0, na.rm = TRUE) > 0) {
    cat("\nTop 5 zonas con mayor brecha:\n")
    top_brechas <- zonas_no_cubiertas %>%
      arrange(desc(brecha_clp)) %>%
      head(5) %>%
      st_drop_geometry() %>%
      select(GEOCODIGO, nom_comuna, gasto_total_mensual, brecha_clp, n_consumidores)
    print(top_brechas)
  }
  
  # Calcular métricas
  demanda_total <- sum(zonas_con_demanda$gasto_total_mensual, na.rm = TRUE)
  demanda_cubierta <- sum(zonas_con_demanda$gasto_total_mensual[zonas_con_demanda$esta_cubierta], na.rm = TRUE)
  demanda_no_cubierta <- sum(zonas_con_demanda$brecha_clp, na.rm = TRUE)
  
  cat("\n--- Métricas de Brecha ---\n")
  cat("Demanda total estimada:", round(demanda_total, 0), "CLP/mes\n")
  cat("Demanda cubierta:", round(demanda_cubierta, 0), "CLP/mes\n")
  cat("Demanda no cubierta (brecha):", round(demanda_no_cubierta, 0), "CLP/mes\n")
  
  if (demanda_total > 0) {
    cat("Porcentaje cubierto:", round(demanda_cubierta/demanda_total * 100, 1), "%\n")
  } else {
    cat("Porcentaje cubierto: 0% (demanda total es 0)\n")
  }
  
  # Mostrar estadísticas adicionales
  cat("\n--- Estadísticas de Cobertura ---\n")
  cat("Total de zonas:", nrow(zonas_con_demanda), "\n")
  cat("Zonas cubiertas:", sum(zonas_con_demanda$esta_cubierta), "\n")
  cat("Zonas no cubiertas:", sum(!zonas_con_demanda$esta_cubierta), "\n")
  cat("Porcentaje de zonas cubiertas:", 
      round(mean(zonas_con_demanda$esta_cubierta) * 100, 1), "%\n")
  
  # Crear zonas_prioritarias
  zonas_prioritarias <- zonas_con_demanda %>%
    filter(!esta_cubierta) %>%
    arrange(desc(gasto_total_mensual)) %>%
    slice_head(n = 5) %>%
    mutate(
      criterio = "gasto_mas_alto",
      ranking = row_number()
    )
}

# 9. PROPUESTA DE UBICACIONES ÓPTIMAS ---------------------------------------
# ---------------------------------------------------------------------------
cat("\n=== PROPUESTA DE UBICACIONES ÓPTIMAS ===\n")

# Verificar si zonas_prioritarias existe y tiene datos
if (exists("zonas_prioritarias") && nrow(zonas_prioritarias) > 0) {
  
  # 9.1 Seleccionar candidatas (top 5 por demanda)
  # Calcular cuantil 75 de manera segura
  if (sum(zonas_prioritarias$gasto_total_mensual > 0) >= 4) {
    quantil_valor <- quantile(zonas_prioritarias$gasto_total_mensual[zonas_prioritarias$gasto_total_mensual > 0], 
                              0.75, na.rm = TRUE)
  } else {
    quantil_valor <- 0
  }
  
  zonas_candidatas <- zonas_prioritarias %>%
    filter(gasto_total_mensual > quantil_valor) %>%
    arrange(desc(gasto_total_mensual)) %>%
    slice_head(n = 5) %>%  # Usar slice_head en lugar de top_n para mayor control
    mutate(
      centroide = st_centroid(geom),
      propuesta_num = row_number()
    )
  
  cat("\nUbicaciones propuestas para nuevos puntos de venta:\n")
  for (i in 1:nrow(zonas_candidatas)) {
    cat(sprintf("\n%d. Zona: %s (Comuna: %s)", 
                i, 
                zonas_candidatas$GEOCODIGO[i],
                ifelse("nom_comuna" %in% colnames(zonas_candidatas), 
                       zonas_candidatas$nom_comuna[i], "Desconocida")))
    cat(sprintf("\n   Demanda: %s CLP/mes", 
                format(round(zonas_candidatas$gasto_total_mensual[i]), big.mark = ".")))
    cat(sprintf("\n   Consumidores: %d", 
                zonas_candidatas$n_consumidores[i]))
  }
} else {
  cat("No hay zonas prioritarias identificadas\n")
  zonas_candidatas <- NULL
}

# 10. VISUALIZACIÓN ESPACIAL (Flujo Optimizado tmap v4) ----------------------
# ---------------------------------------------------------------------------
cat("\n=== INICIANDO VISUALIZACIÓN INTERACTIVA (tmap v4) ===\n")

# 10.1 Configuración de Entorno
tmap_mode("view") 

# 10.2 Validación de Capas Base
if (!exists("zonas_con_demanda")) stop("Error: 'zonas_con_demanda' no encontrada.")

# --- PREPARACIÓN DE CAPAS DE REFERENCIA ---

# Capa de Comunas (Wireframe)
if (exists("comunas_rm_shp")) {
  comunas_gs <- st_transform(comunas_rm_shp, st_crs(zonas_con_demanda))
} else {
  cat("Generando límites comunales por agregación...\n")
  comunas_gs <- zonas_con_demanda %>%
    group_by(nom_comuna) %>%
    summarise(geom = st_union(geom), .groups = "drop")
}

# --- COBERTURA UNIFICADA (simplificada para mapas) ---
# Solo si isocronas_sf existe y hay datos
if (exists("isocronas_sf") && !is.null(isocronas_sf) && nrow(isocronas_sf) > 0) {
  cat("Creando cobertura unificada simplificada para visualización...\n")
  
  # Crear versión simplificada y unificada de las isócronas
  cobertura_unificada <- isocronas_sf %>%
    # 1. Asegurar validez geométrica
    st_make_valid() %>%
    # 2. Unificar todos los polígonos (más eficiente para visualización)
    st_union() %>%
    # 3. Simplificar geometría para reducir tamaño
    st_simplify(dTolerance = 10) %>%  # 10 metros de tolerancia (ajustable)
    # 4. Convertir a simple feature
    st_sf() %>%
    # 5. Asegurar mismo CRS
    st_transform(st_crs(zonas_con_demanda))
  
  cat("✓ Cobertura unificada creada: ", 
      format(object.size(cobertura_unificada), units = "auto"), "\n")
  
} else {
  cat("⚠ ADVERTENCIA: No hay datos de isócronas para crear cobertura.\n")
  cobertura_unificada <- NULL
}

# 10.3 --- GENERACIÓN DE MAPAS ---

# =========================================================================
# MAPA 1: OFERTA ACTUAL (Color Chillón + Guardado Externo)
# =========================================================================

mapa_oferta <- tm_shape(zonas_con_demanda) + 
  # Base
  tm_polygons(fill = "grey95", col = "orange", lwd = 0.1, fill_alpha = 0.4) +
  
  # Oferta (Puntos rojos)
  (if(!is.null(oferta_visualizacion)) {
    tm_shape(oferta_visualizacion) + tm_dots(fill = "red", size = 0.2)
  } else tm_symbols()) +
  
  # Límites Comunales
  tm_shape(comunas_gs) +
  tm_polygons(fill_alpha = 0, col = "black", lwd = 1.2, interactive = FALSE) +
  
  # ETIQUETAS: Celeste Chillón con Fondo Blanco Sólido
  tm_text("nom_comuna", 
          size = 0.8, 
          col = "#0055ff",         # Un azul eléctrico fuerte
          fontface = "bold",
          bg.color = "white",      # FONDO BLANCO
          bg.alpha = 1) +          # 100% OPACITY (Sin transparencia)
  
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_title("Mapa 1: Oferta Actual")

print(mapa_oferta)

# =========================================================================
# MAPA 2: COBERTURA
# =========================================================================
cat("Generando Mapa 2: Cobertura Peatonal con elementos cartográficos...\n")

# Verificar si cobertura_unificada existe
if (exists("cobertura_unificada") && !is.null(cobertura_unificada)) {
  mapa_cobertura <- tm_shape(zonas_con_demanda, name = "Zonas Censales") + 
    # Base con bordes naranjas
    tm_polygons(fill = "grey95", col = "orange", lwd = 0.1) +
    # Capa de Cobertura (Isócronas)
    tm_shape(cobertura_unificada, name = "Área de Cobertura (15 min)") + 
    tm_polygons(fill = "cyan", fill_alpha = 0.4, col = "blue", lwd = 0.5) +
    # Límites Comunales
    tm_shape(comunas_gs, name = "Comunas") +
    tm_polygons(fill_alpha = 0, col = "darkblue", lwd = 1.2, interactive = FALSE) +
    # --- Elementos Cartográficos ---
    tm_compass(type = "arrow", position = c("right", "top"), size = 2) +
    tm_scalebar(breaks = c(0, 5, 10), position = c("left", "bottom"), text.size = 0.6) +
    tm_title("Mapa 2: Cobertura Peatonal (15 min)")
  
  print(mapa_cobertura)
} else {
  cat("ADVERTENCIA: isocronas_sf no está disponible. Saltando Mapa 2.\n")
}

# =========================================================================
# MAPA 3: DEMANDA POTENCIAL
# =========================================================================

cat("Generando Mapa 3: Demanda Potencial...\n")

if ("gasto_total_mensual" %in% colnames(zonas_con_demanda)) {
  
  # 1. Recalcular breaks asegurando que sean ÚNICOS 
  valores <- zonas_con_demanda$gasto_total_mensual
  valores_no_cero <- valores[valores > 0 & !is.na(valores)]
  
  if (length(unique(valores_no_cero)) > 5) {
    brks <- quantile(valores_no_cero, probs = seq(0, 1, 0.25), na.rm = TRUE)
    brks <- unique(c(0, brks)) 
  } else {
    brks <- NULL 
  }
  
  # 2. Construcción del mapa
  mapa_demanda <- tm_shape(zonas_con_demanda, name = "Gasto Mensual") +
    tm_polygons(
      fill = "gasto_total_mensual", 
      fill.scale = if(!is.null(brks)) {
        tm_scale_intervals(values = "YlOrRd", breaks = brks)
      } else {
        tm_scale_intervals(values = "YlOrRd") 
      },
      fill.legend = tm_legend(title = "Demanda (CLP)"),
      col = "white", 
      lwd = 0.1
    ) +
    
    # Capa de Comunas para contexto
    tm_shape(comunas_gs) +
    tm_polygons(fill_alpha = 0, col = "white", lwd = 1.5, interactive = FALSE) +
    tm_polygons(fill_alpha = 0, col = "black", lwd = 0.5, interactive = FALSE) +
    
    # --- ELEMENTOS CARTOGRÁFICOS ---
    tm_scalebar(position = c("left", "bottom"), text.size = 0.6) + # Escala gráfica
    tm_compass(type = "arrow", position = c("right", "top"), size = 1.5) + # Flecha de Norte
    
    tm_title("Mapa 3: Demanda Potencial") +
    tm_layout(frame = FALSE) # Limpia el recuadro exterior para un look más moderno
  
  # 3. Forzar el despliegue
  print(mapa_demanda)
  
} else {
  cat("ERROR: La columna 'gasto_total_mensual' no existe en el objeto.\n")
}

# =========================================================================
# MAPA 4: BRECHA
# =========================================================================
cat("Generando Mapa 4 con elementos cartográficos...\n")
if ("brecha_clp" %in% colnames(zonas_con_demanda)) {
  datos_positivos <- zonas_con_demanda$brecha_clp[zonas_con_demanda$brecha_clp > 0]
  if (length(datos_positivos) > 0) {
    brks_fijos <- unique(c(0, quantile(datos_positivos, probs = seq(0.2, 1, by = 0.2), na.rm = TRUE)))
    mapa_brecha <- tm_shape(zonas_con_demanda) +
      tm_polygons(
        fill = "brecha_clp",
        fill.scale = tm_scale_intervals(values = "YlOrRd", style = "fixed", breaks = brks_fijos),
        fill.legend = tm_legend(title = "Brecha (CLP)"),
        col = "grey80", lwd = 0.05
      ) +
      # --- Elementos Cartográficos ---
      # Brújula/Flecha Norte: Ubicada arriba a la derecha
      tm_compass(type = "8star", position = c("right", "top"), size = 2) +
      # Escala Gráfica: Ubicada abajo a la izquierda, en kilómetros
      tm_scalebar(breaks = c(0, 5, 10), position = c("left", "bottom"), text.size = 0.6) +
      # Capa de Comunas
      tm_shape(comunas_gs) +
      tm_polygons(fill_alpha = 0, col = "darkblue", lwd = 1.2, interactive = FALSE) +
      tm_title("Mapa 4: Brecha Oferta-Demanda")
    if(!is.null(oferta_visualizacion)) {
      mapa_brecha <- mapa_brecha + 
        tm_shape(oferta_visualizacion) + 
        tm_dots(fill = "darkgreen", size = 0.4)
    }
    print(mapa_brecha)
  }
}
# =========================================================================
# MAPA 5: PROPUESTA FINAL
# =========================================================================
if (exists("zonas_candidatas")) {
  cat("Generando Mapa 5: Ubicaciones Recomendadas con elementos cartográficos...\n")
  mapa_propuestas <- tm_shape(zonas_con_demanda, name = "Zonas de Contexto") +
    # Fondo con bordes naranjas previos
    tm_polygons(
      fill = "grey90", 
      col = "orange", 
      lwd = 0.1
    ) +
    # Zonas Propuestas con borde Cyan
    tm_shape(zonas_candidatas, name = "Zonas Propuestas") +
    tm_polygons(
      fill = "red", 
      col = "cyan", 
      lwd = 2,          # Borde más grueso para resaltar
      title = "Propuesta"
    ) +
    # Límites de Comunas en Negro
    tm_shape(comunas_gs, name = "Límites Comunales") +
    tm_polygons(
      fill_alpha = 0, 
      col = "black", 
      lwd = 1.5, 
      interactive = FALSE
    ) +
    # --- Elementos Cartográficos ---
    tm_compass(
      type = "arrow", 
      position = c("right", "top"), 
      size = 2
    ) +
    tm_scalebar(
      breaks = c(0, 5, 10), 
      position = c("left", "bottom"), 
      text.size = 0.6
    ) +
    tm_title("Mapa 5: Ubicación Propuesta Estratégica")
  # Desplegar en el Viewer
  print(mapa_propuestas)
}