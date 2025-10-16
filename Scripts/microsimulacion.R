## 1. Librerías ####
install.packages("rakeR")
install.packages("RPostgres")
install.packages("DBI")
install.packages("data.table")
library(rakeR)
library(RPostgres)
library(DBI)
library(data.table)

## 2. Entradas ####
getwd()
# Rutas de las entradas
ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

# DF's de CASEN y CENSO
casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

## 3. PRE-PROCESAMIENTO ####

### 3.1 CENSO

# Extraemos los nombres de las columnas que nos son útiles
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))


# Generamos los niveles para edad, escolaridad y sexo

age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)


### 3.2 CASEN 

# Se seleccionan las variables de interés

vars_base = c("estrato", # Para extraer comuna
              "esc", # Para años de escolaridad 
              "edad",
              "sexo",
              "e6a", # Para imputar datos 
              "ypc") # IMPORTANTE: VARIABLE A MICROSIMULAR


# Se filtra la CASEN con las variables de interés
casen = casen_raw[, vars_base, drop = FALSE]


# Limpiar memoria
# Importante para objetos muy grandes
rm(casen_raw)


# Extraemos la comuna para cada registro
casen$Comuna = substr(as.character(casen$estrato), 1, 5)

# Se elimina la columna estrato
casen$estrato = NULL


# Quitar etiquetas haven y cambiar tipo de datos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$ypc = as.numeric(unclass(casen$ypc))


# Imputar datos de esc en base a e6a
## esc = B0 + B1 * e6a 

# Reconocer donde están los NA's

# Ajustar un modelo con casos donde no hay na's


# predecir los casos con NA's 


# Imputación lineal de esc en base a e6a
idx_na = which(is.na(casen$esc))

# Ajustar modelo con casos en donde no hay na's
fit = lm(esc ~ e6a, data = casen[-idx_na,])

# Predicción para los casos con NA
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

# Imputar acotada
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))
# Añadimos a un ID único
casen$ID = as.character(seq_len(nrow(casen)))


### 3.3 RE-CODIFICACIÓN

# Categorización de edad
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)



# Categorización de Escolaridad
casen$esc_cat = factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)


# Recodificación de sexo
casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)
## 4. MICROSIMULACIÓN ####

# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","ypc")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Se agregan los datos
zonas_ypc = aggregate(
  ypc ~ zone,
  data = sim_df,
  FUN  = function(x) median(x, na.rm = TRUE)
)
names(zonas_ypc) <- c("geocodigo", "mediana_ingreso")
