# ============================================================
# seed_pmp.R
# Proyecto: Libro de Estadística (PMP)
# Función: Poblar catálogos PMP + entregables base y verificar
# ============================================================

suppressPackageStartupMessages({
  library(DBI); library(RMariaDB); library(dplyr); library(glue); library(purrr); library(tidyr)
})

`%||%` <- function(x,y) if(is.null(x) || length(x)==0) y else x

# ---------------- Conexión ----------------
get_con <- function(){
  host <- Sys.getenv("MYSQL_HOST"); if(!nzchar(host)) host <- "localhost"
  port <- Sys.getenv("MYSQL_PORT"); if(!nzchar(port)) port <- "3306"
  user <- Sys.getenv("MYSQL_USER"); if(!nzchar(user)) user <- "root"
  pass <- Sys.getenv("MYSQL_PASSWORD")
  db   <- Sys.getenv("MYSQL_DBNAME"); if(!nzchar(db)) db <- "libro"
  if(!nzchar(pass)) stop("MYSQL_PASSWORD está vacío. Define Sys.setenv(MYSQL_PASSWORD='...').")
  
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = host, port = as.integer(port), user = user, password = pass,
    dbname = db, bigint = "integer"
  )
}

# ---------------- Utilitarios ----------------
tbl_exists <- function(con, tbl) tbl %in% dbListTables(con)
safe_q <- function(con, sql) tryCatch(dbGetQuery(con, sql), error=function(e) tibble())
next_ids_seq <- function(existing_ids, n) {
  start <- if(length(existing_ids) && all(!is.na(existing_ids))) max(existing_ids) else 0L
  seq.int(from = as.integer(start) + 1L, length.out = n)
}

# ============================================================
# 1) ENTREGABLE_TIPO
# ============================================================
seed_entregable_tipo <- function(con){
  if(!tbl_exists(con,"entregable_tipo")) { message("⚠️ No existe entregable_tipo"); return(invisible()) }
  ex <- dbGetQuery(con,"SELECT tipo_id, nombre FROM entregable_tipo")
  faltan <- setdiff(c("Documento","Matriz","Informe","Registro"), ex$nombre %||% character())
  if(length(faltan)){
    # Si tipo_id no es AI, generamos IDs secuenciales
    if("tipo_id" %in% names(ex) && nrow(ex)) {
      nuevos_ids <- next_ids_seq(ex$tipo_id, length(faltan))
      vals <- paste(mapply(function(id, nom)
        sprintf("(%d,%s)", id, dbQuoteString(con, nom)), nuevos_ids, faltan), collapse = ", ")
      dbExecute(con, glue("INSERT INTO entregable_tipo (tipo_id, nombre) VALUES {vals}"))
    } else {
      vals <- paste(sprintf("(%s,%s)", "DEFAULT", dbQuoteString(con,faltan)), collapse=", ")
      dbExecute(con, glue("INSERT INTO entregable_tipo (tipo_id, nombre) VALUES {vals}"))
    }
  }
  message("✅ entregable_tipo listo")
}

# ============================================================
# 2) FASE  (fecha_inicio/fecha_fin son NOT NULL → asignar fechas)
# ============================================================
seed_fase <- function(con){
  if(!tbl_exists(con,"fase")) { message("⚠️ No existe fase"); return(invisible()) }
  
  ex <- dbGetQuery(con,"SELECT fase_id, nombre FROM fase")
  base <- c("Fase 1 – Planificación",
            "Fase 2 – Redacción inicial",
            "Fase 3 – Revisión",
            "Fase 4 – Edición",
            "Fase 5 – Cierre",
            "Fase 6 – Seguimiento")
  
  faltan <- setdiff(base, ex$nombre %||% character())
  if(length(faltan)){
    nuevos_ids <- next_ids_seq(ex$fase_id %||% integer(), length(faltan))
    hoy  <- as.character(Sys.Date())
    fin  <- as.character(Sys.Date() + 30)
    
    df <- tibble::tibble(
      fase_id = nuevos_ids,
      nombre = faltan,
      fecha_inicio = hoy,
      fecha_fin = fin
    )
    
    vals <- apply(df, 1, function(r)
      sprintf("(%d,%s,%s,%s)",
              as.integer(r[["fase_id"]]),
              dbQuoteString(con, r[["nombre"]]),
              dbQuoteString(con, r[["fecha_inicio"]]),
              dbQuoteString(con, r[["fecha_fin"]]))
    )
    dbExecute(con, glue("INSERT INTO fase (fase_id, nombre, fecha_inicio, fecha_fin) VALUES {paste(vals, collapse=', ')}"))
  }
  message("✅ fase listo")
}

# ============================================================
# 3) RIESGO_CATEGORIA
# ============================================================
seed_riesgo_categoria <- function(con){
  if(!tbl_exists(con,"riesgo_categoria")) { message("⚠️ No existe riesgo_categoria"); return(invisible()) }
  ex <- dbGetQuery(con,"SELECT categoria_id, nombre FROM riesgo_categoria")
  base <- c("Técnico","Organizacional","Externo","Legal","Calidad")
  faltan <- setdiff(base, ex$nombre %||% character())
  if(length(faltan)){
    if("categoria_id" %in% names(ex)) {
      nuevos <- next_ids_seq(ex$categoria_id %||% integer(), length(faltan))
      vals <- paste(mapply(function(id, nom)
        sprintf("(%d,%s,%s)", id, dbQuoteString(con, nom), dbQuoteString(con, paste("Categoría", nom))),
        nuevos, faltan), collapse = ", ")
      dbExecute(con, glue("INSERT INTO riesgo_categoria (categoria_id, nombre, descripcion) VALUES {vals}"))
    } else {
      vals <- paste(sprintf("(%s,%s,%s)", "DEFAULT", dbQuoteString(con,faltan),
                            dbQuoteString(con,paste("Categoría",faltan))), collapse=", ")
      dbExecute(con, glue("INSERT INTO riesgo_categoria (categoria_id, nombre, descripcion) VALUES {vals}"))
    }
  }
  message("✅ riesgo_categoria listo")
}

# ============================================================
# 4) RIESGO_ESCALA
# ============================================================
seed_riesgo_escala <- function(con){
  if(!tbl_exists(con,"riesgo_escala")) { message("⚠️ No existe riesgo_escala"); return(invisible()) }
  ex <- dbGetQuery(con,"SELECT escala_id, nombre FROM riesgo_escala")
  base <- tibble::tribble(
    ~nombre,    ~prob_min, ~prob_max, ~imp_min, ~imp_max,
    "Muy Baja", 1, 1, 1, 1,
    "Baja",     2, 2, 2, 2,
    "Media",    3, 3, 3, 3,
    "Alta",     4, 4, 4, 4,
    "Muy Alta", 5, 5, 5, 5
  )
  faltan <- setdiff(base$nombre, ex$nombre %||% character())
  if(length(faltan)){
    df <- base %>% filter(nombre %in% faltan)
    if("escala_id" %in% names(ex)) {
      nuevos <- next_ids_seq(ex$escala_id %||% integer(), nrow(df))
      df$escala_id <- nuevos
      vals <- apply(df, 1, function(r)
        sprintf("(%d,%s,%d,%d,%d,%d)",
                as.integer(r[["escala_id"]]),
                dbQuoteString(con, r[["nombre"]]),
                as.integer(r[["prob_min"]]),
                as.integer(r[["prob_max"]]),
                as.integer(r[["imp_min"]]),
                as.integer(r[["imp_max"]])))
      dbExecute(con, glue("INSERT INTO riesgo_escala (escala_id,nombre,prob_min,prob_max,imp_min,imp_max) VALUES {paste(vals,collapse=', ')}"))
    } else {
      vals <- apply(df, 1, function(r)
        sprintf("(%s,%s,%d,%d,%d,%d)",
                "DEFAULT", dbQuoteString(con, r[["nombre"]]),
                as.integer(r[["prob_min"]]), as.integer(r[["prob_max"]]),
                as.integer(r[["imp_min"]]),  as.integer(r[["imp_max"]])))
      dbExecute(con, glue("INSERT INTO riesgo_escala (escala_id,nombre,prob_min,prob_max,imp_min,imp_max) VALUES {paste(vals,collapse=', ')}"))
    }
  }
  message("✅ riesgo_escala listo")
}

# ============================================================
# 5) RIESGO_EVENTO  (solo si hay riesgos → evento “Identificacion” por riesgo sin ese tipo)
# ============================================================
seed_riesgo_evento <- function(con){
  if(!tbl_exists(con,"riesgo_evento")) { message("⚠️ No existe riesgo_evento"); return(invisible()) }
  
  # Si no hay riesgos, no insertamos nada (riesgo_id es NOT NULL)
  if(!tbl_exists(con,"riesgo")) { message("⚠️ No existe riesgo → omito riesgo_evento"); return(invisible()) }
  r <- dbGetQuery(con, "SELECT riesgo_id FROM riesgo")
  if(!nrow(r)) { message("ℹ️ No hay riesgos → omito seed de riesgo_evento"); return(invisible()) }
  
  # ¿Qué riesgos NO tienen ya evento 'Identificacion'?
  ya <- safe_q(con, "SELECT DISTINCT riesgo_id FROM riesgo_evento WHERE tipo = 'Identificacion'")
  faltan_ids <- setdiff(r$riesgo_id, ya$riesgo_id %||% integer())
  if(length(faltan_ids)==0){ message("✅ riesgo_evento listo (sin faltantes)"); return(invisible()) }
  
  vals <- paste(sapply(faltan_ids, function(id){
    sprintf("(%s,%s,%d,%s,%s)",
            "DEFAULT", "NULL", as.integer(id), "CURRENT_TIMESTAMP", dbQuoteString(con,"Identificacion"))
  }), collapse = ", ")
  dbExecute(con, glue("INSERT INTO riesgo_evento (evento_id, actor_autor_id, riesgo_id, fecha_evento, tipo) VALUES {vals}"))
  
  message("✅ riesgo_evento listo (insertados ", length(faltan_ids), " eventos 'Identificacion')")
}

# ============================================================
# 6) ENTREGABLES base
# ============================================================
seed_entregables <- function(con){
  if(!tbl_exists(con,"entregable")) { message("⚠️ No existe entregable"); return(invisible()) }
  tipos <- dbGetQuery(con,"SELECT tipo_id, nombre FROM entregable_tipo")
  fases <- dbGetQuery(con,"SELECT fase_id, nombre FROM fase")
  
  base_raw <- tibble::tribble(
    ~nombre,                           ~descripcion,                                                                 ~tipo_nombre, ~fase_nombre,
    "Índice PMP validado",             "Índice general y subcapítulos aprobados.",                                   "Documento",  "Fase 1 – Planificación",
    "Cronograma de entregas",          "Cronograma con fechas por capítulos (Excel/Markdown).",                      "Documento",  "Fase 1 – Planificación",
    "Capítulos 1–32 (borrador)",       "Primer borrador de los 32 capítulos.",                                       "Documento",  "Fase 2 – Redacción inicial",
    "Matriz de riesgos v1",            "Identificación inicial de eventos y escalas de riesgo.",                     "Matriz",     "Fase 2 – Redacción inicial",
    "Revisión de pares por capítulo",  "Informe de revisión por capítulo.",                                          "Informe",    "Fase 3 – Revisión",
    "Informe piloto",                  "Evaluación de la matriz piloto.",                                            "Documento",  "Fase 3 – Revisión",
    "Documento maquetado",             "Versión unificada y diagramada del libro.",                                  "Documento",  "Fase 4 – Edición",
    "Informe de estilo y normas APA",  "Observaciones de estilo, citas y referencias bajo APA 7.",                   "Informe",    "Fase 4 – Edición",
    "Versión final del libro",         "Documento final listo para publicación.",                                    "Documento",  "Fase 5 – Cierre",
    "Registro de propiedad intelectual","Certificado SENADI/Zenodo/OSF y resguardo documental del registro.",        "Registro",   "Fase 5 – Cierre"
  )
  
  df <- base_raw %>%
    left_join(tipos, by = c("tipo_nombre"="nombre")) %>% rename(tipo_id = tipo_id) %>%
    left_join(fases, by = c("fase_nombre"="nombre")) %>% rename(fase_id = fase_id) %>%
    select(nombre, descripcion, tipo_id, fase_id)
  
  ya <- dbGetQuery(con,"SELECT nombre FROM entregable")
  pend <- anti_join(df, ya, by="nombre")
  if(nrow(pend)){
    vals <- apply(pend, 1, function(r)
      sprintf("(%s,%s,%s,%s)",
              dbQuoteString(con, r[["nombre"]]),
              dbQuoteString(con, r[["descripcion"]]),
              ifelse(is.na(r[["tipo_id"]]), "NULL", as.integer(r[["tipo_id"]])),
              ifelse(is.na(r[["fase_id"]]), "NULL", as.integer(r[["fase_id"]]))
      ))
    dbExecute(con, glue("INSERT INTO entregable (nombre, descripcion, tipo_id, fase_id) VALUES {paste(vals, collapse=', ')}"))
  }
  message("✅ entregables base listos")
}

# ============================================================
# EJECUCIÓN MAESTRA
# ============================================================
con <- get_con(); on.exit(try(dbDisconnect(con), silent=TRUE), add=TRUE)

seed_entregable_tipo(con)
seed_fase(con)
seed_riesgo_categoria(con)
seed_riesgo_escala(con)
seed_riesgo_evento(con)
seed_entregables(con)

message("🎉 Seed PMP completado.")

# ---------------- Verificación post-seed ----------------
check_tbl <- function(con, tbl, cols_show = NULL, n = 10){
  if(!(tbl %in% dbListTables(con))){
    message("❌ Tabla no existe: ", tbl); return(invisible(NULL))
  }
  cnt <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM `%s`", tbl))$n[[1]]
  message(sprintf("✔ %s: %s filas", tbl, cnt))
  if(cnt > 0){
    if(is.null(cols_show)){
      q <- sprintf("SELECT * FROM `%s` ORDER BY 1 DESC LIMIT %d", tbl, n)
    } else {
      q <- sprintf("SELECT %s FROM `%s` ORDER BY 1 DESC LIMIT %d",
                   paste(sprintf("`%s`", cols_show), collapse=", "), tbl, n)
    }
    print(dbGetQuery(con, q))
  }
  invisible(cnt)
}

message("\n===== VERIFICACIÓN PMP =====")
check_tbl(con, "entregable_tipo", c("tipo_id","nombre"))
check_tbl(con, "fase", c("fase_id","nombre","fecha_inicio","fecha_fin"))
check_tbl(con, "riesgo_categoria", c("categoria_id","nombre","descripcion"))
check_tbl(con, "riesgo_escala", c("escala_id","nombre","prob_min","prob_max","imp_min","imp_max"))
check_tbl(con, "riesgo_evento", c("evento_id","riesgo_id","tipo"))
check_tbl(con, "entregable", c("entregable_id","nombre","tipo_id","fase_id"))
message("===== FIN VERIFICACIÓN =====\n")
