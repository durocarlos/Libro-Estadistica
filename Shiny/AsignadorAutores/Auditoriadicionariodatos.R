# ============================================
# Auditoría del Diccionario de Datos (MySQL)
# Proyecto: Libro de Estadística (PMP)
# ============================================

suppressPackageStartupMessages({
  library(DBI); library(RMariaDB); library(dplyr); library(glue); library(tidyr); library(purrr)
})

`%||%` <- function(x,y) if(is.null(x) || length(x)==0) y else x

get_con <- function(){
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("MYSQL_HOST", "localhost"),
    port = as.integer(Sys.getenv("MYSQL_PORT", "3306")),
    user = Sys.getenv("MYSQL_USER", "root"),
    password = Sys.getenv("MYSQL_PASSWORD", ""),
    dbname = Sys.getenv("MYSQL_DBNAME", "libro"),
    bigint = "integer"
  )
}

# Fallback correcto para schema
schema <- Sys.getenv("MYSQL_DBNAME"); if (schema == "") schema <- "libro"

con <- get_con()
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

# ---------------- Descubrimiento de tablas PMP ----------------
all_tables   <- DBI::dbListTables(con)
pmp_patterns <- c("^entregable", "^fase$", "^riesgo")
pmp_tables   <- all_tables[Reduce(`|`, lapply(pmp_patterns, function(p) grepl(p, all_tables)))]
message("Tablas detectadas PMP: ", paste(pmp_tables, collapse=", "))

# ---------------- Diccionario de columnas ----------------
cols_sql <- "
SELECT TABLE_NAME, COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, COLUMN_DEFAULT, EXTRA, COLUMN_KEY, ORDINAL_POSITION
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = ?
ORDER BY TABLE_NAME, ORDINAL_POSITION;
"
cols <- DBI::dbGetQuery(con, DBI::sqlInterpolate(con, cols_sql, schema))

# ---------------- Llaves y relaciones ----------------
kcu_sql <- "
SELECT
  kcu.TABLE_NAME,
  kcu.COLUMN_NAME,
  kcu.REFERENCED_TABLE_NAME AS REF_TABLE,
  kcu.REFERENCED_COLUMN_NAME AS REF_COLUMN,
  tc.CONSTRAINT_TYPE
FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu
JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc
  ON tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME
 AND tc.TABLE_SCHEMA = kcu.TABLE_SCHEMA
 AND tc.TABLE_NAME   = kcu.TABLE_NAME
WHERE kcu.TABLE_SCHEMA = ?
ORDER BY kcu.TABLE_NAME, kcu.COLUMN_NAME;
"
keys <- DBI::dbGetQuery(con, DBI::sqlInterpolate(con, kcu_sql, schema))

# ---------------- Conteo de registros ----------------
count_tbl <- function(tbl){
  tryCatch(DBI::dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM `{tbl}`"))$n[[1]], error=function(e) NA_integer_)
}
counts <- tibble(TABLE_NAME = pmp_tables, rows = purrr::map_int(pmp_tables, count_tbl))

# ---------------- Filtrar solo PMP ----------------
cols_pmp <- cols %>% filter(TABLE_NAME %in% pmp_tables)
keys_pmp <- keys %>% filter(TABLE_NAME %in% pmp_tables)

# ---------------- Diccionario compacto ----------------
dicc_por_tabla <- cols_pmp %>%
  mutate(
    PK   = ifelse(COLUMN_KEY == "PRI", "Sí", ""),
    NULO = ifelse(IS_NULLABLE == "YES", "Sí", "No")
  ) %>%
  select(TABLE_NAME, COLUMN_NAME, COLUMN_TYPE, NULO, COLUMN_DEFAULT, EXTRA, PK)

# ---------------- Relaciones legibles ----------------
relaciones <- keys_pmp %>%
  filter(CONSTRAINT_TYPE %in% c("PRIMARY KEY","FOREIGN KEY","UNIQUE")) %>%
  mutate(
    REL = case_when(
      CONSTRAINT_TYPE == "PRIMARY KEY" ~ "PK",
      CONSTRAINT_TYPE == "FOREIGN KEY" ~ "FK",
      CONSTRAINT_TYPE == "UNIQUE"      ~ "UQ",
      TRUE ~ CONSTRAINT_TYPE
    )
  ) %>%
  select(TABLE_NAME, COLUMN_NAME, REL, REF_TABLE, REF_COLUMN)

# ---------------- Huérfanos de FK (solo tablas PMP) ----------------
fk_rows <- keys_pmp %>%
  filter(CONSTRAINT_TYPE == "FOREIGN KEY",
         !is.na(REF_TABLE), !is.na(REF_COLUMN),
         TABLE_NAME %in% pmp_tables, REF_TABLE %in% pmp_tables)

fk_orphans <- list()
if (nrow(fk_rows)) {
  fk_orphans <- pmap(fk_rows, function(TABLE_NAME, COLUMN_NAME, REF_TABLE, REF_COLUMN, CONSTRAINT_TYPE){
    sql <- glue(
      "SELECT COUNT(*) AS orfanos
         FROM `{TABLE_NAME}` t
    LEFT JOIN `{REF_TABLE}` r
           ON t.`{COLUMN_NAME}` = r.`{REF_COLUMN}`
        WHERE t.`{COLUMN_NAME}` IS NOT NULL
          AND r.`{REF_COLUMN}` IS NULL"
    )
    n <- tryCatch(DBI::dbGetQuery(con, sql)$orfanos[[1]], error=function(e) NA_integer_)
    tibble(TABLE_NAME, COLUMN_NAME, REF_TABLE, REF_COLUMN, ORPHANS = n)
  }) %>% bind_rows()
} else {
  fk_orphans <- tibble(TABLE_NAME=character(), COLUMN_NAME=character(),
                       REF_TABLE=character(), REF_COLUMN=character(), ORPHANS=integer())
}

# ---------------- Duplicados en catálogos (por nombre) ----------------
tbl_exists <- function(tbl) tbl %in% all_tables
safe_q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error=function(e) tibble())

check_unique_by_nombre <- function(tbl, nombre_col="nombre"){
  if(!tbl_exists(tbl)) return(NULL)
  cols <- DBI::dbListFields(con, tbl)
  if(!(nombre_col %in% cols)) return(NULL)
  sql <- glue("SELECT `{nombre_col}` AS nombre, COUNT(*) AS n FROM `{tbl}` GROUP BY `{nombre_col}` HAVING COUNT(*)>1")
  res <- safe_q(sql)
  if(nrow(res)) mutate(res, tabla = tbl) else NULL
}

duplicates_in_catalogs <- compact(list(
  check_unique_by_nombre("entregable_tipo","nombre"),
  check_unique_by_nombre("fase","nombre"),
  check_unique_by_nombre("riesgo_categoria","nombre"),
  check_unique_by_nombre("riesgo_escala","nombre"),
  check_unique_by_nombre("riesgo_evento","nombre")
))
duplicates_in_catalogs <- if(length(duplicates_in_catalogs)) bind_rows(duplicates_in_catalogs) else tibble()

# ---------------- Cobertura: entregables por fase y por tipo ----------------
entregables_por_fase <- tibble()
entregables_por_tipo <- tibble()

if (tbl_exists("entregable")) {
  ent_cols <- DBI::dbListFields(con, "entregable")
  fase_fk  <- first(intersect(c("fase_id","id_fase","fase"), ent_cols))
  tipo_fk  <- first(intersect(c("entregable_tipo_id","tipo_id","id_tipo","tipo"), ent_cols))
  
  if (tbl_exists("fase") && !is.na(fase_fk)) {
    fase_id_col <- first(intersect(c("fase_id","id","id_fase"), DBI::dbListFields(con,"fase")))
    entregables_por_fase <- safe_q(glue("
      SELECT f.nombre AS fase, COUNT(*) AS entregables
        FROM entregable e
        JOIN fase f ON e.`{fase_fk}` = f.`{fase_id_col}`
    GROUP BY f.nombre
    ORDER BY entregables DESC"))
  }
  if (tbl_exists("entregable_tipo") && !is.na(tipo_fk)) {
    tipo_id_col <- first(intersect(c("entregable_tipo_id","tipo_id","id"), DBI::dbListFields(con,"entregable_tipo")))
    entregables_por_tipo <- safe_q(glue("
      SELECT t.nombre AS tipo, COUNT(*) AS entregables
        FROM entregable e
        JOIN entregable_tipo t ON e.`{tipo_fk}` = t.`{tipo_id_col}`
    GROUP BY t.nombre
    ORDER BY entregables DESC"))
  }
}

# ---------------- Tablas vacías ----------------
empty_tables <- counts %>% filter(is.na(rows) | rows == 0) %>% pull(TABLE_NAME)

# ---------------- Impresión legible ----------------
cat("\n================= AUDITORÍA PMP =================\n")
cat("Esquema:", schema, "\n")
cat("Tablas PMP detectadas (", length(pmp_tables), "): ", paste(pmp_tables, collapse=", "), "\n", sep="")

cat("\n--- Conteo de registros ---\n")
print(counts %>% arrange(desc(rows)))

if(length(empty_tables)){
  cat("\n⚠️  Tablas vacías:", paste(empty_tables, collapse=", "), "\n")
} else {
  cat("\n✅ No hay tablas vacías dentro del conjunto PMP.\n")
}

cat("\n--- Duplicados en catálogos (por nombre) ---\n")
if(nrow(duplicates_in_catalogs)){
  print(duplicates_in_catalogs %>% arrange(tabla, desc(n)))
} else {
  cat("✅ Sin duplicados detectados en catálogos revisados.\n")
}

cat("\n--- Chequeo de orfandad en FKs (solo entre tablas PMP) ---\n")
if(nrow(fk_orphans)){
  print(arrange(fk_orphans, desc(ORPHANS)))
} else {
  cat("✅ No se detectaron FKs entre tablas PMP o no hay orfandad.\n")
}

if(nrow(entregables_por_fase)){
  cat("\n--- Entregables por Fase ---\n"); print(entregables_por_fase)
}
if(nrow(entregables_por_tipo)){
  cat("\n--- Entregables por Tipo ---\n"); print(entregables_por_tipo)
}

cat("\n--- Diccionario (vista compacta) ---\n")
print(arrange(dicc_por_tabla, TABLE_NAME, COLUMN_NAME))
cat("\n================= FIN AUDITORÍA =================\n")

# ---------------- Utilidad: inspección puntual ----------------
ver_tabla <- function(tabla){
  stopifnot(tabla %in% all_tables)
  dic <- dicc_por_tabla %>% filter(TABLE_NAME == tabla)
  rel <- relaciones     %>% filter(TABLE_NAME == tabla)
  n   <- counts         %>% filter(TABLE_NAME == tabla)
  cat("\n-------------------------------\n")
  cat("TABLA:", tabla, "\n")
  cat("Registros:", n$rows %||% NA, "\n")
  cat("\n[Columnas]\n"); print(dic)
  cat("\n[Relaciones]\n"); print(rel)
  invisible(list(columnas = dic, relaciones = rel, registros = n$rows))
}

# Ejemplos (descomenta si quieres):
# invisible(ver_tabla("entregable"))
# invisible(ver_tabla("fase"))
# invisible(ver_tabla("entregable_nota"))
# invisible(ver_tabla("entregable_revision"))
# invisible(ver_tabla("riesgo_matriz"))

