# ============================================================
# preflight_check.R  —  Verificador total (FS + Git + DB + PDFs)
# ============================================================

suppressPackageStartupMessages({
  library(DBI); library(RMariaDB)
  library(dplyr); library(tibble); library(purrr)
  library(glue); library(fs); library(stringr); library(crayon)
})

# ---------------------------
# 1) Configuración de entorno
# ---------------------------

# Ruta base del repo (ajústala si fuera necesario)
repo_dir <- "C:/Users/Carlos Sarmiento/OneDrive/Documentos/GitHub/Libro-Estadistica"

# Subcarpetas relevantes
docs_dir      <- file.path(repo_dir, "docs")
shiny_dir     <- file.path(repo_dir, "Shiny")
preflight_dir <- file.path(repo_dir, "preflight")
dir_create(preflight_dir)

# Credenciales MySQL: toma de variables de entorno (o defaults)
MYSQL_HOST <- Sys.getenv("MYSQL_HOST", "localhost")
MYSQL_PORT <- as.integer(Sys.getenv("MYSQL_PORT", "3306"))
MYSQL_USER <- Sys.getenv("MYSQL_USER", "root")
MYSQL_PASSWORD <- Sys.getenv("MYSQL_PASSWORD", "")
MYSQL_DBNAME <- Sys.getenv("MYSQL_DBNAME", "libro")

# Tablas mínimas requeridas
tables_required <- c(
  "autor", "rol",
  "capitulo", "capitulo_autor",
  "subcapitulo", "subcapitulo_autor",
  "fase",
  "riesgo", "riesgo_accion", "riesgo_evento", "riesgo_nota",
  "riesgo_matriz", "riesgo_escala"
)

# >>> TRIGGERS: usa los nombres REALES que creaste <<<
triggers_required <- c(
  "trg_riesgo_bi_ranges",   # BEFORE INSERT — validación de rangos
  "trg_riesgo_bu_ranges",   # BEFORE UPDATE — validación de rangos
  "trg_riesgo_au_estado"    # AFTER UPDATE  — bitácora cambio de estado
)

# Documentos PDF a vigilar (última versión _VerN.pdf)
pdf_stems <- c(
  "Diccionario_BD_Libro_PMP",
  "ER_PMP_matriz_notas"
)

# Helper para construir filas de resultado
row <- function(etapa, item, nivel = c("OK","WARN","ERROR"), detalle = "") {
  nivel <- match.arg(nivel)
  tibble(Etapa = etapa, Item = item, Nivel = nivel, Detalle = detalle)
}

results <- tibble(Etapa=character(), Item=character(), Nivel=character(), Detalle=character())

cat(blue$bold("\n=======================================\n"))
cat(blue$bold(" PRE-FLIGHT — Inicio de verificación\n"))
cat(blue$bold("=======================================\n\n"))

# --------------------------------
# 2) Verificación Git (repositorio)
# --------------------------------

git_ok <- FALSE
git_detail <- NULL

# 1) Señal mínima: existe carpeta .git
has_dotgit <- fs::dir_exists(file.path(repo_dir, ".git"))

# 2) Prueba con git CLI usando -C "repo_dir"
inside <- tryCatch(
  system2("git",
          c("-C", shQuote(repo_dir), "rev-parse", "--is-inside-work-tree"),
          stdout = TRUE, stderr = TRUE),
  error = function(e) character()
)

if (length(inside) && tolower(trimws(inside[1])) == "true") {
  # OK: estamos dentro de un repo
  results <- bind_rows(results, row("GIT", "Repositorio", "OK", "Dentro de un repositorio Git"))
  
  # ¿Working tree limpio?
  status <- tryCatch(
    system2("git", c("-C", shQuote(repo_dir), "status", "--porcelain"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  if (length(status) == 0) {
    results <- bind_rows(results, row("GIT", "Working tree", "OK", "Sin cambios pendientes"))
    git_ok <- TRUE
  } else {
    results <- bind_rows(
      results,
      row("GIT", "Working tree", "WARN",
          "Hay cambios sin commitear (git status --porcelain != vacío)")
    )
  }
  
  # Último commit (opcional)
  last_commit <- tryCatch(
    system2("git",
            c("-C", shQuote(repo_dir), "log", "-1", "--pretty=format:%h | %ci | %an | %s"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  if (length(last_commit) > 0) {
    results <- bind_rows(results, row("GIT", "Último commit", "OK", last_commit[1]))
  }
  
} else {
  # No pudimos confirmar con git CLI: degradamos a WARN, no ERROR
  if (has_dotgit) {
    git_detail <- "Se detectó carpeta .git pero no se pudo confirmar con el CLI de git (¿git no está en PATH?)."
  } else {
    git_detail <- "No parece estar dentro de un repo Git (no se encontró .git y/o git CLI no respondió)."
  }
  results <- bind_rows(results, row("GIT", "Repositorio", "WARN", git_detail))
}

# ----------------------
# 3) Verificación MySQL
# ----------------------
con <- NULL
db_ok <- FALSE
try({
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = MYSQL_HOST, port = MYSQL_PORT, user = MYSQL_USER,
    password = MYSQL_PASSWORD, dbname = MYSQL_DBNAME, bigint = "integer"
  )
  results <- bind_rows(results, row("DB", "Conexión", "OK",
                                    glue("Conectado a {MYSQL_DBNAME}@{MYSQL_HOST}:{MYSQL_PORT}")))
  db_ok <- TRUE
}, silent = TRUE)

if (!db_ok) {
  results <- bind_rows(results, row("DB", "Conexión", "ERROR", "No se pudo conectar"))
}

# Tablas
if (db_ok) {
  have <- tryCatch(dbListTables(con), error = function(e) character())
  for (tb in tables_required) {
    if (tb %in% have) {
      results <- bind_rows(results, row("DB", paste("Tabla:", tb), "OK"))
    } else {
      results <- bind_rows(results, row("DB", paste("Tabla:", tb), "ERROR", "No existe"))
    }
  }
  # Triggers
  trig <- tryCatch(DBI::dbGetQuery(con, "SHOW TRIGGERS"), error = function(e) NULL)
  if (!is.null(trig) && nrow(trig) > 0) {
    have_trig <- trig$Trigger
    for (tg in triggers_required) {
      if (tg %in% have_trig) {
        results <- bind_rows(results, row("DB", paste("Trigger:", tg), "OK"))
      } else if (any(startsWith(have_trig, tg))) {
        results <- bind_rows(results, row("DB", paste("Trigger:", tg), "WARN",
                                          "No exacto pero hay variantes presentes"))
      } else {
        results <- bind_rows(results, row("DB", paste("Trigger:", tg), "WARN",
                                          "No encontrado (revisar si el nombre varía)"))
      }
    }
  } else {
    results <- bind_rows(results, row("DB", "Triggers", "WARN",
                                      "No se pudo listar triggers (SHOW TRIGGERS)"))
  }
}

# Cierra conexión si abrió
if (!is.null(con)) try(DBI::dbDisconnect(con), silent = TRUE)

# --------------------------------------------
# 4) PDFs: Detectar la última versión (_VerN)
# --------------------------------------------

# --------------------------------------------
# 4) PDFs: Verificación por nombre exacto (_VerN)
# --------------------------------------------

# Cambia aquí la versión cuando toque (6, 7, …)
VER <- 5

# Construimos los nombres exactos que deben existir en /docs
pdf_targets <- c(
  glue("Diccionario_BD_Libro_PMP_Ver{VER}.pdf"),
  glue("ER_PMP_matriz_notas_Ver{VER}.pdf")
)

for (fname in pdf_targets) {
  fpath <- file.path(docs_dir, fname)
  if (fs::file_exists(fpath)) {
    results <- bind_rows(results, row("PDF", tools::file_path_sans_ext(fname), "OK",
                                      glue("{fname}")))
  } else {
    results <- bind_rows(results, row("PDF", tools::file_path_sans_ext(fname), "ERROR",
                                      glue("No se encontró el archivo exacto: {fname}")))
  }
}

# -------------------------------------------------
# 5) Resumen y semáforo por sección + estado final
# -------------------------------------------------

cat(blue$bold("\n=========================\n"))
cat(blue$bold(" Resumen por sección\n"))
cat(blue$bold("=========================\n"))

summary_by <- results %>%
  mutate(Nivel = factor(Nivel, levels = c("ERROR","WARN","OK"))) %>%
  count(Etapa, Nivel) %>%
  tidyr::pivot_wider(names_from = Nivel, values_from = n, values_fill = 0) %>%
  arrange(Etapa)

print(summary_by, n = nrow(summary_by))

# Estado global
global_estado <- if (any(results$Nivel == "ERROR")) {
  "ERROR"
} else if (any(results$Nivel == "WARN")) {
  "WARN"
} else {
  "OK"
}

cat(blue$bold("\n=========================\n"))
cat(blue$bold(" Estado global\n"))
cat(blue$bold("=========================\n"))

if (global_estado == "OK") {
  cat(green$bold("\n✅ PRE-FLIGHT COMPLETO — Todo verificado correctamente.\n\n"))
} else if (global_estado == "WARN") {
  cat(yellow$bold("\n🟡 PRE-FLIGHT con advertencias. Revisa el detalle mostrado arriba.\n\n"))
} else {
  cat(red$bold("\n❌ PRE-FLIGHT con errores. Revisa el detalle mostrado arriba.\n\n"))
}

# -------------------------------------------------
# 6) Guardar CSV/HTML del reporte (con BOM para acentos)
# -------------------------------------------------
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
csv_path  <- file.path(preflight_dir, glue("preflight_{ts}.csv"))
html_path <- file.path(preflight_dir, glue("preflight_{ts}.html"))

# CSV con BOM (soluciona "ConexiÃ³n" en Windows)
try({
  readr::write_csv(results, csv_path, na = "", bom = TRUE)
}, silent = TRUE)

# HTML simple
try({
  tbl <- results %>%
    mutate(color = dplyr::case_when(
      Nivel == "OK"    ~ "#2e7d32",
      Nivel == "WARN"  ~ "#f9a825",
      Nivel == "ERROR" ~ "#c62828",
      TRUE ~ "#444444"
    )) %>%
    mutate(Nivel = sprintf("<b style='color:%s'>%s</b>", color, Nivel)) %>%
    select(-color)
  
  html <- paste0(
    "<html><head><meta charset='UTF-8'><title>Preflight</title>",
    "<style>body{font-family:Arial; font-size:14px} table{border-collapse:collapse} ",
    "th,td{border:1px solid #ddd; padding:6px 8px} th{background:#f3f3f3}</style></head><body>",
    "<h2>Preflight — ", ts, "</h2>",
    "<p><b>Estado global:</b> ",
    if (global_estado=="OK") "<span style='color:#2e7d32'>✅ OK</span>" else
      if (global_estado=="WARN") "<span style='color:#f9a825'>🟡 WARN</span>" else
        "<span style='color:#c62828'>❌ ERROR</span>",
    "</p>",
    "<table><tr><th>Etapa</th><th>Item</th><th>Nivel</th><th>Detalle</th></tr>",
    paste0(apply(tbl, 1, function(r) sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                                             r[["Etapa"]], r[["Item"]], r[["Nivel"]], r[["Detalle"]])), collapse = "\n"),
    "</table></body></html>"
  )
  writeLines(html, html_path)
}, silent = TRUE)

cat(blue$bold("Archivos de reporte:\n"))
cat(glue(" - {csv_path}\n - {html_path}\n"))




