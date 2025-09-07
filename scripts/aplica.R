# app.R — Shiny: Asignador de Autores por Capítulo/Subcapítulo (MySQL)
# Requisitos: install.packages(c("shiny","DBI","RMariaDB","dplyr","DT","shinyWidgets"))
# Opcional: guardar credenciales en variables de entorno (recomendado)
# Sys.setenv(MYSQL_HOST="localhost", MYSQL_PORT="3306", MYSQL_USER="root", MYSQL_PASSWORD="TU_PASS", MYSQL_DBNAME="libro")
rm(list = ls())
# ======================================================
# aplica.R — Launcher Shiny con validación MySQL (UTF-8)
# ======================================================

options(encoding = "UTF-8")

# --- 1) Credenciales MySQL ---
# (puedes moverlas a ~/.Renviron; si lo haces, elimina este bloque Sys.setenv)
Sys.setenv(
  MYSQL_HOST = Sys.getenv("MYSQL_HOST", "localhost"),
  MYSQL_PORT = Sys.getenv("MYSQL_PORT", "3306"),
  MYSQL_USER = Sys.getenv("MYSQL_USER", "root"),
  MYSQL_PASSWORD = Sys.getenv("MYSQL_PASSWORD", "mvSH2014**"),
  MYSQL_DBNAME = Sys.getenv("MYSQL_DBNAME", "libro")
)

# --- 2) Paquetes requeridos ---
req_pkgs <- c("shiny", "DBI", "RMariaDB")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req_pkgs, require, character.only = TRUE))

# --- 3) Validación de credenciales y conexión ---
get_env <- function(k) { v <- Sys.getenv(k, ""); if (nzchar(v)) v else NA_character_ }

cfg <- list(
  host = get_env("MYSQL_HOST"),
  port = as.integer(get_env("MYSQL_PORT")),
  user = get_env("MYSQL_USER"),
  pass = get_env("MYSQL_PASSWORD"),
  dbnm = get_env("MYSQL_DBNAME")
)

# Chequeo rápido de vacíos
faltan <- names(cfg)[sapply(cfg, function(x) is.na(x) || (is.character(x) && !nzchar(x)))]
if (length(faltan)) {
  stop("⚠️ Variables de entorno faltantes: ", paste(faltan, collapse = ", "),
       "\nDefine las credenciales con Sys.setenv(...) o en ~/.Renviron y vuelve a ejecutar.")
}

cat("🔎 Probando conexión MySQL a ", cfg$host, ":", cfg$port, " BD=", cfg$dbnm, " ...\n", sep = "")

con <- NULL
ok <- FALSE
try({
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = cfg$host, port = cfg$port,
    user = cfg$user, password = cfg$pass, dbname = cfg$dbnm
  )
  ok <- DBI::dbIsValid(con)
}, silent = TRUE)

if (!ok) {
  stop(
    "❌ No se pudo conectar a MySQL.\n",
    "- Verifica usuario/contraseña y que el servidor esté activo.\n",
    "- Probá desde consola: mysql -h ", cfg$host, " -P ", cfg$port,
    " -u ", cfg$user, " -p\n",
    "- Revisa ~/.Renviron o el bloque Sys.setenv del script."
  )
}

# Ping y DB activa
db_activa <- tryCatch(DBI::dbGetQuery(con, "SELECT DATABASE() AS db"), error = function(e) NULL)
if (is.null(db_activa) || is.na(db_activa$db[1])) {
  DBI::dbDisconnect(con)
  stop("❌ Conexión abierta pero la base '", cfg$dbnm, "' no está seleccionada o no existe.")
}
cat("✅ Conectado. Base activa: ", db_activa$db[1], "\n", sep = "")

# --- 4) Validación de tablas clave ---
tablas_req <- c("autor","capitulo","subcapitulo","rol","capitulo_autor","subcapitulo_autor")
tablas_tienen <- tryCatch(DBI::dbListTables(con), error = function(e) character(0))
faltan_tablas <- setdiff(tablas_req, tablas_tienen)

if (length(faltan_tablas)) {
  DBI::dbDisconnect(con)
  stop("❌ Faltan tablas requeridas: ", paste(faltan_tablas, collapse = ", "),
       "\nAsegúrate de haber importado el esquema (Dump SQL) correctamente.")
}

# (Opcional) Mensaje de resumen
n_autores <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autor")$n[1]
n_caps    <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM capitulo")$n[1]
cat("📊 Resumen: autores=", n_autores, " | capítulos=", n_caps, "\n", sep = "")

DBI::dbDisconnect(con)

# --- 5) Lanzar Shiny ---
repo_root <- "C:/Users/Carlos Sarmiento/OneDrive/Documentos/GitHub/Libro-Estadistica/scripts"
app_path  <- file.path(repo_root, "app.R")

if (!dir.exists(repo_root)) stop("No existe la carpeta del repositorio: ", repo_root)
if (!file.exists(app_path)) stop("No se encontró app.R en: ", app_path)

setwd(repo_root)
cat("🚀 Lanzando Shiny: ", app_path, "\n", sep = "")
shiny::runApp(appPath = app_path, launch.browser = TRUE)
