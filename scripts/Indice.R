#"C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe" -u root -p -e "SHOW DATABASES;"

## =========================================================
## 0) Paquetes
## =========================================================
pkgs <- c("DBI","RMariaDB","readr","readxl","dplyr","stringr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

## =========================================================
## 1) Conexión a MySQL
##    (usa "libro_copia" si quieres cargar en la copia)
## =========================================================
con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost", port = 3306,
  user = "root",
  password = "mvSH2014**",     # <-- CAMBIA ESTO
  dbname = "libro"         # o "libro"
)
dbGetQuery(con, "SELECT DATABASE();")

## Carpeta de trabajo
base_dir <- "C:/Users/Carlos Sarmiento/OneDrive/Documentos/GitHub/Libro-Estadistica/data"

# Verificar
dbGetQuery(con, "SELECT DATABASE();")
dbGetQuery(con, "SHOW TABLES;")

# Ejemplo: traer capítulos
capitulos <- dbGetQuery(con, "SELECT * FROM capitulo;")
head(capitulos)
subcapitulos <- dbGetQuery(con, "SELECT * FROM subcapitulo;")
head(subcapitulos)
dbGetQuery(con, "SELECT COUNT(*) AS capitulos FROM capitulo;")
dbGetQuery(con, "SELECT COUNT(*) AS subcapitulos FROM subcapitulo;")
dbGetQuery(con, "SELECT COUNT(*) AS autores FROM autor;")
dbGetQuery(con, "SELECT * FROM rol ORDER BY rol_id;")
dbGetQuery(con, "SELECT * FROM fase ORDER BY fase_id;")

## =========================================================
## 2) Listar todas las tablas y ver contenido
## =========================================================

# Obtener nombres de todas las tablas
tablas <- dbGetQuery(con, "SHOW TABLES;")[,1]

# Mostrar nombres de tablas
print(tablas)

# Traer el contenido de cada tabla
todas_las_tablas <- lapply(tablas, function(t) {
  cat("\n========================================\n")
  cat("Tabla:", t, "\n")
  cat("========================================\n")
  
  datos <- dbGetQuery(con, paste0("SELECT * FROM ", t, " LIMIT 50;")) # ver primeros 50
  print(datos)
  return(datos)
})

names(todas_las_tablas) <- tablas



