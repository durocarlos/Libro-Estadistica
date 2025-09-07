#install.packages(c("DBI","RMariaDB","readr","dplyr","stringr"))# Solo una vez
library(DBI)
library(RMariaDB)
library(readr)
library(dplyr)
library(stringr)

# 1) Conexión permitiendo multi-statements (para ejecutar dumps largos)
con <- dbConnect(
  MariaDB(),
  host = "localhost", port = 3306,
  user = "root",
  password = "mvSH2014**",
  client.flag = RMariaDB::CLIENT_MULTI_STATEMENTS
)

#cd "C:\Users\Carlos Sarmiento\OneDrive\Documentos\GitHub\Libro-Estadistica\data"

# --- Verificación rápida del esquema "libro" ---

library(DBI)
library(dplyr)

# 1) Tablas disponibles
cat("\n== Tablas en el schema ==\n")
print(DBI::dbListTables(con))

# Helper para correr consultas seguras
q <- function(sql) { tryCatch(DBI::dbGetQuery(con, sql), error = function(e) { message("ERROR: ", e$message); NULL }) }

# 2) Columnas de tablas principales (si existen)
main_tables <- c("autor","afiliacion","autor_afiliacion",
                 "rol","estado_capitulo","estado_subcapitulo","fase",
                 "capitulo","subcapitulo",
                 "capitulo_autor","subcapitulo_autor",
                 "entrega","revision")

cat("\n== Columnas por tabla (si existe) ==\n")
for (t in main_tables) {
  if (t %in% DBI::dbListTables(con)) {
    cat("\n--", t, "--\n")
    print(DBI::dbListFields(con, t))
  }
}

# 3) Conteos rápidos
cat("\n== Conteos clave ==\n")
print(q("SELECT COUNT(*) AS n FROM autor"))
print(q("SELECT COUNT(*) AS n FROM capitulo"))
print(q("SELECT COUNT(*) AS n FROM subcapitulo"))
print(q("SELECT COUNT(*) AS n FROM capitulo_autor"))
print(q("SELECT COUNT(*) AS n FROM subcapitulo_autor"))
print(q("SELECT COUNT(*) AS n FROM fase"))

# 4) Previews (primeras filas)
cat("\n== Preview de tablas ==\n")
print(q("SELECT autor_id, nombre_completo, email FROM autor LIMIT 5"))
print(q("SELECT numero, titulo FROM capitulo ORDER BY numero LIMIT 5"))
print(q("SELECT capitulo_id, numero, titulo FROM subcapitulo ORDER BY capitulo_id, numero LIMIT 5"))
print(q("SELECT * FROM fase ORDER BY fase_id"))

# 5) Chequeos de relaciones
cat("\n== Chequeos de integridad (joins) ==\n")

# subcapítulos por capítulo (debería ser 3 si usaste el script estándar)
print(q("
  SELECT c.numero AS cap, COUNT(sc.subcapitulo_id) AS subcaps
  FROM capitulo c
  LEFT JOIN subcapitulo sc ON sc.capitulo_id = c.capitulo_id
  GROUP BY c.numero
  ORDER BY c.numero
  LIMIT 10
"))

# autores asignados por capítulo (si ya cargaste capitulo_autor)
print(q("
  SELECT c.numero AS cap, COUNT(ca.autor_id) AS autores_asignados
  FROM capitulo c
  LEFT JOIN capitulo_autor ca ON ca.capitulo_id = c.capitulo_id
  GROUP BY c.numero
  ORDER BY c.numero
  LIMIT 10
"))

# 6) Fase “hoy” (usa fecha del servidor MySQL)
cat("\n== Fase activa hoy ==\n")
print(q("
  SELECT fase_id, nombre, fecha_inicio, fecha_fin
  FROM fase
  WHERE CURDATE() BETWEEN fecha_inicio AND fecha_fin
"))

# 7) Vistas (si existen) y preview
cat("\n== Vistas ==\n")
vistas <- c("vw_portada_capitulo","vw_autores_subcapitulo","vw_avance_capitulo")
print(intersect(vistas, DBI::dbListTables(con)))

if ("vw_portada_capitulo" %in% DBI::dbListTables(con)) {
  cat("\nPreview vw_portada_capitulo:\n")
  print(q("SELECT * FROM vw_portada_capitulo ORDER BY cap_numero, COALESCE(orden,9999), rol LIMIT 10"))
}
if ("vw_autores_subcapitulo" %in% DBI::dbListTables(con)) {
  cat("\nPreview vw_autores_subcapitulo:\n")
  print(q("SELECT * FROM vw_autores_subcapitulo ORDER BY cap_numero, sub_numero, COALESCE(orden,9999) LIMIT 10"))
}
if ("vw_avance_capitulo" %in% DBI::dbListTables(con)) {
  cat("\nPreview vw_avance_capitulo:\n")
  print(q("SELECT * FROM vw_avance_capitulo ORDER BY numero LIMIT 10"))
}

cat("\n== Verificación completada ==\n")
