# install.packages(c("DBI","RMariaDB","dplyr","tidyr","readr","ggplot2","scales"))

library(DBI)
library(RMariaDB)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# --------------------------
# 1) Parámetros
# --------------------------
dbname   <- "libro"  # o "libro_copia"
user     <- "root"
password <- "mvSH2014**"     # <— cambia aquí
host     <- "localhost"
port     <- 3306

# Carpeta de salida
base_dir <- "C:/Users/Carlos Sarmiento/OneDrive/Documentos/GitHub/Libro-Estadistica/data"
out_dir  <- file.path(base_dir, "reports")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --------------------------
# 2) Conexión
# --------------------------
con <- dbConnect(MariaDB(), host=host, port=port, user=user, password=password, dbname=dbname)

# Helper seguro
q <- function(sql) tryCatch(dbGetQuery(con, sql), error=function(e){message("ERROR SQL: ", e$message); tibble()})

# --------------------------
# 3) Tablas base (dataframes)
# --------------------------
capitulos <- q("SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero;")

# Si existen vistas, úsalas; si no, construimos las consultas
tiene_vw_portada <- "vw_portada_capitulo" %in% dbListTables(con)
tiene_vw_avance  <- "vw_avance_capitulo"  %in% dbListTables(con)

vw_portada <- if (tiene_vw_portada) {
  q("SELECT * FROM vw_portada_capitulo ORDER BY cap_numero, COALESCE(orden,9999), rol;")
} else {
  q("
    SELECT c.numero AS cap_numero, c.titulo AS cap_titulo,
           a.nombre_completo, r.nombre AS rol, ca.orden
    FROM capitulo c
    JOIN capitulo_autor ca ON ca.capitulo_id = c.capitulo_id
    JOIN autor a ON a.autor_id = ca.autor_id
    JOIN rol   r ON r.rol_id   = ca.rol_id
    ORDER BY c.numero, COALESCE(ca.orden, 9999), r.nombre;
  ")
}

vw_avance <- if (tiene_vw_avance) {
  q("SELECT * FROM vw_avance_capitulo ORDER BY numero;")
} else {
  q("
    SELECT c.capitulo_id, c.numero, c.titulo,
           COUNT(sc.subcapitulo_id) AS sub_total,
           SUM(sc.estado_id=5) AS sub_maquetados,
           SUM(sc.estado_id=4) AS sub_aprobados
    FROM capitulo c
    LEFT JOIN subcapitulo sc ON sc.capitulo_id = c.capitulo_id
    GROUP BY c.capitulo_id, c.numero, c.titulo
    ORDER BY c.numero;
  ")
}

fases <- q("SELECT fase_id, nombre, fecha_inicio, fecha_fin FROM fase ORDER BY fase_id;")

# Autores por subcapítulo (si existe tabla)
aut_sub <- q("
  SELECT c.numero AS cap_numero, sc.numero AS sub_numero,
         c.titulo AS cap_titulo, sc.titulo AS sub_titulo,
         a.nombre_completo, r.nombre AS rol, sca.orden
  FROM subcapitulo sc
  JOIN capitulo c ON c.capitulo_id = sc.capitulo_id
  LEFT JOIN subcapitulo_autor sca ON sca.subcapitulo_id = sc.subcapitulo_id
  LEFT JOIN autor a ON a.autor_id = sca.autor_id
  LEFT JOIN rol r ON r.rol_id = sca.rol_id
  ORDER BY c.numero, sc.numero, COALESCE(sca.orden,9999);
")

# --------------------------
# 4) Reportes tabulares
# --------------------------
# 4.1 Autores por capítulo (conteo)
autores_por_cap <- vw_portada %>%
  group_by(cap_numero, cap_titulo) %>%
  summarise(autores = n_distinct(nombre_completo), .groups="drop") %>%
  arrange(cap_numero)

# 4.2 Breakdown por rol
autores_por_cap_rol <- vw_portada %>%
  group_by(cap_numero, cap_titulo, rol) %>%
  summarise(autores = n_distinct(nombre_completo), .groups="drop") %>%
  arrange(cap_numero, rol)

# 4.3 Avance por capítulo (si la lógica de estados se usa)
avance_cap <- vw_avance %>%
  mutate(pct_aprob  = ifelse(sub_total>0, sub_aprobados/sub_total, 0),
         pct_maquet = ifelse(sub_total>0, sub_maquetados/sub_total, 0)) %>%
  arrange(numero)

# 4.4 Autores por subcapítulo (top 10 muestra)
autores_sub_preview <- aut_sub %>%
  filter(!is.na(nombre_completo)) %>%
  head(30)

# --------------------------
# 5) Gráficos
# --------------------------
# 5.1 Barras: autores por capítulo
p1 <- ggplot(autores_por_cap, aes(x = factor(cap_numero), y = autores)) +
  geom_col() +
  labs(title = "Autores por capítulo", x = "Capítulo", y = "N° autores") +
  theme_minimal()
p1
ggsave(filename = file.path(out_dir, "autores_por_capitulo.png"),
       plot = p1, width = 9, height = 5, dpi = 150)

# 5.2 Gantt simple de fases
if (nrow(fases) > 0) {
  fases_g <- fases %>%
    mutate(nombre = factor(nombre, levels = rev(nombre)))
  p2 <- ggplot(fases_g, aes(y = nombre)) +
    geom_segment(aes(x = as.Date(fecha_inicio), xend = as.Date(fecha_fin),
                     yend = nombre), linewidth = 3) +
    scale_x_date(date_breaks = "1 month", labels = label_date("%Y-%m")) +
    labs(title = "Cronograma de fases", x = "Fecha", y = NULL) +
    theme_minimal()
  ggsave(filename = file.path(out_dir, "fases_gantt.png"),
         plot = p2, width = 10, height = 4.5, dpi = 150)
}
p2
# --------------------------
# 6) Exportar CSV
# --------------------------
write_csv(autores_por_cap,      file.path(out_dir, "autores_por_capitulo.csv"))
write_csv(autores_por_cap_rol,  file.path(out_dir, "autores_por_capitulo_por_rol.csv"))
write_csv(avance_cap,           file.path(out_dir, "avance_por_capitulo.csv"))
write_csv(autores_sub_preview,  file.path(out_dir, "autores_por_subcapitulo_preview.csv"))
write_csv(fases,                file.path(out_dir, "fases.csv"))

# --------------------------
# 7) Resumen por consola
# --------------------------
cat("\n==== RESUMEN ====\n")
cat("Tablas:", paste(dbListTables(con), collapse=", "), "\n")
cat("Capítulos:", nrow(capitulos), " | Fases:", nrow(fases), "\n")
cat("Autores por capítulo (primeros 40):\n")
print(head(autores_por_cap, 40))
cat("\nAvance por capítulo (primeros 10):\n")
print(head(avance_cap, 40))
cat("\nArchivos generados en:\n", normalizePath(out_dir), "\n")

# --------------------------
# 8) Cerrar conexión
# --------------------------
dbDisconnect(con)
