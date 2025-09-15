# =========================
# Chequeo rápido de tablas PMP
# =========================

check_pmp_tables <- function(con){
  tablas <- c(
    "entregable", "entregable_nota", "entregable_revision", "entregable_tipo",
    "fase",
    "riesgo", "riesgo_accion", "riesgo_categoria", "riesgo_escala",
    "riesgo_evento", "riesgo_matriz", "riesgo_nota"
  )
  
  res <- data.frame(
    tabla = character(),
    existe = logical(),
    registros = integer(),
    stringsAsFactors = FALSE
  )
  
  existentes <- DBI::dbListTables(con)
  
  for (t in tablas) {
    if (t %in% existentes) {
      n <- tryCatch(
        DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", t))$n,
        error = function(e) NA_integer_
      )
      res <- rbind(res, data.frame(tabla=t, existe=TRUE, registros=n))
    } else {
      res <- rbind(res, data.frame(tabla=t, existe=FALSE, registros=NA))
    }
  }
  res
}

# =========================
# Ejemplo de uso
# =========================
con <- get_con()   # Usa tu función de conexión ya definida
estado_pmp <- check_pmp_tables(con)
print(estado_pmp)

# Si quieres verlo bonito en Shiny o RStudio:
DT::datatable(estado_pmp, options=list(pageLength=12))
