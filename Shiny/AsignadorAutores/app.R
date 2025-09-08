# ======================================================
# app.R — Shiny: Asignador de Autores (Libro de Estadística)
# Carpeta sugerida: Libro-Estadistica/Shiny/AsignadorAutores/app.R
# Credenciales: usa ~/.Renviron o edita Sys.setenv() abajo.
# ======================================================

# --- Credenciales MySQL (si NO usas ~/.Renviron, deja esto activo) ---
Sys.setenv(
  MYSQL_HOST = Sys.getenv("MYSQL_HOST", "localhost"),
  MYSQL_PORT = Sys.getenv("MYSQL_PORT", "3306"),
  MYSQL_USER = Sys.getenv("MYSQL_USER", "root"),
  MYSQL_PASSWORD = Sys.getenv("MYSQL_PASSWORD", "mvSH2014**"),
  MYSQL_DBNAME = Sys.getenv("MYSQL_DBNAME", "libro")
)

library(shiny)
library(DBI)
library(RMariaDB)
library(dplyr)
library(DT)
library(shinyWidgets)

# ---------- Conexión y helpers ----------
get_con <- function() {
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host     = Sys.getenv("MYSQL_HOST", "localhost"),
    port     = as.integer(Sys.getenv("MYSQL_PORT", "3306")),
    user     = Sys.getenv("MYSQL_USER", "root"),
    password = Sys.getenv("MYSQL_PASSWORD", ""),
    dbname   = Sys.getenv("MYSQL_DBNAME", "libro")
  )
}
q    <- function(con, sql, ...)  tryCatch(DBI::dbGetQuery(con, sql, ...), error=function(e){ warning(e$message); tibble() })
exec <- function(con, sql, ...) tryCatch(DBI::dbExecute(con, sql, ...),  error=function(e){ warning(e$message); -1 })

rol_id <- function(con, rol_txt){
  df <- q(con,"SELECT rol_id FROM rol WHERE nombre=?", params=list(rol_txt))
  if (nrow(df)==1) df$rol_id[1] else NA_integer_
}
cap_id <- function(con, num){
  df <- q(con,"SELECT capitulo_id FROM capitulo WHERE numero=?", params=list(num))
  if (nrow(df)==1) df$capitulo_id[1] else NA_integer_
}
sub_id <- function(con, num_cap, num_sub){
  df <- q(con,"
    SELECT sc.subcapitulo_id
      FROM subcapitulo sc
      JOIN capitulo c ON c.capitulo_id=sc.capitulo_id
     WHERE c.numero=? AND sc.numero=?", params=list(num_cap,num_sub))
  if (nrow(df)==1) df$subcapitulo_id[1] else NA_integer_
}

tiene_principal_cap <- function(con, cap){
  q(con,"SELECT COUNT(*) n FROM capitulo_autor WHERE capitulo_id=? AND rol_id=1",
    params=list(cap))$n[1] > 0
}
tiene_principal_sub <- function(con, sub){
  q(con,"SELECT COUNT(*) n FROM subcapitulo_autor WHERE subcapitulo_id=? AND rol_id=1",
    params=list(sub))$n[1] > 0
}

normalizar_cap <- function(con, cap){
  exec(con,"DROP TEMPORARY TABLE IF EXISTS tmp_orden_cap")
  exec(con, paste0(
    "CREATE TEMPORARY TABLE tmp_orden_cap AS
     SELECT ca.capitulo_id, ca.autor_id, ca.rol_id,
            ROW_NUMBER() OVER(
              PARTITION BY ca.capitulo_id
              ORDER BY CASE ca.rol_id WHEN 1 THEN 0 ELSE 1 END,
                       (SELECT nombre_completo FROM autor a WHERE a.autor_id=ca.autor_id)
            ) rn
       FROM capitulo_autor ca
      WHERE ca.capitulo_id=", cap))
  exec(con,"
    UPDATE capitulo_autor ca
      JOIN tmp_orden_cap t USING (capitulo_id, autor_id, rol_id)
       SET ca.orden=t.rn
     WHERE ca.capitulo_id=?", params=list(cap))
}
normalizar_sub <- function(con, sub){
  exec(con,"DROP TEMPORARY TABLE IF EXISTS tmp_orden_sub")
  exec(con, paste0(
    "CREATE TEMPORARY TABLE tmp_orden_sub AS
     SELECT sa.subcapitulo_id, sa.autor_id, sa.rol_id,
            ROW_NUMBER() OVER(
              PARTITION BY sa.subcapitulo_id
              ORDER BY CASE sa.rol_id WHEN 1 THEN 0 ELSE 1 END,
                       (SELECT nombre_completo FROM autor a WHERE a.autor_id=sa.autor_id)
            ) rn
       FROM subcapitulo_autor sa
      WHERE sa.subcapitulo_id=", sub))
  exec(con,"
    UPDATE subcapitulo_autor sa
      JOIN tmp_orden_sub t USING (subcapitulo_id, autor_id, rol_id)
       SET sa.orden=t.rn
     WHERE sa.subcapitulo_id=?", params=list(sub))
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Asignador de Autores — Libro de Estadística"),
  tags$small("Variables: MYSQL_HOST, MYSQL_PORT, MYSQL_USER, MYSQL_PASSWORD, MYSQL_DBNAME"),
  br(),
  fluidRow(
    column(
      width = 4,
      h4("Selección"),
      pickerInput("cap_num", "Capítulo:", choices = NULL, options=list(`live-search`=TRUE)),
      numericInput("sub_num", "Subcapítulo (1-3)", value = NA, min = 1, max = 3, step = 1),
      pickerInput("autor", "Autor:", choices = NULL, options=list(`live-search`=TRUE)),
      pickerInput("rol", "Rol:", choices = c("Autor principal","Coautor","Revisor","Coordinador")),
      numericInput("orden", "Orden (opcional)", value = NA, min = 1, step = 1),
      actionButton("add_cap", "Asignar a CAPÍTULO", class="btn-primary"),
      actionButton("add_sub", "Asignar a SUBCAPÍTULO", class="btn-secondary"),
      br(), br(),
      downloadButton("dl_cap_csv",  "Equipo del CAPÍTULO (CSV)", class="btn-success"),
      downloadButton("dl_sub_csv",  "Equipo del SUBCAPÍTULO (CSV)", class="btn-success"),
      downloadButton("dl_full_csv", "Todas las asignaciones (CSV)", class="btn-info"),
      hr(),
      strong("Acciones sobre selección"), br(),
      actionButton("rm_cap", "Quitar de CAPÍTULO", class="btn-danger"),
      actionButton("rm_sub", "Quitar de SUBCAPÍTULO", class="btn-danger"),
      br(), br(),
      selectInput("rol_edit", "Nuevo rol", c("Autor principal","Coautor","Revisor","Coordinador")),
      numericInput("orden_edit", "Nuevo orden (opcional)", value = NA, min = 1, step = 1),
      actionButton("upd_cap", "Actualizar en CAPÍTULO", class="btn-warning"),
      actionButton("upd_sub", "Actualizar en SUBCAPÍTULO", class="btn-warning"),
      br(), br(),
      actionButton("up_cap",   "Subir en CAPÍTULO ↑", class="btn-secondary"),
      actionButton("down_cap", "Bajar en CAPÍTULO ↓", class="btn-secondary"),
      br(),
      actionButton("up_sub",   "Subir en SUBCAPÍTULO ↑", class="btn-secondary"),
      actionButton("down_sub", "Bajar en SUBCAPÍTULO ↓", class="btn-secondary"),
      br(), br(),
      actionButton("norm_cap", "Normalizar ORDEN del CAPÍTULO", class="btn-success"),
      actionButton("norm_sub", "Normalizar ORDEN del SUBCAPÍTULO", class="btn-success")
    ),
    column(
      width = 8,
      tabsetPanel(
        tabPanel("Equipo del capítulo",    DTOutput("tabla_cap")),
        tabPanel("Equipo del subcapítulo", DTOutput("tabla_sub"))
      )
    )
  ),
  
  # ======== (AGREGADO) Leyenda fija al final de la página ========
 
  # --- LEYENDA flotante (abajo-derecha) ---
  tags$head(tags$style(HTML("
  .legend-fixed {
    position: fixed;
    right: 10px; bottom: 10px;
    width: 35%; max-width: 420px;
    z-index: 1000; background: #ffffff;
    border: 1px solid #e5e5e5;
    box-shadow: 0 0 8px rgba(0,0,0,.10);
    border-radius: 6px;
  }
  .legend-fixed .legend-inner {
    padding: 10px 14px;
    font-size: 13px; line-height: 1.35;
  }
  .legend-fixed ul { margin-bottom: 0; padding-left: 18px; }
  /* En pantallas pequeñas, que use casi todo el ancho */
  @media (max-width: 992px) {
    .legend-fixed { right: 8px; left: 8px; width: auto; max-width: none; }
  }
"))),
  div(
    class = "legend-fixed",
    div(
      class = "legend-inner",
      wellPanel(
        tags$b("Leyenda rápida:"),
        tags$ul(
          tags$li(tags$b("Asignar a CAPÍTULO / SUBCAPÍTULO:"), " crea la relación con el autor y rol indicado (si ya existe, se ignora)."),
          tags$li(tags$b("Quitar de CAPÍTULO / SUBCAPÍTULO:"), " elimina la relación seleccionada en la tabla."),
          tags$li(tags$b("Nuevo rol / Nuevo orden"), " + ", tags$b("Actualizar en ..."), ": modifica el registro seleccionado."),
          tags$li(tags$b("Subir / Bajar:"), " mueve el autor dentro del equipo respetando que el ", tags$i("Autor principal"), " queda primero."),
          tags$li(tags$b("Normalizar orden:"), " reenumera el orden empezando en 1 (principal primero, luego alfabético)."),
          tags$li(tags$b("Descargas CSV:"), " exportan el equipo del capítulo, del subcapítulo o el consolidado.")
        )
      )
    )
  )
  # --- FIN LEYENDA flotante ---
  
  
  # ======== FIN AGREGADO ========
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  con <- get_con()
  onStop(function() try(DBI::dbDisconnect(con), silent = TRUE))
  
  # Cargar opciones
  capitulos <- q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero")
  autores   <- q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
  
  updatePickerInput(
    session, "cap_num",
    choices = setNames(capitulos$numero, paste0("Cap ", capitulos$numero, " — ", capitulos$titulo))
  )
  updatePickerInput(
    session, "autor",
    choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">"))
  )
  
  # Data reactiva
  cap_df <- reactive({
    req(input$cap_num)
    q(con, "
      SELECT c.capitulo_id, a.autor_id, r.rol_id,
             c.numero, c.titulo, a.nombre_completo, a.email, r.nombre AS rol, ca.orden
        FROM capitulo_autor ca
        JOIN capitulo c ON c.capitulo_id=ca.capitulo_id
        JOIN autor a    ON a.autor_id=ca.autor_id
        JOIN rol r      ON r.rol_id=ca.rol_id
       WHERE c.numero=?
       ORDER BY ca.orden, a.nombre_completo",
      params=list(as.integer(input$cap_num)))
  })
  
  sub_df <- reactive({
    req(input$cap_num, !is.na(input$sub_num))
    q(con, "
      SELECT sc.subcapitulo_id, a.autor_id, r.rol_id,
             c.numero AS cap, sc.numero AS sub, sc.titulo AS sub_titulo,
             a.nombre_completo, a.email, r.nombre AS rol, sa.orden
        FROM subcapitulo_autor sa
        JOIN subcapitulo sc ON sc.subcapitulo_id=sa.subcapitulo_id
        JOIN capitulo c     ON c.capitulo_id=sc.capitulo_id
        JOIN autor a        ON a.autor_id=sa.autor_id
        JOIN rol r          ON r.rol_id=sa.rol_id
       WHERE c.numero=? AND sc.numero=?
       ORDER BY sa.orden, a.nombre_completo",
      params=list(as.integer(input$cap_num), as.integer(input$sub_num)))
  })
  
  output$tabla_cap <- renderDT({
    datatable(cap_df(), selection="single", rownames=FALSE,
              options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2)))))
  })
  output$tabla_sub <- renderDT({
    datatable(sub_df(), selection="single", rownames=FALSE,
              options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2)))))
  })
  
  # Alta
  observeEvent(input$add_cap, {
    v_cap <- cap_id(con, as.integer(input$cap_num))
    v_aut <- as.integer(input$autor)
    v_rol <- rol_id(con, input$rol)
    v_ord <- if (is.na(input$orden)) NULL else as.integer(input$orden)
    validate(need(!is.na(v_cap),"Capítulo no válido"),
             need(!is.na(v_aut),"Autor no válido"),
             need(!is.na(v_rol),"Rol no válido"))
    if (v_rol==1 && tiene_principal_cap(con, v_cap)) {
      showNotification("Ya existe Autor principal en este capítulo", type="error"); return(NULL)
    }
    exec(con,"INSERT IGNORE INTO capitulo_autor (capitulo_id, autor_id, rol_id, orden) VALUES (?,?,?,?)",
         params=list(v_cap, v_aut, v_rol, v_ord))
    normalizar_cap(con, v_cap); showNotification("Asignado al capítulo ✔", type="message")
  })
  
  observeEvent(input$add_sub, {
    v_sub <- sub_id(con, as.integer(input$cap_num), as.integer(input$sub_num))
    v_aut <- as.integer(input$autor)
    v_rol <- rol_id(con, input$rol)
    v_ord <- if (is.na(input$orden)) NULL else as.integer(input$orden)
    validate(need(!is.na(v_sub),"Subcapítulo no válido"),
             need(!is.na(v_aut),"Autor no válido"),
             need(!is.na(v_rol),"Rol no válido"))
    if (v_rol==1 && tiene_principal_sub(con, v_sub)) {
      showNotification("Ya existe Autor principal en este subcapítulo", type="error"); return(NULL)
    }
    exec(con,"INSERT IGNORE INTO subcapitulo_autor (subcapitulo_id, autor_id, rol_id, orden) VALUES (?,?,?,?)",
         params=list(v_sub, v_aut, v_rol, v_ord))
    normalizar_sub(con, v_sub); showNotification("Asignado al subcapítulo ✔", type="message")
  })
  
  # Baja
  observeEvent(input$rm_cap, {
    s <- input$tabla_cap_rows_selected; req(length(s)==1)
    df <- cap_df()
    exec(con,"DELETE FROM capitulo_autor WHERE capitulo_id=? AND autor_id=? AND rol_id=?",
         params=list(df$capitulo_id[s], df$autor_id[s], df$rol_id[s]))
    normalizar_cap(con, df$capitulo_id[s]); showNotification("Eliminado del capítulo ✔", type="message")
  })
  
  observeEvent(input$rm_sub, {
    s <- input$tabla_sub_rows_selected; req(length(s)==1)
    df <- sub_df()
    exec(con,"DELETE FROM subcapitulo_autor WHERE subcapitulo_id=? AND autor_id=? AND rol_id=?",
         params=list(df$subcapitulo_id[s], df$autor_id[s], df$rol_id[s]))
    normalizar_sub(con, df$subcapitulo_id[s]); showNotification("Eliminado del subcapítulo ✔", type="message")
  })
  
  # Update
  observeEvent(input$upd_cap, {
    s <- input$tabla_cap_rows_selected; req(length(s)==1)
    df <- cap_df()
    new_rol <- rol_id(con, input$rol_edit)
    new_ord <- if (is.na(input$orden_edit)) NULL else as.integer(input$orden_edit)
    if (new_rol==1 && tiene_principal_cap(con, df$capitulo_id[s]) && df$rol_id[s]!=1) {
      showNotification("Ya existe Autor principal en este capítulo", type="error"); return(NULL)
    }
    exec(con,"UPDATE capitulo_autor SET rol_id=?, orden=? WHERE capitulo_id=? AND autor_id=?",
         params=list(new_rol, new_ord, df$capitulo_id[s], df$autor_id[s]))
    normalizar_cap(con, df$capitulo_id[s]); showNotification("Actualizado en capítulo ✔", type="warning")
  })
  
  observeEvent(input$upd_sub, {
    s <- input$tabla_sub_rows_selected; req(length(s)==1)
    df <- sub_df()
    new_rol <- rol_id(con, input$rol_edit)
    new_ord <- if (is.na(input$orden_edit)) NULL else as.integer(input$orden_edit)
    if (new_rol==1 && tiene_principal_sub(con, df$subcapitulo_id[s]) && df$rol_id[s]!=1) {
      showNotification("Ya existe Autor principal en este subcapítulo", type="error"); return(NULL)
    }
    exec(con,"UPDATE subcapitulo_autor SET rol_id=?, orden=? WHERE subcapitulo_id=? AND autor_id=?",
         params=list(new_rol, new_ord, df$subcapitulo_id[s], df$autor_id[s]))
    normalizar_sub(con, df$subcapitulo_id[s]); showNotification("Actualizado en subcapítulo ✔", type="warning")
  })
  
  # Mover (↑/↓) con protección de “principal”
  mover_cap <- function(con, cap, autor, dir=c("UP","DOWN")){
    dir <- match.arg(dir)
    df <- q(con, "
      SELECT ca.capitulo_id, ca.autor_id, ca.rol_id, ca.orden,
             ROW_NUMBER() OVER(
               PARTITION BY ca.capitulo_id
               ORDER BY CASE ca.rol_id WHEN 1 THEN 0 ELSE 1 END,
                        (SELECT nombre_completo FROM autor a WHERE a.autor_id=ca.autor_id)
             ) rn
        FROM capitulo_autor ca
       WHERE ca.capitulo_id=?", params=list(cap))
    if (!nrow(df)) return(invisible())
    row <- df[df$autor_id==autor,]; if (!nrow(row)) return(invisible())
    rn <- row$rn[1]; rol <- row$rol_id[1]
    rn_t <- if (dir=="UP") rn-1 else rn+1
    if (rn_t < 1 || rn_t > nrow(df)) return(invisible())
    tgt <- df[df$rn==rn_t,]
    if (dir=="UP" && rol!=1 && tgt$rol_id[1]==1) return(invisible())
    exec(con,"UPDATE capitulo_autor SET orden=IFNULL(orden,10000) WHERE capitulo_id=? AND autor_id IN (?,?)",
         params=list(cap, autor, tgt$autor_id[1]))
    normalizar_cap(con, cap)
  }
  mover_sub <- function(con, sub, autor, dir=c("UP","DOWN")){
    dir <- match.arg(dir)
    df <- q(con, "
      SELECT sa.subcapitulo_id, sa.autor_id, sa.rol_id, sa.orden,
             ROW_NUMBER() OVER(
               PARTITION BY sa.subcapitulo_id
               ORDER BY CASE sa.rol_id WHEN 1 THEN 0 ELSE 1 END,
                        (SELECT nombre_completo FROM autor a WHERE a.autor_id=sa.autor_id)
             ) rn
        FROM subcapitulo_autor sa
       WHERE sa.subcapitulo_id=?", params=list(sub))
    if (!nrow(df)) return(invisible())
    row <- df[df$autor_id==autor,]; if (!nrow(row)) return(invisible())
    rn <- row$rn[1]; rol <- row$rol_id[1]
    rn_t <- if (dir=="UP") rn-1 else rn+1
    if (rn_t < 1 || rn_t > nrow(df)) return(invisible())
    tgt <- df[df$rn==rn_t,]
    if (dir=="UP" && rol!=1 && tgt$rol_id[1]==1) return(invisible())
    exec(con,"UPDATE subcapitulo_autor SET orden=IFNULL(orden,10000) WHERE subcapitulo_id=? AND autor_id IN (?,?)",
         params=list(sub, autor, tgt$autor_id[1]))
    normalizar_sub(con, sub)
  }
  
  observeEvent(input$up_cap,   { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df(); mover_cap(con, df$capitulo_id[s], df$autor_id[s], "UP");   showNotification("Movido ↑ en capítulo", type="message") })
  observeEvent(input$down_cap, { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df(); mover_cap(con, df$capitulo_id[s], df$autor_id[s], "DOWN"); showNotification("Movido ↓ en capítulo", type="message") })
  observeEvent(input$up_sub,   { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df(); mover_sub(con, df$subcapitulo_id[s], df$autor_id[s], "UP");   showNotification("Movido ↑ en subcapítulo", type="message") })
  observeEvent(input$down_sub, { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df(); mover_sub(con, df$subcapitulo_id[s], df$autor_id[s], "DOWN"); showNotification("Movido ↓ en subcapítulo", type="message") })
  
  observeEvent(input$norm_cap, { req(input$cap_num);  normalizar_cap(con, cap_id(con, as.integer(input$cap_num))); showNotification("Orden normalizado en capítulo ✔", type="message") })
  observeEvent(input$norm_sub, { req(input$cap_num, input$sub_num); normalizar_sub(con, sub_id(con, as.integer(input$cap_num), as.integer(input$sub_num))); showNotification("Orden normalizado en subcapítulo ✔", type="message") })
  
  # Descargas
  output$dl_cap_csv <- downloadHandler(
    filename = function() sprintf("equipo_cap_%s.csv", input$cap_num),
    content  = function(file) write.csv(cap_df(), file, row.names=FALSE, fileEncoding="UTF-8")
  )
  output$dl_sub_csv <- downloadHandler(
    filename = function() sprintf("equipo_cap_%s_sub_%s.csv", input$cap_num, input$sub_num),
    content  = function(file) write.csv(sub_df(), file, row.names=FALSE, fileEncoding="UTF-8")
  )
  output$dl_full_csv <- downloadHandler(
    filename = function() sprintf("asignaciones_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content  = function(file) {
      df <- q(con, "
        SELECT c.numero, c.titulo, a.nombre_completo, a.email, r.nombre AS rol, ca.orden
          FROM capitulo_autor ca
          JOIN capitulo c ON c.capitulo_id=ca.capitulo_id
          JOIN autor a    ON a.autor_id=ca.autor_id
          JOIN rol r      ON r.rol_id=ca.rol_id
         ORDER BY c.numero, ca.orden, a.nombre_completo")
      write.csv(df, file, row.names=FALSE, fileEncoding="UTF-8")
    }
  )
}

shinyApp(ui, server)
