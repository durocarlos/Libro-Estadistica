# ======================================================
# app.R — Libro de Estadística + Alumnos (completo)
# ======================================================

# --- Credenciales MySQL (ajusta si hace falta) ---
Sys.setenv(
  MYSQL_HOST = Sys.getenv("MYSQL_HOST", "localhost"),
  MYSQL_PORT = Sys.getenv("MYSQL_PORT", "3306"),
  MYSQL_USER = Sys.getenv("MYSQL_USER", "root"),
  MYSQL_PASSWORD = Sys.getenv("MYSQL_PASSWORD", "mvSH2014**"),
  MYSQL_DBNAME = Sys.getenv("MYSQL_DBNAME", "libro")
)

suppressPackageStartupMessages({
  library(shiny); library(shinyjs); library(DBI); library(RMariaDB)
  library(dplyr); library(tidyr); library(DT); library(shinyWidgets)
  library(readr); library(ggplot2); library(scales); library(bslib)
  library(glue); library(htmltools); library(blastula); library(knitr)
  library(keyring); library(rmarkdown)
})

# ---------- Conexión y utilitarios ----------
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

q <- function(con, sql, ...) {
  tryCatch(DBI::dbGetQuery(con, sql, ...),
           error = function(e){ warning(e$message); tibble() })
}
exec <- function(con, sql, ...) {
  tryCatch(DBI::dbExecute(con, sql, ...),
           error = function(e){ warning(e$message); -1L })
}

to_db_int <- function(x){
  if (is.null(x) || length(x)==0 || is.na(x) || !nzchar(as.character(x))) NA_integer_ else as.integer(x)
}
`%||%` <- function(x,y) if (is.null(x) || length(x)==0 || is.na(x)) y else x

# Helpers autores/capitulos
rol_id <- function(con, rol_txt){
  df <- q(con,"SELECT rol_id FROM rol WHERE nombre=?", params=list(rol_txt))
  if (nrow(df)==1) df$rol_id[1] else NA_integer_
}
cap_id <- function(con, num){
  df <- q(con,"SELECT capitulo_id FROM capitulo WHERE numero=?", params=list(num))
  if (nrow(df)==1) df$capitulo_id[1] else NA_integer_
}
sub_id <- function(con, num_cap, num_sub){
  df <- q(con,"SELECT sc.subcapitulo_id FROM subcapitulo sc JOIN capitulo c ON c.capitulo_id=sc.capitulo_id WHERE c.numero=? AND sc.numero=?", params=list(num_cap,num_sub))
  if (nrow(df)==1) df$subcapitulo_id[1] else NA_integer_
}
tiene_principal_cap <- function(con, cap){
  q(con,"SELECT COUNT(*) n FROM capitulo_autor WHERE capitulo_id=? AND rol_id=1", params=list(cap))$n[1] > 0
}
tiene_principal_sub <- function(con, sub){
  q(con,"SELECT COUNT(*) n FROM subcapitulo_autor WHERE subcapitulo_id=? AND rol_id=1", params=list(sub))$n[1] > 0
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

# ============== MÓDULO: Correos (libre) ==============
correo_libre_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Redacción y destinatarios"),
      fluidRow(
        column(4,
               textInput(ns("creds_id"), "ID de credencial (keyring)", "blastula-v1-office365"),
               verbatimTextOutput(ns("cred_info"), placeholder = TRUE),
               textInput(ns("from_email"), "Remitente (debe coincidir con la credencial)", ""),
               pickerInput(ns("contactos"), "Contactos (tabla autor)", choices=NULL, multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),
               div(
                 actionButton(ns("add_to_to"),  "➕ a Para", class="btn btn-outline-primary btn-sm"),
                 actionButton(ns("add_to_cc"),  "➕ a CC",   class="btn btn-outline-secondary btn-sm"),
                 actionButton(ns("add_to_bcc"), "➕ a CCO",  class="btn btn-outline-secondary btn-sm")
               ),
               br(),
               textAreaInput(ns("to"),  "Para (uno por línea o separados por ;)", width="100%", height="110px"),
               textAreaInput(ns("cc"),  "CC (opcional)",  width="100%", height="70px"),
               textAreaInput(ns("bcc"), "CCO (opcional)", width="100%", height="70px"),
               textInput(ns("subject"), "Asunto", ""),
               checkboxInput(ns("modo_prueba"), "Modo prueba (guardar HTML en outbox/ y NO enviar)", TRUE),
               fileInput(ns("up_files"), "Subir archivos (adjuntos)", multiple = TRUE),
               textAreaInput(ns("abs_paths"), "Rutas absolutas a adjuntar (una por línea)", width="100%", height="120px",
                             placeholder="C:\\ruta\\a\\archivo1.pdf\nD:\\otra\\carpeta\\archivo2.xlsx"),
               actionButton(ns("send_btn"), "✉️ Enviar / Generar", class="btn btn-success", width = "100%")
        ),
        column(8,
               textAreaInput(ns("body_md"), "Cuerpo (Markdown)", width="100%", height="360px",
                             "Estimado/a:\n\nAdjunto encontrará la documentación correspondiente.\n\nSaludos cordiales,\nCoordinación Libro de Estadística"),
               hr(), htmlOutput(ns("preview_html"))
        )
      )
    ),
    bslib::card(bslib::card_header("Registro (Log)"), DT::DTOutput(ns("tbl_log")))
  )
}
correo_libre_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    app_dir <- normalizePath(getwd()); outbox <- file.path(app_dir, "outbox")
    if (!dir.exists(outbox)) dir.create(outbox, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(outbox, "sent_log.csv")
    `%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a
    parse_emails <- function(x){ x <- x %||% ""; if (!nzchar(trimws(x))) return(character(0)); y <- unlist(strsplit(x, "[;\n\r]+")); y <- trimws(y); y <- y[nzchar(y)]; unique(y) }
    paste_emails <- function(v){ v <- unique(trimws(v)); v <- v[nzchar(v)]; paste(v, collapse = ";\n") }
    is_email <- function(z) grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", z)
    append_log <- function(row){
      row$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      row <- row[, c("timestamp","cred_id","from","to","cc","bcc","subject","modo_prueba","resultado","detalle")]
      if (!file.exists(log_file)) write.csv(row, log_file, row.names=FALSE, fileEncoding="UTF-8")
      else write.table(row, log_file, sep=",", col.names=FALSE, row.names=FALSE, append=TRUE)
    }
    get_contactos <- reactive({
      tryCatch(DBI::dbGetQuery(con, "SELECT nombre_completo, email FROM autor WHERE email IS NOT NULL AND email <> '' ORDER BY nombre_completo"),
               error=function(e){ warning(e$message); data.frame(nombre_completo=character(), email=character()) })
    })
    observe({
      df <- get_contactos(); ch <- setNames(df$email, paste0(df$nombre_completo, " <", df$email, ">"))
      updatePickerInput(session, "contactos", choices = ch)
    })
    observeEvent(input$add_to_to,  { s <- input$contactos; if(length(s)) updateTextAreaInput(session,"to",  value=paste_emails(c(parse_emails(input$to),  s))) })
    observeEvent(input$add_to_cc,  { s <- input$contactos; if(length(s)) updateTextAreaInput(session,"cc",  value=paste_emails(c(parse_emails(input$cc),  s))) })
    observeEvent(input$add_to_bcc,{ s <- input$contactos; if(length(s)) updateTextAreaInput(session,"bcc", value=paste_emails(c(parse_emails(input$bcc), s))) })
    cred_user <- reactive({
      id <- input$creds_id %||% ""; if (!nzchar(id)) return(NA_character_)
      kr <- try(keyring::key_list(id), silent = TRUE)
      if (inherits(kr, "try-error") || nrow(kr)==0) return(NA_character_)
      kr$username[1]
    })
    output$cred_info <- renderText({
      u <- cred_user()
      if (is.na(u)) "✗ Credencial NO encontrada."
      else { isolate(if (!nzchar(trimws(input$from_email))) updateTextInput(session,"from_email", value = u)); paste0("✓ Credencial: ", input$creds_id, "\nUsuario: ", u) }
    })
    output$preview_html <- renderUI({
      subj <- input$subject %||% ""; body <- input$body_md %||% ""
      HTML(glue::glue("<h4>Asunto</h4><p>{htmltools::htmlEscape(subj)}</p>
                       <h4>Cuerpo</h4><pre style='white-space:pre-wrap'>{htmltools::htmlEscape(body)}</pre>"))
    })
    output$tbl_log <- DT::renderDT({
      if (!file.exists(log_file)) return(DT::datatable(data.frame(MENSAJE="Sin registros")))
      df <- tryCatch(read.csv(log_file, fileEncoding="UTF-8"), error=function(e) data.frame(ERROR=e$message))
      if (nrow(df) > 500) df <- utils::tail(df, 500)
      DT::datatable(df, options=list(pageLength=10, scrollX=TRUE))
    })
    observeEvent(input$send_btn, {
      cred_id <- input$creds_id %||% ""; cu <- cred_user()
      validate(need(nzchar(cred_id),"ID de credencial requerido"), need(!is.na(cu),"No encuentro esa credencial en keyring."))
      from_addr <- tolower(trimws(input$from_email %||% ""))
      validate(need(nzchar(from_addr),"Remitente requerido"), need(identical(from_addr, tolower(cu)), paste0("El remitente debe ser exactamente: ", cu)))
      tos <- parse_emails(input$to); ccs <- parse_emails(input$cc); bccs <- parse_emails(input$bcc)
      validate(need(length(tos)>0,"Indica al menos un destinatario en 'Para'"))
      bad <- c(tos,ccs,bccs)[!is_email(c(tos,ccs,bccs))]
      validate(need(length(bad)==0, paste("Direcciones inválidas:", paste(bad, collapse=", "))))
      subj <- input$subject %||% ""; validate(need(nzchar(trimws(subj)), "Debes indicar un Asunto"))
      body <- input$body_md %||% ""
      em <- blastula::compose_email(body=blastula::md(body), footer = blastula::md(glue::glue("_Enviado · {format(Sys.time(), '%Y-%m-%d %H:%M')}._")))
      if (!is.null(input$up_files) && nrow(input$up_files) > 0) {
        for (i in seq_len(nrow(input$up_files))) {
          path <- input$up_files$datapath[i]; if (file.exists(path)) try({ em <- em %>% blastula::add_attachment(file = path, filename = input$up_files$name[i]) }, silent = TRUE)
        }
      }
      abs_lines <- input$abs_paths %||% ""
      if (nzchar(trimws(abs_lines))) {
        ap <- unlist(strsplit(abs_lines, "[\r\n]+")); ap <- trimws(ap); ap <- ap[nzchar(ap)]
        for (p in ap) { if (file.exists(p)) try({ em <- em %>% blastula::add_attachment(file = p) }, silent = TRUE) else showNotification(paste("⚠️ No existe:", p), type="warning") }
      }
      log_row <- data.frame(cred_id=cred_id, from=from_addr, to=paste(tos,collapse="; "), cc=paste(ccs,collapse="; "),
                            bcc=paste(bccs,collapse="; "), subject=subj, modo_prueba=isTRUE(input$modo_prueba),
                            resultado="", detalle="", stringsAsFactors=FALSE)
      if (isTRUE(input$modo_prueba)) {
        fhtml <- file.path(outbox, glue::glue("PREVIEW_{format(Sys.time(), '%Y%m%d_%H%M%S')}.html"))
        ok <- FALSE; try({ em %>% blastula::render_email() %>% writeLines(con=fhtml); ok <- file.exists(fhtml) }, silent = TRUE)
        log_row$resultado <- if (ok) "HTML guardado" else "ERROR"; log_row$detalle <- if (ok) basename(fhtml) else "No se pudo escribir HTML"
        append_log(log_row); showNotification("📝 Modo prueba: HTML generado en outbox/", type="message"); return(invisible())
      }
      ok <- FALSE; err_msg <- NULL
      tryCatch({
        blastula::smtp_send(email=em, from=from_addr, to=tos, cc=if(length(ccs)) ccs else NULL, bcc=if(length(bccs)) bccs else NULL,
                            subject=subj, credentials=blastula::creds_key(cred_id)); ok <- TRUE
      }, error=function(e){ err_msg <<- conditionMessage(e) })
      log_row$resultado <- if (ok) "ENVIADO" else "ERROR"; log_row$detalle <- err_msg %||% ""; append_log(log_row)
      if (ok) showNotification("✅ Correo enviado", type="message") else showNotification(paste("❌ No se pudo enviar:", log_row$detalle), type="error")
    })
  })
}

# ================== MÓDULO ALUMNOS ==================
# (usa nombres reales: id_nico, correo_electronico, etc.)
alumnos_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Gestión",
               br(),
               fluidRow(
                 column(5,
                        h4("Formulario alumno"),
                        pickerInput(ns("alumno_pick"), "Escoger alumno (para cargar/editar):", choices=NULL, options=list(`live-search`=TRUE)),
                        actionButton(ns("alumno_load"), "Cargar selección"),
                        br(), br(),
                        textInput(ns("a_id_nico"),       "ID único (respetando ceros a la izquierda)"),
                        textInput(ns("a_nombre"),        "Nombre completo"),
                        textInput(ns("a_email"),         "Correo electrónico"),
                        textInput(ns("a_carrera"),       "Carrera / Facultad"),
                        textInput(ns("a_seccion"),       "Sección (Diurna / Nocturna)"),
                        textInput(ns("a_curso"),         "Curso (II / III)"),
                        textInput(ns("a_asignatura"),    "Asignatura (Descriptiva / Inferencial)"),
                        br(),
                        fluidRow(
                          column(3, actionButton(ns("alumno_new"),  "Crear",   class="btn-primary",  width="100%")),
                          column(3, actionButton(ns("alumno_save"), "Guardar", class="btn-warning",  width="100%")),
                          column(3, actionButton(ns("alumno_del"),  "Eliminar",class="btn-danger",   width="100%")),
                          column(3, actionButton(ns("alumno_clear"),"Limpiar", class="btn-default",  width="100%"))
                        )
                 ),
                 column(7,
                        h4("Alumnos"),
                        DTOutput(ns("tabla_alumnos"))
                 )
               )
      ),
      tabPanel("KPIs",
               br(),
               fluidRow(
                 column(4, bslib::value_box(title="Total alumnos", value=textOutput(ns("kpi_total")),   theme_color="primary")),
                 column(4, bslib::value_box(title="Por sección",   value=textOutput(ns("kpi_seccion")), theme_color="indigo")),
                 column(4, bslib::value_box(title="Por asignatura",value=textOutput(ns("kpi_asig")),    theme_color="teal"))
               ),
               br(),
               bslib::card(
                 bslib::card_header("Resumen"),
                 DTOutput(ns("resumen_tbl"))
               )
      )
    )
  )
}

alumnos_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Cargar listas iniciales
    alumnos_tbl <- reactiveVal(
      q(con, "SELECT alumno_id, id_nico, nombre_completo, correo_electronico AS email,
                     carrera_facultad, seccion, curso, asignatura
              FROM alumno ORDER BY nombre_completo")
    )
    
    # Tabla
    output$tabla_alumnos <- renderDT({
      df <- alumnos_tbl()
      if (!nrow(df)) df <- tibble(
        alumno_id=integer(), id_nico=character(), nombre_completo=character(), email=character(),
        carrera_facultad=character(), seccion=character(), curso=character(), asignatura=character()
      )
      datatable(df, selection="single", rownames=FALSE,
                options=list(pageLength=12, order=list(list(2,"asc")), columnDefs=list(list(visible=FALSE, targets=0))))
    })
    
    # Select de alumnos
    observe({
      df <- alumnos_tbl()
      if (!nrow(df)) {
        updatePickerInput(session, "alumno_pick", choices = setNames(character(0), character(0)))
      } else {
        updatePickerInput(session, "alumno_pick",
                          choices = setNames(df$alumno_id, paste0(df$nombre_completo, " <", df$email, ">")))
      }
    })
    
    # Cargar por selección
    load_alumno_by_id <- function(id_sel){
      id_num <- suppressWarnings(as.integer(id_sel))
      if (is.na(id_num)) { showNotification("Selección inválida.", type="error"); return(invisible()) }
      a <- q(con, "SELECT * FROM alumno WHERE alumno_id=?", params=list(id_num))
      if (!nrow(a)) { showNotification("No se encontró el alumno.", type="error"); return(invisible()) }
      updateTextInput(session, "a_id_nico",    value = a$id_nico[1]             %||% "")
      updateTextInput(session, "a_nombre",     value = a$nombre_completo[1]     %||% "")
      updateTextInput(session, "a_email",      value = a$correo_electronico[1]  %||% "")
      updateTextInput(session, "a_carrera",    value = a$carrera_facultad[1]    %||% "")
      updateTextInput(session, "a_seccion",    value = a$seccion[1]             %||% "")
      updateTextInput(session, "a_curso",      value = a$curso[1]               %||% "")
      updateTextInput(session, "a_asignatura", value = a$asignatura[1]          %||% "")
      showNotification("Alumno cargado ✔", type="message")
    }
    observeEvent(input$alumno_load, { req(input$alumno_pick); load_alumno_by_id(input$alumno_pick) }, ignoreInit=TRUE)
    observeEvent(input$alumno_pick, { if (!is.null(input$alumno_pick) && nzchar(input$alumno_pick)) load_alumno_by_id(input$alumno_pick) }, ignoreInit=TRUE)
    
    # Limpiar formulario
    observeEvent(input$alumno_clear, {
      updateTextInput(session,"a_id_nico",value="")
      updateTextInput(session,"a_nombre",value="")
      updateTextInput(session,"a_email",value="")
      updateTextInput(session,"a_carrera",value="")
      updateTextInput(session,"a_seccion",value="")
      updateTextInput(session,"a_curso",value="")
      updateTextInput(session,"a_asignatura",value="")
      updatePickerInput(session,"alumno_pick",selected=character(0))
      shinyjs::runjs("$('#a_id_nico').trigger('focus');")
    }, ignoreInit=TRUE)
    
    # Crear (no permite si hay selección activa)
    observeEvent(input$alumno_new, {
      if (!is.null(input$alumno_pick) && nzchar(input$alumno_pick)) {
        showModal(modalDialog(
          title = "No se puede crear aún",
          "Tienes un alumno seleccionado. Presiona 'Limpiar' para borrar el formulario y poder crear un nuevo registro.",
          easyClose = TRUE, footer = modalButton("Entendido")
        ))
        return(invisible())
      }
      # Validaciones mínimas
      validate(
        need(nchar(trimws(input$a_id_nico)) > 0,   "ID único requerido"),
        need(nchar(trimws(input$a_nombre))  > 0,   "Nombre requerido"),
        need(nchar(trimws(input$a_email))   > 0,   "Correo requerido")
      )
      # Insert
      sql <- "INSERT INTO alumno
              (id_nico, nombre_completo, correo_electronico, carrera_facultad, seccion, curso, asignatura)
              VALUES (?,?,?,?,?,?,?)"
      res <- tryCatch(DBI::dbExecute(con, sql, params = list(
        trimws(input$a_id_nico), trimws(input$a_nombre), tolower(trimws(input$a_email)),
        trimws(input$a_carrera), trimws(input$a_seccion), trimws(input$a_curso), trimws(input$a_asignatura)
      )), error=function(e) e)
      if (inherits(res,"error")) {
        msg <- conditionMessage(res)
        if (grepl("Duplicate entry|1062", msg, ignore.case=TRUE)) {
          showNotification("❌ No se pudo crear: ya existe un alumno con ese ID o correo.", type="error", duration=8)
        } else {
          showNotification(paste("❌ Error al crear alumno:", msg), type="error", duration=8)
        }
        return(invisible())
      }
      # Refrescar
      alumnos_tbl(q(con, "SELECT alumno_id, id_nico, nombre_completo, correo_electronico AS email, carrera_facultad, seccion, curso, asignatura FROM alumno ORDER BY nombre_completo"))
      updateTextInput(session,"a_id_nico",""); updateTextInput(session,"a_nombre",""); updateTextInput(session,"a_email","")
      updateTextInput(session,"a_carrera",""); updateTextInput(session,"a_seccion",""); updateTextInput(session,"a_curso",""); updateTextInput(session,"a_asignatura","")
      updatePickerInput(session,"alumno_pick",selected=character(0))
      shinyjs::runjs("$('#a_id_nico').trigger('focus');")
      showNotification("✅ Alumno creado. Formulario listo para un nuevo registro.", type="message")
    })
    
    # Guardar cambios
    observeEvent(input$alumno_save, {
      req(input$alumno_pick)
      validate(
        need(nchar(trimws(input$a_nombre)) > 0, "Nombre requerido"),
        need(nchar(trimws(input$a_email))  > 0, "Correo requerido")
      )
      sql <- "UPDATE alumno
              SET id_nico=?, nombre_completo=?, correo_electronico=?, carrera_facultad=?, seccion=?, curso=?, asignatura=?
              WHERE alumno_id=?"
      res <- tryCatch(DBI::dbExecute(con, sql, params=list(
        trimws(input$a_id_nico), trimws(input$a_nombre), tolower(trimws(input$a_email)),
        trimws(input$a_carrera), trimws(input$a_seccion), trimws(input$a_curso), trimws(input$a_asignatura),
        as.integer(input$alumno_pick)
      )), error=function(e) e)
      if (inherits(res,"error")) {
        showNotification(paste("❌ Error al guardar:", conditionMessage(res)), type="error", duration=8); return(invisible())
      }
      alumnos_tbl(q(con, "SELECT alumno_id, id_nico, nombre_completo, correo_electronico AS email, carrera_facultad, seccion, curso, asignatura FROM alumno ORDER BY nombre_completo"))
      showNotification("📝 Cambios guardados.", type="warning")
    })
    
    # Eliminar
    observeEvent(input$alumno_del, {
      req(input$alumno_pick)
      res <- tryCatch(DBI::dbExecute(con, "DELETE FROM alumno WHERE alumno_id=?", params=list(as.integer(input$alumno_pick))), error=function(e) e)
      if (inherits(res,"error")) {
        showNotification(paste("❌ No se pudo eliminar:", conditionMessage(res)), type="error", duration=8); return(invisible())
      }
      alumnos_tbl(q(con, "SELECT alumno_id, id_nico, nombre_completo, correo_electronico AS email, carrera_facultad, seccion, curso, asignatura FROM alumno ORDER BY nombre_completo"))
      updatePickerInput(session,"alumno_pick",selected=character(0))
      showNotification("🗑️ Alumno eliminado.", type="message")
      updateTextInput(session,"a_id_nico",""); updateTextInput(session,"a_nombre",""); updateTextInput(session,"a_email","")
      updateTextInput(session,"a_carrera",""); updateTextInput(session,"a_seccion",""); updateTextInput(session,"a_curso",""); updateTextInput(session,"a_asignatura","")
    })
    
    # -------- KPIs & Resumen (vistas si existen; si no, cálculo directo) --------
    safe_q <- function(sql){
      tryCatch(DBI::dbGetQuery(con, sql), error=function(e) tibble())
    }
    output$kpi_total   <- renderText({
      k <- safe_q("SELECT total_alumnos AS n FROM v_alumno_kpis LIMIT 1")
      if (nrow(k)) return(format(k$n[1]))
      n <- safe_q("SELECT COUNT(*) n FROM alumno"); format(if (nrow(n)) n$n[1] else 0)
    })
    output$kpi_seccion <- renderText({
      k <- safe_q("SELECT total_diurna, total_nocturna FROM v_alumno_kpis LIMIT 1")
      if (nrow(k)) return(paste("Diurna:", k$total_diurna[1], "· Nocturna:", k$total_nocturna[1]))
      d <- safe_q("SELECT SUM(LOWER(seccion) LIKE 'diurna%') di, SUM(LOWER(seccion) LIKE 'nocturna%') no FROM alumno")
      paste("Diurna:", d$di %||% 0, "· Nocturna:", d$no %||% 0)
    })
    output$kpi_asig <- renderText({
      k <- safe_q("SELECT total_descriptiva, total_inferencial FROM v_alumno_kpis LIMIT 1")
      if (nrow(k)) return(paste("Descriptiva:", k$total_descriptiva[1], "· Inferencial:", k$total_inferencial[1]))
      d <- safe_q("SELECT SUM(LOWER(asignatura) LIKE 'descriptiva%') d, SUM(LOWER(asignatura) LIKE 'inferencial%') i FROM alumno")
      paste("Descriptiva:", d$d %||% 0, "· Inferencial:", d$i %||% 0)
    })
    output$resumen_tbl <- renderDT({
      df <- safe_q("SELECT * FROM v_alumno_resumen ORDER BY seccion, curso, asignatura, carrera_facultad")
      if (!nrow(df)) df <- safe_q("
        SELECT seccion, curso, asignatura, carrera_facultad, COUNT(*) AS total
        FROM alumno
        GROUP BY seccion, curso, asignatura, carrera_facultad
        ORDER BY seccion, curso, asignatura, carrera_facultad")
      datatable(df, rownames=FALSE, options=list(pageLength=15, scrollX=TRUE))
    })
  })
}

# ================== MODULO PMP (tab independiente) ==================
pmp_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("pmp_conn_status")),
    fluidRow(
      column(3, bslib::value_box(title="Extreme/Critica", value=htmlOutput(ns("kpi_extreme")), theme_color="#b71c1c")),
      column(3, bslib::value_box(title="Alta",            value=htmlOutput(ns("kpi_high")),    theme_color="#e65100")),
      column(3, bslib::value_box(title="Media",           value=htmlOutput(ns("kpi_medium")),  theme_color="#fdd835")),
      column(3, bslib::value_box(title="Baja",            value=htmlOutput(ns("kpi_low")),     theme_color="#4a3047"))
    ),
    br(),
    bslib::card(bslib::card_header("Acciones (semaforo)"), DTOutput(ns("tbl_acciones"))),
    bslib::card(bslib::card_header("Heatmap Prob × Impacto"), plotOutput(ns("heatmap"), height=420)),
    bslib::card(bslib::card_header("Eventos recientes"), DTOutput(ns("tbl_eventos"))),
    bslib::card(
      bslib::card_header("Exportar / Guardar"),
      textInput(ns("rep_name"), "Nombre base", value=paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))),
      checkboxInput(ns("snap_csv"), "Guardar snapshots CSV", TRUE),
      div(class="d-flex gap-3 flex-wrap",
          downloadButton(ns("dl_md"), "Descargar .md"),
          actionButton(ns("save_md"), "Guardar en /PMP/Riesgos/reportes", class="btn-primary")
      )
    )
  )
}

pmp_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    report_dir <- normalizePath(file.path(getwd(),"PMP","Riesgos","reportes"), mustWork=FALSE)
    if (!dir.exists(report_dir)) dir.create(report_dir, recursive=TRUE, showWarnings=FALSE)
    
    output$pmp_conn_status <- renderUI({
      ok <- TRUE; tryCatch(DBI::dbGetQuery(con,"SELECT 1"), error=function(e) ok <<- FALSE)
      if (isTRUE(ok)) div(class="alert alert-success","Conexion MySQL OK.") else div(class="alert alert-danger","Sin conexion MySQL.")
    })
    
    .sql_prior <- "SELECT prioridad, COUNT(*) AS total FROM v_riesgo_registro GROUP BY prioridad"
    .sql_acc   <- "SELECT accion_id, riesgo_id, titulo, responsable, estado, fecha_compromiso, semaforo FROM v_riesgo_acciones ORDER BY FIELD(semaforo,'Vencida','Por vencer (≤7 días)','En curso','Cerrada'), fecha_compromiso"
    .sql_heat  <- "SELECT prob_nivel, imp_nivel, prioridad, color_hex FROM v_riesgo_heatmap"
    .sql_evt   <- "SELECT evento_id, riesgo_id, tipo, detalle, fecha_evento FROM riesgo_evento ORDER BY evento_id DESC LIMIT 20"
    
    pq <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error=function(e){ warning(e$message); data.frame() })
    prior <- reactive({ pq(.sql_prior) })
    acc   <- reactive({ pq(.sql_acc)   })
    heat  <- reactive({ pq(.sql_heat)  })
    evt   <- reactive({ pq(.sql_evt)   })
    
    output$kpi_extreme <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Extreme","Crítica","Critica")) %>% summarise(n=sum(total,na.rm=TRUE)) %>% pull(n); format(n %||% 0) })
    output$kpi_high    <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("High","Alta"))   %>% summarise(n=sum(total,na.rm=TRUE)) %>% pull(n); format(n %||% 0) })
    output$kpi_medium  <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Medium","Media"))%>% summarise(n=sum(total,na.rm=TRUE)) %>% pull(n); format(n %||% 0) })
    output$kpi_low     <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Low","Baja"))   %>% summarise(n=sum(total,na.rm=TRUE)) %>% pull(n); format(n %||% 0) })
    
    output$tbl_acciones <- DT::renderDT({
      dat <- acc(); validate(need(ncol(dat)>0, "Sin datos (¿existe v_riesgo_acciones?)."))
      dat$fecha_compromiso <- as.Date(dat$fecha_compromiso)
      DT::datatable(dat, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    output$heatmap <- renderPlot({
      h <- heat(); validate(need(ncol(h)>0, "Sin datos (¿existe v_riesgo_heatmap?)."))
      ggplot(h, aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
        geom_tile(color="grey90") + scale_fill_identity() +
        labs(x="Probabilidad", y="Impacto") + theme_minimal(base_size = 14)
    })
    
    output$tbl_eventos <- DT::renderDT({
      e <- evt(); validate(need(ncol(e)>0, "Sin datos (¿hay registros en riesgo_evento?)."))
      e$fecha_evento <- as.Date(e$fecha_evento)
      DT::datatable(e, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    build_md <- function(prior, acc, evt){
      lines <- c(
        paste0("# Reporte de Riesgos - ", format(Sys.Date(), "%Y-%m-%d")), "",
        "## Resumen por prioridad",
        "| Prioridad | Total |", "|---|---|",
        if (nrow(prior)) apply(prior, 1, function(r) paste0("| ", r[['prioridad']], " | ", r[['total']], " |")) else "| (sin datos) | 0 |",
        "", "## Acciones (semaforo)",
        if (nrow(acc)) knitr::kable(acc, format = "pipe") else "_Sin acciones disponibles_",
        "", "## Eventos recientes",
        if (nrow(evt)) knitr::kable(evt, format = "pipe") else "_Sin eventos recientes_",
        "", "_Generado desde Shiny_"
      )
      paste(lines, collapse="\n")
    }
    
    output$dl_md <- downloadHandler(
      filename=function(){ paste0(nzchar(input$rep_name) %||% "reporte_riesgos", ".md") },
      content=function(file){ md <- build_md(prior(), acc(), evt()); writeLines(md, file, useBytes=TRUE) }
    )
    
    observeEvent(input$save_md,{
      base <- if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))
      md_path <- file.path(report_dir, paste0(base, ".md"))
      md <- build_md(prior(), acc(), evt())
      if (!dir.exists(report_dir)) dir.create(report_dir, recursive=TRUE, showWarnings=FALSE)
      writeLines(md, md_path, useBytes=TRUE)
      if (isTRUE(input$snap_csv)) {
        readr::write_csv(prior(), file.path(report_dir, paste0(base,"_prioridades.csv")))
        readr::write_csv(acc(),   file.path(report_dir, paste0(base,"_acciones.csv")))
        readr::write_csv(evt(),   file.path(report_dir, paste0(base,"_eventos.csv")))
      }
      showNotification(paste("Reporte guardado en:", md_path), type="message", duration=6)
    })
  })
}

# ================== MODULO PMP (limpio) ==================
pmp_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("conn_status")),
    tabsetPanel(
      tabPanel("Tablero",
               br(),
               fluidRow(
                 column(3, bslib::value_box(title="Extreme/Crítica", value=htmlOutput(ns("kpi_ext")),  theme_color="#b71c1c")),
                 column(3, bslib::value_box(title="Alta",            value=htmlOutput(ns("kpi_high")), theme_color="#e65100")),
                 column(3, bslib::value_box(title="Media",           value=htmlOutput(ns("kpi_mid")),  theme_color="#fdd835")),
                 column(3, bslib::value_box(title="Baja",            value=htmlOutput(ns("kpi_low")),  theme_color="#4a3047"))
               ),
               br(),
               fluidRow(
                 column(6, bslib::card(bslib::card_header("Heatmap Prob × Impacto"), plotOutput(ns("heatmap"), height=420))),
                 column(6, bslib::card(bslib::card_header("Eventos recientes"), DTOutput(ns("tbl_evt"))))
               )
      ),
      
      tabPanel("Riesgos",
               br(),
               bslib::card(
                 bslib::card_header("Filtros"),
                 fluidRow(
                   column(4, selectizeInput(ns("f_resp"), "Responsable", choices=NULL, multiple=TRUE,
                                            options=list(placeholder="Todos"))),
                   column(4, pickerInput(ns("f_estado"), "Semáforo / Estado",
                                         choices=c("Vencida","Por vencer (≤7 días)","En curso","Cerrada"),
                                         multiple=TRUE, options=list(`actions-box`=TRUE))),
                   column(4, dateRangeInput(ns("f_fecha"), "Rango fecha compromiso"))
                 )
               ),
               br(),
               fluidRow(
                 column(5, bslib::card(bslib::card_header("Resumen por prioridad"), DTOutput(ns("tbl_prior")))),
                 column(7, bslib::card(bslib::card_header("Acciones (filtradas)"),   DTOutput(ns("tbl_acc"))))
               )
      ),
      
      tabPanel("Entregables",
               br(),
               bslib::card(
                 bslib::card_header("Filtros de entregables"),
                 fluidRow(
                   column(4, selectizeInput(ns("e_resp"), "Responsable", choices=NULL, multiple=TRUE,
                                            options=list(placeholder="Todos"))),
                   column(4, pickerInput(ns("e_estado"), "Estado",
                                         choices=c("Pendiente","En curso","Entregado","Aprobado","Rechazado"),
                                         multiple=TRUE, options=list(`actions-box`=TRUE))),
                   column(4, dateRangeInput(ns("e_rango"), "Rango de compromiso"))
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        bslib::value_box(title="Pendientes", value=htmlOutput(ns("kpi_e_pend")), theme_color="indigo"),
                        bslib::value_box(title="En curso",   value=htmlOutput(ns("kpi_e_curso")), theme_color="teal")
                 ),
                 column(6,
                        bslib::value_box(title="Entregados", value=htmlOutput(ns("kpi_e_ent")),  theme_color="green"),
                        bslib::value_box(title="Aprobados",  value=htmlOutput(ns("kpi_e_aprob")), theme_color="blue")
                 )
               ),
               br(),
               bslib::card(bslib::card_header("Listado (filtrado)"), DTOutput(ns("tbl_ent"))),
               br(),
               bslib::card(
                 bslib::card_header("Gantt de entregables"),
                 plotOutput(ns("plot_gantt_ent"), height=380),
                 div(class="d-flex gap-2 flex-wrap",
                     downloadButton(ns("dl_ent_csv"), "CSV (filtrado)"),
                     downloadButton(ns("dl_ent_png"), "PNG Gantt"),
                     downloadButton(ns("dl_ent_pdf"), "PDF Gantt"))
               )
      ),
      
      tabPanel("RACI",
               br(),
               bslib::card(
                 bslib::card_header("Matriz RACI (solo lectura)"),
                 fluidRow(
                   column(6, textInput(ns("raci_busca"), "Buscar entregable", "")),
                   column(6, helpText("Origen: v_raci / raci / entregable_responsable (auto-fallback)."))
                 ),
                 DTOutput(ns("tbl_raci")),
                 br(),
                 div(class="d-flex gap-2 flex-wrap",
                     downloadButton(ns("dl_raci_csv"), "RACI (CSV)"),
                     downloadButton(ns("dl_raci_pdf"), "RACI (PDF)"))
               )
      ),
      
      tabPanel("Stakeholders",
               br(),
               bslib::card(bslib::card_header("Mapa de interesados (solo lectura)"), DTOutput(ns("tbl_stake")))
      ),
      
      tabPanel("Gantt",
               br(),
               bslib::card(bslib::card_header("Cronograma de fases"),
                           plotOutput(ns("plot_gantt_fases"), height=380),
                           DTOutput(ns("tbl_fases"))
               )
      ),
      
      tabPanel("Exportar",
               br(),
               bslib::card(
                 bslib::card_header("Descargas / Snapshots"),
                 textInput(ns("rep_name"), "Nombre base",
                           value=paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))),
                 checkboxInput(ns("snap_csv"), "Guardar snapshots CSV (prioridades/acciones/eventos)", TRUE),
                 div(class="d-flex gap-3 flex-wrap",
                     downloadButton(ns("dl_exec_pdf"), "Reporte ejecutivo (PDF)"),
                     downloadButton(ns("dl_pdf"),      "Reporte completo (PDF)"),
                     downloadButton(ns("dl_md"),       "Descargar .md"),
                     downloadButton(ns("dl_acc_fil_csv"), "Acciones filtradas (CSV)"),
                     downloadButton(ns("dl_prior_csv"),   "Prioridades (CSV)"),
                     downloadButton(ns("dl_evt_csv"),     "Eventos (CSV)"),
                     downloadButton(ns("dl_heat_png"),    "Heatmap (PNG)"))
               )
      )
    )
  )
}

pmp_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ---- utilidades seguras ----
    `%||%` <- function(x,y) if (is.null(x) || length(x)==0 || all(is.na(x))) y else x  # vector-safe
    qsafe <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error=function(e){ warning(e$message); data.frame() })
    qtry_first <- function(sqls){ for (s in sqls){ df <- qsafe(s); if (nrow(df)) return(df) }; data.frame() }
    
    # Estado conexión
    output$conn_status <- renderUI({
      ok <- TRUE; tryCatch(DBI::dbGetQuery(con,"SELECT 1"), error=function(e) ok <<- FALSE)
      if (ok) div(class="alert alert-success","Conexión MySQL OK.") else div(class="alert alert-danger","Sin conexión MySQL.")
    })
    
    # ---- fuentes de datos ----
    prior <- reactive({ qsafe("SELECT prioridad, COUNT(*) AS total FROM v_riesgo_registro GROUP BY prioridad") })
    acc   <- reactive({ qsafe("SELECT accion_id, riesgo_id, titulo, responsable, estado, fecha_compromiso, semaforo
                               FROM v_riesgo_acciones
                               ORDER BY FIELD(semaforo,'Vencida','Por vencer (≤7 días)','En curso','Cerrada'), fecha_compromiso") })
    heat  <- reactive({ qsafe("SELECT prob_nivel, imp_nivel, prioridad, color_hex FROM v_riesgo_heatmap") })
    evt   <- reactive({ qsafe("SELECT evento_id, riesgo_id, tipo, detalle, fecha_evento FROM riesgo_evento ORDER BY evento_id DESC LIMIT 50") })
    fases <- reactive({ qsafe("SELECT fase_id, nombre, fecha_inicio, fecha_fin FROM fase ORDER BY fase_id") })
    
    entregables_raw <- reactive({
      qtry_first(c(
        "SELECT e.entregable_id, CAST(e.nombre AS CHAR(255)) AS nombre, e.estado,
                e.fecha_compromiso, e.fecha_entrega, e.version, e.url_repositorio,
                a.nombre_completo AS responsable
           FROM entregable e
           LEFT JOIN autor a ON a.autor_id=e.responsable_id
          ORDER BY e.entregable_id DESC",
        "SELECT * FROM entregable ORDER BY 1 DESC"
      ))
    })
    
    stakeholders <- reactive({
      qtry_first(c(
        "SELECT stakeholder_id, nombre, rol, organizacion, email, telefono,
                influencia, interes, estrategia
           FROM stakeholder ORDER BY nombre",
        "SELECT * FROM stakeholder ORDER BY 1"
      ))
    })
    
    # ---- tablero ----
    kpi_pick <- function(df, keys) df %>% dplyr::filter(prioridad %in% keys) %>% dplyr::summarise(n=sum(total,na.rm=TRUE)) %>% dplyr::pull(n) %||% 0
    output$kpi_ext  <- renderText(kpi_pick(prior(), c("Extreme","Crítica","Critica")))
    output$kpi_high <- renderText(kpi_pick(prior(), c("High","Alta")))
    output$kpi_mid  <- renderText(kpi_pick(prior(), c("Medium","Media")))
    output$kpi_low  <- renderText(kpi_pick(prior(), c("Low","Baja")))
    
    output$heatmap <- renderPlot({
      h <- heat(); validate(need(nrow(h)>0,"Sin datos (v_riesgo_heatmap)."))
      ggplot2::ggplot(h, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
        ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
        ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size=14)
    })
    output$tbl_evt <- DT::renderDT({
      e <- evt(); if (nrow(e)) e$fecha_evento <- as.Date(e$fecha_evento)
      DT::datatable(e, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    # ---- riesgos (filtros) ----
    observe({
      a <- acc()
      updateSelectizeInput(session, "f_resp",
                           choices = if (nrow(a)) sort(unique(na.omit(a$responsable))) else character(0),
                           server = TRUE)
    })
    acc_fil <- reactive({
      df <- acc(); if (!nrow(df)) return(df)
      if (length(input$f_resp))   df <- df[df$responsable %in% input$f_resp, , drop=FALSE]
      if (length(input$f_estado)) df <- df[df$semaforo %in% input$f_estado | df$estado %in% input$f_estado, , drop=FALSE]
      if (!any(is.na(input$f_fecha))) {
        f1 <- as.Date(input$f_fecha[1]); f2 <- as.Date(input$f_fecha[2])
        df$fecha_compromiso <- as.Date(df$fecha_compromiso)
        df <- df[df$fecha_compromiso >= f1 & df$fecha_compromiso <= f2, , drop=FALSE]
      }
      df
    })
    output$tbl_prior <- DT::renderDT({
      df <- prior(); if (!nrow(df)) df <- data.frame(prioridad="(sin datos)", total=0)
      DT::datatable(df, options=list(pageLength=10), rownames=FALSE)
    })
    output$tbl_acc <- DT::renderDT({
      dat <- acc_fil(); if (nrow(dat)) dat$fecha_compromiso <- as.Date(dat$fecha_compromiso)
      DT::datatable(dat, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    # ---- entregables ----
    observe({
      e <- entregables_raw()
      updateSelectizeInput(session, "e_resp",
                           choices = if (nrow(e) && "responsable" %in% names(e)) sort(unique(na.omit(e$responsable))) else character(0),
                           server = TRUE)
    })
    ent_fil <- reactive({
      df <- entregables_raw(); if (!nrow(df)) return(df)
      if (!"responsable" %in% names(df)) df$responsable <- NA_character_
      if (!"estado"       %in% names(df)) df$estado       <- NA_character_
      if (!"fecha_compromiso" %in% names(df)) df$fecha_compromiso <- NA
      
      if (length(input$e_resp))   df <- df[df$responsable %in% input$e_resp, , drop=FALSE]
      if (length(input$e_estado)) df <- df[df$estado %in% input$e_estado, , drop=FALSE]
      if (!any(is.na(input$e_rango))) {
        f1 <- as.Date(input$e_rango[1]); f2 <- as.Date(input$e_rango[2])
        df$fecha_compromiso <- as.Date(df$fecha_compromiso)
        df <- df[df$fecha_compromiso >= f1 & df$fecha_compromiso <= f2, , drop=FALSE]
      }
      df
    })
    
    # KPIs entregables
    output$kpi_e_pend  <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("pendiente","pendientes"), na.rm=TRUE)) })
    output$kpi_e_curso <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("en curso","progreso"), na.rm=TRUE)) })
    output$kpi_e_ent   <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("entregado","entregada"), na.rm=TRUE)) })
    output$kpi_e_aprob <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("aprobado","aprobada"), na.rm=TRUE)) })
    
    output$tbl_ent <- DT::renderDT({
      df <- ent_fil()
      if ("fecha_compromiso" %in% names(df)) df$fecha_compromiso <- as.Date(df$fecha_compromiso)
      if ("fecha_entrega"    %in% names(df)) df$fecha_entrega    <- as.Date(df$fecha_entrega)
      DT::datatable(df, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    
    # --- Gantt robusto (sin %||% en vectores) ---
    .add_gantt_dates <- function(df){
      df$inicio <- as.Date(df$fecha_compromiso)
      if ("fecha_entrega" %in% names(df)) {
        df$fin <- as.Date(df$fecha_entrega)
        na_fin <- is.na(df$fin); if (any(na_fin)) df$fin[na_fin] <- df$inicio[na_fin]
      } else {
        df$fin <- df$inicio
      }
      df$nombre <- if ("nombre" %in% names(df)) df$nombre else paste("Entregable", seq_len(nrow(df)))
      df$nombre <- factor(df$nombre, levels = rev(df$nombre))
      df
    }
    
    output$plot_gantt_ent <- renderPlot({
      df <- ent_fil(); validate(need(nrow(df)>0,"Sin datos de entregables"))
      df <- .add_gantt_dates(df)
      ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
        ggplot2::geom_segment(ggplot2::aes(x=inicio, xend=fin, yend=nombre), linewidth=3) +
        ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
        ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
    })
    
    output$dl_ent_csv <- downloadHandler(
      filename=function(){ paste0("entregables_filtrados_", format(Sys.Date(), "%Y%m%d"), ".csv") },
      content=function(file){ readr::write_csv(ent_fil(), file) }
    )
    output$dl_ent_png <- downloadHandler(
      filename=function(){ paste0("gantt_entregables_", format(Sys.Date(), "%Y%m%d"), ".png") },
      content=function(file){
        df <- .add_gantt_dates(ent_fil()); validate(need(nrow(df)>0,"Sin datos"))
        p <- ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
          ggplot2::geom_segment(ggplot2::aes(x=inicio, xend=fin, yend=nombre), linewidth=3) +
          ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
          ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
        ggplot2::ggsave(file, plot=p, width=10, height=5, dpi=150)
      },
      contentType="image/png"
    )
    output$dl_ent_pdf <- downloadHandler(
      filename=function(){ paste0("gantt_entregables_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content=function(file){
        df <- .add_gantt_dates(ent_fil()); validate(need(nrow(df)>0,"Sin datos"))
        rmd <- tempfile(fileext=".Rmd")
        writeLines(con=rmd, text=paste0(
          "---\n","title: \"Gantt de Entregables\"\n","output: pdf_document\n","---\n\n",
          "```{r, echo=FALSE, message=FALSE}\n",
          "library(ggplot2); library(scales)\n",
          "d <- ", deparse(df), "\n",
          "ggplot(d, aes(y=nombre)) +\n",
          "  geom_segment(aes(x=inicio, xend=fin, yend=nombre), linewidth=3) +\n",
          "  scale_x_date(date_breaks='1 month', labels=scales::label_date('%Y-%m')) +\n",
          "  labs(x='Fecha', y=NULL) + theme_minimal()\n",
          "```\n"
        ))
        ok <- FALSE
        if (requireNamespace("tinytex", quietly=TRUE)) {
          try(rmarkdown::render(rmd, output_file=file, quiet=TRUE), silent=TRUE)
          ok <- file.exists(file)
        }
        if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
          html <- tempfile(fileext=".html")
          try(rmarkdown::render(rmd, output_file=basename(html), output_dir=dirname(html), quiet=TRUE), silent=TRUE)
          try(pagedown::chrome_print(input=html, output=file, verbose=0), silent=TRUE)
        }
      },
      contentType="application/pdf"
    )
    
    # ---- RACI ----
    raci_direct <- reactive({
      qtry_first(c(
        "SELECT * FROM v_raci",
        "SELECT e.entregable_id, e.nombre AS entregable, r.resp_R AS R, r.resp_A AS A, r.resp_C AS C, r.resp_I AS I
           FROM raci r JOIN entregable e ON e.entregable_id=r.entregable_id"
      ))
    })
    raci_from_rel <- reactive({
      qsafe("
        SELECT e.entregable_id, CAST(e.nombre AS CHAR(255)) AS entregable,
               GROUP_CONCAT(CASE WHEN er.rol='R' THEN a.nombre_completo END SEPARATOR '; ') AS R,
               GROUP_CONCAT(CASE WHEN er.rol='A' THEN a.nombre_completo END SEPARATOR '; ') AS A,
               GROUP_CONCAT(CASE WHEN er.rol='C' THEN a.nombre_completo END SEPARATOR '; ') AS C,
               GROUP_CONCAT(CASE WHEN er.rol='I' THEN a.nombre_completo END SEPARATOR '; ') AS I
          FROM entregable_responsable er
          JOIN entregable e ON e.entregable_id=er.entregable_id
          LEFT JOIN autor a ON a.autor_id=er.autor_id
         GROUP BY e.entregable_id, e.nombre
         ORDER BY e.entregable_id DESC")
    })
    raci_df <- reactive({
      df <- raci_direct(); if (nrow(df)) return(df)
      df <- raci_from_rel(); if (nrow(df)) return(df)
      e <- entregables_raw(); if (!nrow(e)) return(data.frame())
      out <- unique(e[, c("entregable_id","nombre")]); names(out) <- c("entregable_id","entregable")
      out$R <- NA_character_; out$A <- NA_character_; out$C <- NA_character_; out$I <- NA_character_
      out
    })
    raci_fil <- reactive({
      df <- raci_df(); if (!nrow(df)) return(df)
      txt <- trimws(input$raci_busca %||% "")
      if (nzchar(txt)) df <- df[grepl(txt, df$entregable, ignore.case=TRUE), , drop=FALSE]
      df
    })
    output$tbl_raci <- DT::renderDT({
      df <- raci_fil()
      cols <- intersect(c("entregable","R","A","C","I"), names(df))
      if (length(cols)) df <- df[, unique(c(cols, setdiff(names(df), cols))), drop=FALSE]
      DT::datatable(df, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    output$dl_raci_csv <- downloadHandler(
      filename=function(){ paste0("raci_", format(Sys.Date(), "%Y%m%d"), ".csv") },
      content=function(file){ readr::write_csv(raci_fil(), file) }
    )
    output$dl_raci_pdf <- downloadHandler(
      filename=function(){ paste0("raci_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content=function(file){
        df <- raci_fil()
        rmd <- tempfile(fileext=".Rmd")
        writeLines(con=rmd, text=paste0(
          "---\n","title: \"Matriz RACI\"\n","output: pdf_document\n","---\n\n",
          "```{r, echo=FALSE}\nknitr::kable(", deparse(df), ", format='latex', booktabs=TRUE, longtable=TRUE)\n```\n"
        ))
        ok <- FALSE
        if (requireNamespace("tinytex", quietly=TRUE)) {
          try(rmarkdown::render(rmd, output_file=file, quiet=TRUE), silent=TRUE)
          ok <- file.exists(file)
        }
        if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
          html <- tempfile(fileext=".html")
          try(rmarkdown::render(rmd, output_file=basename(html), output_dir=dirname(html), quiet=TRUE), silent=TRUE)
          try(pagedown::chrome_print(input=html, output=file, verbose=0), silent=TRUE)
        }
      },
      contentType="application/pdf"
    )
    
    # ---- Stakeholders ----
    output$tbl_stake <- DT::renderDT({
      s <- stakeholders()
      DT::datatable(s, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    
    # ---- Gantt Fases ----
    output$plot_gantt_fases <- renderPlot({
      df <- fases(); validate(need(nrow(df)>0,"Sin datos de fases"))
      df$nombre <- factor(df$nombre, levels=rev(df$nombre))
      ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
        ggplot2::geom_segment(ggplot2::aes(x=as.Date(fecha_inicio), xend=as.Date(fecha_fin), yend=nombre), linewidth=3) +
        ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
        ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
    })
    output$tbl_fases <- DT::renderDT({
      DT::datatable(fases(), options=list(pageLength=12), rownames=FALSE)
    })
    
    # ---- Exportes generales ----
    build_md <- function(prior, acc, evt){
      lines <- c(
        paste0("# Reporte PMP - ", format(Sys.Date(), "%Y-%m-%d")), "",
        "## Resumen por prioridad",
        "| Prioridad | Total |", "|---|---|",
        if (nrow(prior)) apply(prior, 1, function(r) paste0("| ", r[['prioridad']], " | ", r[['total']], " |")) else "| (sin datos) | 0 |",
        "", "## Acciones (semáforo)",
        if (nrow(acc)) knitr::kable(acc, format = "pipe") else "_Sin acciones disponibles_",
        "", "## Eventos recientes",
        if (nrow(evt)) knitr::kable(evt, format = "pipe") else "_Sin eventos recientes_",
        "", "_Generado desde Shiny_"
      )
      paste(lines, collapse="\n")
    }
    output$dl_md <- downloadHandler(
      filename=function(){
        base <- if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))
        paste0(base, ".md")
      },
      content=function(file){ writeLines(build_md(prior(), acc_fil(), evt()), file, useBytes=TRUE) }
    )
    
    .make_pdf <- function(prior_df, acc_df, evt_df, heat_df, file_pdf){
      rmd <- tempfile(fileext = ".Rmd")
      png_heat <- tempfile(fileext = ".png")
      if (nrow(heat_df)) {
        p <- ggplot2::ggplot(heat_df, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
          ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
          ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size = 14)
        ggplot2::ggsave(filename=png_heat, plot=p, width=7, height=5, dpi=150)
      }
      writeLines(con=rmd, text=paste0(
        "---\n","title: \"Reporte PMP\"\n","date: \"", format(Sys.Date(), "%Y-%m-%d"), "\"\n",
        "output: pdf_document\n","params:\n  prior: !r NULL\n  acc: !r NULL\n  evt: !r NULL\n  heat_png: !r NULL\n","---\n\n",
        "```{r setup, include=FALSE}\nknitr::opts_chunk$set(message=FALSE, warning=FALSE); library(knitr)\n```\n\n",
        "## Resumen por prioridad\n\n","```{r, echo=FALSE}\nif (nrow(params$prior)) kable(params$prior, format='latex', booktabs=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Acciones (semáforo)\n\n","```{r, echo=FALSE}\nif (nrow(params$acc)) kable(params$acc, format='latex', booktabs=TRUE, longtable=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Eventos recientes\n\n","```{r, echo=FALSE}\nif (nrow(params$evt)) kable(params$evt, format='latex', booktabs=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Heatmap Prob × Impacto\n\n","```{r, echo=FALSE, out.width='80%'}\nif (!is.null(params$heat_png) && file.exists(params$heat_png)) knitr::include_graphics(params$heat_png) else cat('_Sin gráfico de heatmap._')\n```\n"
      ))
      ok <- FALSE
      if (requireNamespace("tinytex", quietly=TRUE)) {
        try(rmarkdown::render(input=rmd, output_file=file_pdf,
                              params=list(prior=prior_df, acc=acc_df, evt=evt_df, heat_png=if (file.exists(png_heat)) png_heat else NULL),
                              envir=new.env(parent=globalenv()), quiet=TRUE), silent=TRUE)
        ok <- file.exists(file_pdf)
      }
      if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
        html <- tempfile(fileext=".html")
        try(rmarkdown::render(input=rmd, output_file=basename(html), output_dir=dirname(html),
                              params=list(prior=prior_df, acc=acc_df, evt=evt_df, heat_png=if (file.exists(png_heat)) png_heat else NULL),
                              envir=new.env(parent=globalenv()), quiet=TRUE), silent=TRUE)
        try(pagedown::chrome_print(input=html, output=file_pdf, verbose=0), silent=TRUE)
        ok <- file.exists(file_pdf)
      }
      ok
    }
    output$dl_pdf <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")), ".pdf") },
      content=function(file){ ok <- .make_pdf(prior(), acc(), evt(), heat(), file); if (!ok) showNotification("No se pudo generar el PDF (instala tinytex o pagedown).", type="error", duration=8) },
      contentType="application/pdf"
    )
    output$dl_exec_pdf <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")), "_ejecutivo.pdf") },
      content=function(file){ ok <- .make_pdf(prior(), acc_fil(), evt(), heat(), file); if (!ok) showNotification("No se pudo generar el PDF (instala tinytex o pagedown).", type="error", duration=8) },
      contentType="application/pdf"
    )
    output$dl_acc_fil_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_acciones_filtradas.csv") },
      content=function(file){ readr::write_csv(acc_fil(), file) }
    )
    output$dl_prior_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_prioridades.csv") },
      content=function(file){ readr::write_csv(prior(), file) }
    )
    output$dl_evt_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_eventos.csv") },
      content=function(file){ readr::write_csv(evt(), file) }
    )
    output$dl_heat_png <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_heatmap.png") },
      content=function(file){
        h <- heat(); validate(need(nrow(h)>0, "Sin datos de heatmap"))
        p <- ggplot2::ggplot(h, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
          ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
          ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size=14)
        ggplot2::ggsave(file, plot=p, width=8, height=5, dpi=150)
      },
      contentType="image/png"
    )
    
    # ---- Snapshots CSV (rápidos) ----
    observeEvent(input$snap_csv, ignoreInit=TRUE, {
      if (isTRUE(input$snap_csv)) {
        base <- if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))
        outdir <- normalizePath(file.path(getwd(),"PMP","Riesgos","reportes"), mustWork=FALSE)
        if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
        readr::write_csv(prior(), file.path(outdir, paste0(base,"_prioridades.csv")))
        readr::write_csv(acc(),   file.path(outdir, paste0(base,"_acciones.csv")))
        readr::write_csv(evt(),   file.path(outdir, paste0(base,"_eventos.csv")))
        showNotification(paste("Snapshots CSV guardados en", outdir), type="message", duration=6)
      }
    })
  })
}

# ======================= MODULO PMP (UI + SERVER) =======================

# ------------------------------ UI -------------------------------------
pmp_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("conn_status")),
    tabsetPanel(
      tabPanel("Tablero",
               br(),
               fluidRow(
                 column(3, bslib::value_box(title="Extreme/Crítica", value=htmlOutput(ns("kpi_ext")),  theme_color="#b71c1c")),
                 column(3, bslib::value_box(title="Alta",            value=htmlOutput(ns("kpi_high")), theme_color="#e65100")),
                 column(3, bslib::value_box(title="Media",           value=htmlOutput(ns("kpi_mid")),  theme_color="#fdd835")),
                 column(3, bslib::value_box(title="Baja",            value=htmlOutput(ns("kpi_low")),  theme_color="#4a3047"))
               ),
               br(),
               fluidRow(
                 column(6, bslib::card(bslib::card_header("Heatmap Prob × Impacto"),
                                       plotOutput(ns("heatmap"), height=420))),
                 column(6, bslib::card(bslib::card_header("Eventos recientes"),
                                       DTOutput(ns("tbl_evt"))))
               )
      ),
      
      tabPanel("Riesgos",
               br(),
               bslib::card(
                 bslib::card_header("Filtros"),
                 fluidRow(
                   column(4, selectizeInput(ns("f_resp"), "Responsable", choices=NULL, multiple=TRUE,
                                            options=list(placeholder="Todos"))),
                   column(4, shinyWidgets::pickerInput(ns("f_estado"), "Semáforo / Estado",
                                                       choices=c("Vencida","Por vencer (≤7 días)","En curso","Cerrada"),
                                                       multiple=TRUE, options=list(`actions-box`=TRUE))),
                   column(4, dateRangeInput(ns("f_fecha"), "Rango fecha compromiso", start=NA, end=NA))
                 )
               ),
               br(),
               fluidRow(
                 column(5, bslib::card(bslib::card_header("Resumen por prioridad"), DTOutput(ns("tbl_prior")))),
                 column(7, bslib::card(bslib::card_header("Acciones (filtradas)"),   DTOutput(ns("tbl_acc"))))
               )
      ),
      
      tabPanel("Entregables",
               br(),
               bslib::card(
                 bslib::card_header("Filtros de entregables"),
                 fluidRow(
                   column(4, selectizeInput(ns("e_resp"), "Responsable", choices=NULL, multiple=TRUE,
                                            options=list(placeholder="Todos"))),
                   column(4, shinyWidgets::pickerInput(ns("e_estado"), "Estado",
                                                       choices=c("Pendiente","En curso","Entregado","Aprobado","Rechazado"),
                                                       multiple=TRUE, options=list(`actions-box`=TRUE))),
                   column(4, dateRangeInput(ns("e_rango"), "Rango de compromiso", start=NA, end=NA))
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        bslib::value_box(title="Pendientes", value=htmlOutput(ns("kpi_e_pend")), theme_color="indigo"),
                        bslib::value_box(title="En curso",   value=htmlOutput(ns("kpi_e_curso")), theme_color="teal")
                 ),
                 column(6,
                        bslib::value_box(title="Entregados", value=htmlOutput(ns("kpi_e_ent")), theme_color="green"),
                        bslib::value_box(title="Aprobados",  value=htmlOutput(ns("kpi_e_aprob")), theme_color="blue")
                 )
               ),
               br(),
               bslib::card(bslib::card_header("Listado (filtrado)"), DTOutput(ns("tbl_ent"))),
               br(),
               bslib::card(
                 bslib::card_header("Gantt de entregables"),
                 plotOutput(ns("plot_gantt_ent"), height=380),
                 div(class="d-flex gap-2 flex-wrap",
                     downloadButton(ns("dl_ent_csv"),  "CSV (filtrado)"),
                     downloadButton(ns("dl_ent_png"),  "PNG Gantt"),
                     downloadButton(ns("dl_ent_pdf"),  "PDF Gantt")
                 )
               )
      ),
      
      tabPanel("RACI",
               br(),
               bslib::card(
                 bslib::card_header("Matriz RACI (solo lectura)"),
                 fluidRow(
                   column(6, textInput(ns("raci_busca"), "Buscar entregable", "")),
                   column(6, helpText("Se arma desde v_raci / raci / entregable_responsable (lo que exista)."))
                 ),
                 DTOutput(ns("tbl_raci")),
                 br(),
                 div(class="d-flex gap-2 flex-wrap",
                     downloadButton(ns("dl_raci_csv"), "RACI (CSV)"),
                     downloadButton(ns("dl_raci_pdf"), "RACI (PDF)")
                 )
               )
      ),
      
      tabPanel("Stakeholders",
               br(),
               bslib::card(bslib::card_header("Mapa de interesados (solo lectura)"),
                           DTOutput(ns("tbl_stake")))
      ),
      
      tabPanel("Gantt",
               br(),
               bslib::card(bslib::card_header("Cronograma de fases"),
                           plotOutput(ns("plot_gantt_fases"), height=380),
                           DTOutput(ns("tbl_fases")))
      ),
      
      tabPanel("Exportar",
               br(),
               bslib::card(
                 bslib::card_header("Descargas / Snapshots"),
                 textInput(ns("rep_name"), "Nombre base",
                           value=paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))),
                 checkboxInput(ns("snap_csv"), "Guardar snapshots CSV (prioridades/acciones/eventos)", TRUE),
                 div(class="d-flex gap-3 flex-wrap",
                     downloadButton(ns("dl_exec_pdf"), "Reporte ejecutivo (PDF)"),
                     downloadButton(ns("dl_pdf"),      "Reporte completo (PDF)"),
                     downloadButton(ns("dl_md"),       "Descargar .md"),
                     downloadButton(ns("dl_acc_fil_csv"), "Acciones filtradas (CSV)"),
                     downloadButton(ns("dl_prior_csv"),   "Prioridades (CSV)"),
                     downloadButton(ns("dl_evt_csv"),     "Eventos (CSV)"),
                     downloadButton(ns("dl_heat_png"),    "Heatmap (PNG)")
                 )
               )
      )
    )
  )
}

# ---------------------------- SERVER -----------------------------------
pmp_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(x,y) if (is.null(x) || length(x)==0 || is.na(x)) y else x
    qsafe  <- function(sql)  tryCatch(DBI::dbGetQuery(con, sql), error=function(e){ warning(e$message); data.frame() })
    qsafe0 <- function(sql)  tryCatch(DBI::dbGetQuery(con, sql), error=function(e) data.frame())
    table_exists <- function(tbl){
      out <- tryCatch(DBI::dbGetQuery(con, sprintf("SHOW TABLES LIKE '%s'", tbl)),
                      error = function(e) data.frame())
      nrow(out) > 0
    }
    
    # Estado conexión
    output$conn_status <- renderUI({
      ok <- TRUE; tryCatch(DBI::dbGetQuery(con,"SELECT 1"), error=function(e) ok <<- FALSE)
      if (isTRUE(ok)) div(class="alert alert-success","Conexión MySQL OK.")
      else            div(class="alert alert-danger","Sin conexión MySQL.")
    })
    
    # --- Datos base ---
    prior <- reactive({ qsafe("SELECT prioridad, COUNT(*) AS total FROM v_riesgo_registro GROUP BY prioridad") })
    acc   <- reactive({ qsafe("SELECT accion_id, riesgo_id, titulo, responsable, estado, fecha_compromiso, semaforo
                               FROM v_riesgo_acciones
                               ORDER BY FIELD(semaforo,'Vencida','Por vencer (≤7 días)','En curso','Cerrada'), fecha_compromiso") })
    heat  <- reactive({ qsafe("SELECT prob_nivel, imp_nivel, prioridad, color_hex FROM v_riesgo_heatmap") })
    evt   <- reactive({ qsafe("SELECT evento_id, riesgo_id, tipo, detalle, fecha_evento FROM riesgo_evento ORDER BY evento_id DESC LIMIT 50") })
    fases <- reactive({ qsafe("SELECT fase_id, nombre, fecha_inicio, fecha_fin FROM fase ORDER BY fase_id") })
    
    # Entregables (dos rutas)
    qtry_first <- function(sqls){
      for (s in sqls){ df <- qsafe(s); if (nrow(df)) return(df) }
      data.frame()
    }
    entregables_raw <- reactive({
      qtry_first(c(
        "SELECT e.entregable_id, CAST(e.nombre AS CHAR(255)) AS nombre, e.estado,
                e.fecha_compromiso, e.fecha_entrega, e.version, e.url_repositorio,
                a.nombre_completo AS responsable
         FROM entregable e LEFT JOIN autor a ON a.autor_id=e.responsable_id
         ORDER BY e.entregable_id DESC",
        "SELECT * FROM entregable ORDER BY 1 DESC"
      ))
    })
    
    # Stakeholders (silencioso)
    stakeholders <- reactive({
      if (!table_exists("stakeholder")) return(data.frame())
      qsafe0("SELECT stakeholder_id, nombre, rol, organizacion, email, telefono,
                     influencia, interes, estrategia
              FROM stakeholder ORDER BY nombre")
    })
    
    # KPIs tablero
    output$kpi_ext  <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Extreme","Crítica","Critica")) %>% dplyr::summarise(n=sum(total,na.rm=TRUE)) %>% dplyr::pull(n); format(n %||% 0) })
    output$kpi_high <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("High","Alta"))   %>% dplyr::summarise(n=sum(total,na.rm=TRUE)) %>% dplyr::pull(n); format(n %||% 0) })
    output$kpi_mid  <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Medium","Media"))%>% dplyr::summarise(n=sum(total,na.rm=TRUE)) %>% dplyr::pull(n); format(n %||% 0) })
    output$kpi_low  <- renderText({ df<-prior(); n<-df %>% dplyr::filter(prioridad %in% c("Low","Baja"))   %>% dplyr::summarise(n=sum(total,na.rm=TRUE)) %>% dplyr::pull(n); format(n %||% 0) })
    
    output$heatmap <- renderPlot({
      h <- heat(); validate(need(nrow(h)>0,"Sin datos (v_riesgo_heatmap)."))
      ggplot2::ggplot(h, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
        ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
        ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size = 14)
    })
    output$tbl_evt <- DT::renderDT({
      e <- evt(); if (nrow(e)) e$fecha_evento <- as.Date(e$fecha_evento)
      DT::datatable(e, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    # ====== RIESGOS ======
    observe({
      a <- acc()
      updateSelectizeInput(session, "f_resp",
                           choices = if (nrow(a)) sort(unique(na.omit(a$responsable))) else character(0),
                           server = TRUE)
    })
    acc_fil <- reactive({
      df <- acc(); if (!nrow(df)) return(df)
      if (!is.null(input$f_resp) && length(input$f_resp))   df <- df[df$responsable %in% input$f_resp, , drop=FALSE]
      if (!is.null(input$f_estado) && length(input$f_estado))df <- df[df$semaforo %in% input$f_estado | df$estado %in% input$f_estado, , drop=FALSE]
      if (!any(is.na(input$f_fecha))) {
        f1 <- as.Date(input$f_fecha[1]); f2 <- as.Date(input$f_fecha[2])
        df$fecha_compromiso <- as.Date(df$fecha_compromiso)
        df <- df[df$fecha_compromiso >= f1 & df$fecha_compromiso <= f2, , drop=FALSE]
      }
      df
    })
    output$tbl_prior <- DT::renderDT({
      df <- prior(); if (!nrow(df)) df <- data.frame(prioridad="(sin datos)", total=0)
      DT::datatable(df, options=list(pageLength=10), rownames=FALSE)
    })
    output$tbl_acc <- DT::renderDT({
      dat <- acc_fil(); if (nrow(dat)) dat$fecha_compromiso <- as.Date(dat$fecha_compromiso)
      DT::datatable(dat, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })
    
    # ====== ENTREGABLES ======
    observe({
      e <- entregables_raw()
      updateSelectizeInput(session, "e_resp",
                           choices = if (nrow(e) && "responsable" %in% names(e)) sort(unique(na.omit(e$responsable))) else character(0),
                           server = TRUE)
    })
    
    ent_fil <- reactive({
      df <- entregables_raw(); if (!nrow(df)) return(df)
      if (!"responsable" %in% names(df))       df$responsable       <- NA_character_
      if (!"estado" %in% names(df))            df$estado            <- NA_character_
      if (!"fecha_compromiso" %in% names(df))  df$fecha_compromiso  <- NA
      if (!is.null(input$e_resp)   && length(input$e_resp))   df <- df[df$responsable %in% input$e_resp, , drop=FALSE]
      if (!is.null(input$e_estado) && length(input$e_estado)) df <- df[df$estado %in% input$e_estado, , drop=FALSE]
      if (!any(is.na(input$e_rango))) {
        f1 <- as.Date(input$e_rango[1]); f2 <- as.Date(input$e_rango[2])
        df$fecha_compromiso <- as.Date(df$fecha_compromiso)
        df <- df[df$fecha_compromiso >= f1 & df$fecha_compromiso <= f2, , drop=FALSE]
      }
      df
    })
    
    output$kpi_e_pend  <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("pendiente","pendientes"), na.rm=TRUE)) })
    output$kpi_e_curso <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("en curso","progreso"), na.rm=TRUE)) })
    output$kpi_e_ent   <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("entregado","entregada"), na.rm=TRUE)) })
    output$kpi_e_aprob <- renderText({ df<-ent_fil(); format(sum(tolower(df$estado) %in% c("aprobado","aprobada"), na.rm=TRUE)) })
    
    # --- Gantt robusto (filtra fechas NA) ---
    .add_gantt_dates <- function(df){
      df$inicio <- suppressWarnings(as.Date(df$fecha_compromiso))
      if ("fecha_entrega" %in% names(df)) df$fin <- suppressWarnings(as.Date(df$fecha_entrega)) else df$fin <- NA
      na_fin <- is.na(df$fin); df$fin[na_fin] <- df$inicio[na_fin]
      df <- df[!is.na(df$inicio) & !is.na(df$fin), , drop = FALSE]
      df$nombre <- if ("nombre" %in% names(df)) df$nombre else paste("Entregable", seq_len(nrow(df)))
      df$nombre <- factor(df$nombre, levels = rev(df$nombre))
      df
    }
    
    output$tbl_ent <- DT::renderDT({
      df <- ent_fil()
      if ("fecha_compromiso" %in% names(df)) df$fecha_compromiso <- as.Date(df$fecha_compromiso)
      if ("fecha_entrega"    %in% names(df)) df$fecha_entrega    <- as.Date(df$fecha_entrega)
      DT::datatable(df, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    
    output$plot_gantt_ent <- renderPlot({
      df <- .add_gantt_dates(ent_fil())
      validate(need(nrow(df) > 0, "Sin datos de entregables con fechas válidas"))
      ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
        ggplot2::geom_segment(ggplot2::aes(x=inicio, xend=fin, yend=nombre), linewidth=3) +
        ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
        ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
    })
    
    output$dl_ent_csv <- downloadHandler(
      filename=function(){ paste0("entregables_filtrados_", format(Sys.Date(), "%Y%m%d"), ".csv") },
      content=function(file){ readr::write_csv(ent_fil(), file) }
    )
    output$dl_ent_png <- downloadHandler(
      filename=function(){ paste0("gantt_entregables_", format(Sys.Date(), "%Y%m%d"), ".png") },
      content=function(file){
        df <- .add_gantt_dates(ent_fil())
        validate(need(nrow(df) > 0, "Sin datos de entregables con fechas válidas"))
        p <- ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
          ggplot2::geom_segment(ggplot2::aes(x=inicio, xend=fin, yend=nombre), linewidth=3) +
          ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
          ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
        ggplot2::ggsave(file, plot=p, width=10, height=5, dpi=150)
      },
      contentType="image/png"
    )
    output$dl_ent_pdf <- downloadHandler(
      filename=function(){ paste0("gantt_entregables_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content=function(file){
        df <- .add_gantt_dates(ent_fil())
        validate(need(nrow(df) > 0, "Sin datos de entregables con fechas válidas"))
        rmd <- tempfile(fileext=".Rmd")
        writeLines(con=rmd, text=paste0(
          "---\n","title: \"Gantt de Entregables\"\n","output: pdf_document\n","---\n\n",
          "```{r, echo=FALSE, message=FALSE}\n",
          "library(ggplot2); library(scales)\n",
          "d <- ", deparse(df), "\n",
          "ggplot(d, aes(y = nombre)) +\n",
          "  geom_segment(aes(x = inicio, xend = fin, yend = nombre), linewidth = 3) +\n",
          "  scale_x_date(date_breaks = '1 month', labels = label_date('%Y-%m')) +\n",
          "  labs(x='Fecha', y=NULL) + theme_minimal()\n",
          "```\n"
        ))
        ok <- FALSE
        if (requireNamespace("tinytex", quietly=TRUE)) {
          try(rmarkdown::render(rmd, output_file=file, quiet=TRUE), silent=TRUE)
          ok <- file.exists(file)
        }
        if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
          html <- tempfile(fileext=".html")
          try(rmarkdown::render(rmd, output_file=basename(html), output_dir=dirname(html), quiet=TRUE), silent=TRUE)
          try(pagedown::chrome_print(input=html, output=file, verbose=0), silent=TRUE)
        }
      },
      contentType="application/pdf"
    )
    
    # ====== RACI ======
    raci_direct <- reactive({
      if (table_exists("v_raci")) return(qsafe0("SELECT * FROM v_raci"))
      if (table_exists("raci"))   return(qsafe0("
        SELECT e.entregable_id, e.nombre AS entregable, r.resp_R AS R, r.resp_A AS A, r.resp_C AS C, r.resp_I AS I
        FROM raci r JOIN entregable e ON e.entregable_id=r.entregable_id"))
      data.frame()
    })
    raci_from_rel <- reactive({
      if (!table_exists("entregable_responsable")) return(data.frame())
      qsafe0("
        SELECT e.entregable_id, CAST(e.nombre AS CHAR(255)) AS entregable,
               GROUP_CONCAT(CASE WHEN er.rol='R' THEN a.nombre_completo END SEPARATOR '; ') AS R,
               GROUP_CONCAT(CASE WHEN er.rol='A' THEN a.nombre_completo END SEPARATOR '; ') AS A,
               GROUP_CONCAT(CASE WHEN er.rol='C' THEN a.nombre_completo END SEPARATOR '; ') AS C,
               GROUP_CONCAT(CASE WHEN er.rol='I' THEN a.nombre_completo END SEPARATOR '; ') AS I
        FROM entregable_responsable er
        JOIN entregable e ON e.entregable_id=er.entregable_id
        LEFT JOIN autor a ON a.autor_id=er.autor_id
        GROUP BY e.entregable_id, e.nombre
        ORDER BY e.entregable_id DESC")
    })
    raci_df <- reactive({
      df <- raci_direct(); if (nrow(df)) return(df)
      df <- raci_from_rel(); if (nrow(df)) return(df)
      e <- entregables_raw(); if (!nrow(e)) return(data.frame())
      out <- unique(e[, c("entregable_id","nombre")]); names(out) <- c("entregable_id","entregable")
      out$R <- out$A <- out$C <- out$I <- NA_character_; out
    })
    raci_fil <- reactive({
      df <- raci_df(); if (!nrow(df)) return(df)
      txt <- trimws(input$raci_busca %||% "")
      if (nzchar(txt)) df <- df[grepl(txt, df$entregable, ignore.case=TRUE), , drop=FALSE]
      df
    })
    output$tbl_raci <- DT::renderDT({
      df <- raci_fil()
      cols <- intersect(c("entregable","R","A","C","I"), names(df))
      if (length(cols)) df <- df[, unique(c(cols, setdiff(names(df), cols))), drop=FALSE]
      DT::datatable(df, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    output$dl_raci_csv <- downloadHandler(
      filename=function(){ paste0("raci_", format(Sys.Date(), "%Y%m%d"), ".csv") },
      content=function(file){ readr::write_csv(raci_fil(), file) }
    )
    output$dl_raci_pdf <- downloadHandler(
      filename=function(){ paste0("raci_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
      content=function(file){
        df <- raci_fil()
        rmd <- tempfile(fileext=".Rmd")
        writeLines(con=rmd, text=paste0(
          "---\n","title: \"Matriz RACI\"\n","output: pdf_document\n","---\n\n",
          "```{r, echo=FALSE}\nknitr::kable(", deparse(df), ", format='latex', booktabs=TRUE, longtable=TRUE)\n```\n"
        ))
        ok <- FALSE
        if (requireNamespace("tinytex", quietly=TRUE)) {
          try(rmarkdown::render(rmd, output_file=file, quiet=TRUE), silent=TRUE)
          ok <- file.exists(file)
        }
        if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
          html <- tempfile(fileext=".html")
          try(rmarkdown::render(rmd, output_file=basename(html), output_dir=dirname(html), quiet=TRUE), silent=TRUE)
          try(pagedown::chrome_print(input=html, output=file, verbose=0), silent=TRUE)
        }
      },
      contentType="application/pdf"
    )
    
    # ====== STAKEHOLDERS ======
    output$tbl_stake <- DT::renderDT({
      s <- stakeholders()
      DT::datatable(s, options=list(pageLength=12, scrollX=TRUE), rownames=FALSE)
    })
    
    # ====== GANTT FASES ======
    output$plot_gantt_fases <- renderPlot({
      df <- fases(); validate(need(nrow(df)>0,"Sin datos de fases"))
      df$nombre <- factor(df$nombre, levels=rev(df$nombre))
      ggplot2::ggplot(df, ggplot2::aes(y=nombre)) +
        ggplot2::geom_segment(ggplot2::aes(x=as.Date(fecha_inicio), xend=as.Date(fecha_fin), yend=nombre), linewidth=3) +
        ggplot2::scale_x_date(date_breaks="1 month", labels=scales::label_date("%Y-%m")) +
        ggplot2::labs(x="Fecha", y=NULL) + ggplot2::theme_minimal()
    })
    output$tbl_fases <- DT::renderDT({
      DT::datatable(fases(), options=list(pageLength=12), rownames=FALSE)
    })
    
    # ====== EXPORTES GENERALES ======
    build_md <- function(prior, acc, evt){
      lines <- c(
        paste0("# Reporte PMP - ", format(Sys.Date(), "%Y-%m-%d")), "",
        "## Resumen por prioridad",
        "| Prioridad | Total |", "|---|---|",
        if (nrow(prior)) apply(prior, 1, function(r) paste0("| ", r[['prioridad']], " | ", r[['total']], " |"))
        else "| (sin datos) | 0 |",
        "", "## Acciones (semáforo)",
        if (nrow(acc)) knitr::kable(acc, format = "pipe") else "_Sin acciones disponibles_",
        "", "## Eventos recientes",
        if (nrow(evt)) knitr::kable(evt, format = "pipe") else "_Sin eventos recientes_",
        "", "_Generado desde Shiny_"
      )
      paste(lines, collapse="\n")
    }
    output$dl_md <- downloadHandler(
      filename = function(){
        base <- if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))
        paste0(base, ".md")
      },
      content = function(file){
        writeLines(build_md(prior(), acc_fil(), evt()), file, useBytes=TRUE)
      }
    )
    
    .make_pdf <- function(prior_df, acc_df, evt_df, heat_df, file_pdf){
      rmd <- tempfile(fileext = ".Rmd")
      png_heat <- tempfile(fileext = ".png")
      if (nrow(heat_df)) {
        p <- ggplot2::ggplot(heat_df, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
          ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
          ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size = 14)
        ggplot2::ggsave(filename=png_heat, plot=p, width=7, height=5, dpi=150)
      }
      writeLines(con=rmd, text=paste0(
        "---\n","title: \"Reporte PMP\"\n","date: \"", format(Sys.Date(), "%Y-%m-%d"), "\"\n",
        "output: pdf_document\n",
        "params:\n  prior: !r NULL\n  acc: !r NULL\n  evt: !r NULL\n  heat_png: !r NULL\n",
        "---\n\n",
        "```{r setup, include=FALSE}\nknitr::opts_chunk$set(message=FALSE, warning=FALSE); library(knitr)\n```\n\n",
        "## Resumen por prioridad\n\n",
        "```{r, echo=FALSE}\nif (nrow(params$prior)) kable(params$prior, format='latex', booktabs=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Acciones (semáforo)\n\n",
        "```{r, echo=FALSE}\nif (nrow(params$acc)) kable(params$acc, format='latex', booktabs=TRUE, longtable=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Eventos recientes\n\n",
        "```{r, echo=FALSE}\nif (nrow(params$evt)) kable(params$evt, format='latex', booktabs=TRUE) else cat('_Sin datos disponibles._')\n```\n\n",
        "## Heatmap Prob × Impacto\n\n",
        "```{r, echo=FALSE, out.width='80%'}\nif (!is.null(params$heat_png) && file.exists(params$heat_png)) knitr::include_graphics(params$heat_png) else cat('_Sin gráfico de heatmap._')\n```\n"
      ))
      ok <- FALSE
      if (requireNamespace("tinytex", quietly=TRUE)) {
        try(rmarkdown::render(input=rmd, output_file=file_pdf,
                              params=list(prior=prior_df, acc=acc_df, evt=evt_df,
                                          heat_png=if (file.exists(png_heat)) png_heat else NULL),
                              envir=new.env(parent=globalenv()), quiet=TRUE), silent=TRUE)
        ok <- file.exists(file_pdf)
      }
      if (!ok && requireNamespace("pagedown", quietly=TRUE)) {
        html <- tempfile(fileext=".html")
        try(rmarkdown::render(input=rmd, output_file=basename(html), output_dir=dirname(html),
                              params=list(prior=prior_df, acc=acc_df, evt=evt_df,
                                          heat_png=if (file.exists(png_heat)) png_heat else NULL),
                              envir=new.env(parent=globalenv()), quiet=TRUE), silent=TRUE)
        try(pagedown::chrome_print(input=html, output=file_pdf, verbose=0), silent=TRUE)
        ok <- file.exists(file_pdf)
      }
      ok
    }
    
    output$dl_pdf <- downloadHandler(
      filename = function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")), ".pdf") },
      content  = function(file){
        ok <- .make_pdf(prior(), acc(), evt(), heat(), file)
        if (!ok) showNotification("No se pudo generar el PDF (instala tinytex o pagedown).", type="error", duration=8)
      },
      contentType = "application/pdf"
    )
    output$dl_exec_pdf <- downloadHandler(
      filename = function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")), "_ejecutivo.pdf") },
      content  = function(file){
        ok <- .make_pdf(prior(), acc_fil(), evt(), heat(), file)
        if (!ok) showNotification("No se pudo generar el PDF (instala tinytex o pagedown).", type="error", duration=8)
      },
      contentType = "application/pdf"
    )
    output$dl_acc_fil_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_acciones_filtradas.csv") },
      content =function(file){ readr::write_csv(acc_fil(), file) }
    )
    output$dl_prior_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_prioridades.csv") },
      content =function(file){ readr::write_csv(prior(), file) }
    )
    output$dl_evt_csv <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_eventos.csv") },
      content =function(file){ readr::write_csv(evt(), file) }
    )
    output$dl_heat_png <- downloadHandler(
      filename=function(){ paste0(if (nzchar(input$rep_name)) input$rep_name else "reporte_riesgos", "_heatmap.png") },
      content =function(file){
        h <- heat(); validate(need(nrow(h)>0, "Sin datos de heatmap"))
        p <- ggplot2::ggplot(h, ggplot2::aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
          ggplot2::geom_tile(color="grey90") + ggplot2::scale_fill_identity() +
          ggplot2::labs(x="Probabilidad", y="Impacto") + ggplot2::theme_minimal(base_size = 14)
        ggplot2::ggsave(file, plot=p, width=8, height=5, dpi=150)
      },
      contentType = "image/png"
    )
    
    # Snapshots CSV rápidos al activar check
    observeEvent(input$snap_csv, ignoreInit = TRUE, {
      if (isTRUE(input$snap_csv)) {
        base <- if (nzchar(input$rep_name)) input$rep_name else paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))
        outdir <- normalizePath(file.path(getwd(),"PMP","Riesgos","reportes"), mustWork=FALSE)
        if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
        readr::write_csv(prior(), file.path(outdir, paste0(base,"_prioridades.csv")))
        readr::write_csv(acc(),   file.path(outdir, paste0(base,"_acciones.csv")))
        readr::write_csv(evt(),   file.path(outdir, paste0(base,"_eventos.csv")))
        showNotification(paste("Snapshots CSV guardados en", outdir), type="message", duration=6)
      }
    })
  })
}
# ===================== FIN MODULO PMP =====================

suppressPackageStartupMessages({
  library(shiny); library(shinyjs); library(DT)
  library(DBI); library(RMariaDB); library(dplyr); library(shinyWidgets)
})

# ================== CONEXIÓN Y HELPERS ==================
get_con <- function(){
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host     = Sys.getenv("MYSQL_HOST", "localhost"),
    port     = as.integer(Sys.getenv("MYSQL_PORT", "3306")),
    user     = Sys.getenv("MYSQL_USER", "root"),
    password = Sys.getenv("MYSQL_PASSWORD", ""),
    dbname   = Sys.getenv("MYSQL_DBNAME", "libro"),
    bigint   = "integer"
  )
}
q <- function(con, sql, ...) {
  tryCatch(DBI::dbGetQuery(con, sql, ...),
           error = function(e){ warning(e$message); tibble() })
}
exec <- function(con, sql, ...) {
  tryCatch(DBI::dbExecute(con, sql, ...),
           error = function(e){ warning(e$message); 0 })
}
`%||%` <- function(x,y) if (is.null(x) || length(x)==0 || is.na(x)) y else x

# Helpers de IDs
get_rol_id <- function(con, rol_nombre){
  df <- q(con, "SELECT rol_id FROM rol WHERE nombre=?", params = list(rol_nombre))
  if (nrow(df)) df$rol_id[1] else NA_integer_
}
get_cap_id_by_num <- function(con, cap_num){
  df <- q(con, "SELECT capitulo_id FROM capitulo WHERE numero=?", params = list(cap_num))
  if (nrow(df)) df$capitulo_id[1] else NA_integer_
}
get_sub_id_by_nums <- function(con, cap_num, sub_num){
  df <- q(con, "SELECT sc.subcapitulo_id
                  FROM subcapitulo sc
                  JOIN capitulo c ON c.capitulo_id=sc.capitulo_id
                 WHERE c.numero=? AND sc.numero=?", params = list(cap_num, sub_num))
  if (nrow(df)) df$subcapitulo_id[1] else NA_integer_
}
# Normalizaciones de orden
normalizar_cap <- function(con, capitulo_id){
  exec(con,"DROP TEMPORARY TABLE IF EXISTS tmp_orden_cap")
  exec(con, sprintf("
    CREATE TEMPORARY TABLE tmp_orden_cap AS
    SELECT ca.capitulo_id, ca.autor_id, ca.rol_id,
           ROW_NUMBER() OVER(
             PARTITION BY ca.capitulo_id
             ORDER BY CASE ca.rol_id WHEN 1 THEN 0 ELSE 1 END,
                      (SELECT nombre_completo FROM autor a WHERE a.autor_id=ca.autor_id)
           ) rn
      FROM capitulo_autor ca
     WHERE ca.capitulo_id=%d
  ", as.integer(capitulo_id)))
  exec(con,"
    UPDATE capitulo_autor ca
      JOIN tmp_orden_cap t USING (capitulo_id, autor_id, rol_id)
       SET ca.orden=t.rn
     WHERE ca.capitulo_id=?", params=list(as.integer(capitulo_id)))
}
normalizar_sub <- function(con, subcapitulo_id){
  exec(con,"DROP TEMPORARY TABLE IF EXISTS tmp_orden_sub")
  exec(con, sprintf("
    CREATE TEMPORARY TABLE tmp_orden_sub AS
    SELECT sa.subcapitulo_id, sa.autor_id, sa.rol_id,
           ROW_NUMBER() OVER(
             PARTITION BY sa.subcapitulo_id
             ORDER BY CASE sa.rol_id WHEN 1 THEN 0 ELSE 1 END,
                      (SELECT nombre_completo FROM autor a WHERE a.autor_id=sa.autor_id)
           ) rn
      FROM subcapitulo_autor sa
     WHERE sa.subcapitulo_id=%d
  ", as.integer(subcapitulo_id)))
  exec(con,"
    UPDATE subcapitulo_autor sa
      JOIN tmp_orden_sub t USING (subcapitulo_id, autor_id, rol_id)
       SET sa.orden=t.rn
     WHERE sa.subcapitulo_id=?", params=list(as.integer(subcapitulo_id)))
}

# ================== UI ==================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Libro de Estadística — Proyecto Integrado"),
  
  tabsetPanel(id = "main_tabs",
              # -------- Autores (CRUD) --------
              tabPanel("Autores",
                       fluidRow(
                         column(4,
                                h4("Formulario autor"),
                                pickerInput("autor_pick","Selecciona autor (para editar/eliminar):", choices=NULL,
                                            options=list(`live-search`=TRUE)),
                                actionButton("autor_load","Cargar selección"),
                                hr(),
                                textInput("autor_nombre", "Nombre completo"),
                                textInput("autor_email",  "Correo electrónico"),
                                br(),
                                div(
                                  actionButton("add_autor",  "Crear",           class="btn btn-primary"),
                                  actionButton("edit_autor", "Guardar cambios", class="btn btn-warning"),
                                  actionButton("del_autor",  "Eliminar",        class="btn btn-danger"),
                                  actionButton("clear_autor","Limpiar",         class="btn btn-default")
                                )
                         ),
                         column(8,
                                h4("Listado de autores"),
                                DTOutput("tabla_autores")
                         )
                       )
              ),
              
              # -------- Capítulos (CRUD) --------
              tabPanel("Capítulos",
                       fluidRow(
                         column(4,
                                h4("Formulario capítulo"),
                                pickerInput("cap_pick","Selecciona capítulo (para editar/eliminar):", choices=NULL,
                                            options=list(`live-search`=TRUE)),
                                actionButton("cap_load","Cargar selección"),
                                hr(),
                                numericInput("cap_num", "Número", value = NA, min = 1, step = 1),
                                textInput("cap_titulo", "Título"),
                                br(),
                                div(
                                  actionButton("add_cap",  "Crear",           class="btn btn-primary"),
                                  actionButton("edit_cap", "Guardar cambios", class="btn btn-warning"),
                                  actionButton("del_cap",  "Eliminar",        class="btn btn-danger"),
                                  actionButton("clear_cap","Limpiar",         class="btn btn-default")
                                )
                         ),
                         column(8,
                                h4("Listado de capítulos"),
                                DTOutput("tabla_capitulos")
                         )
                       )
              ),
              
              # -------- Equipo (asignación/edición) --------
              tabPanel("Equipo del capítulo",
                       fluidRow(
                         column(4,
                                h4("Asignaciones"),
                                pickerInput("cap_num_sel","Capítulo:", choices=NULL, options=list(`live-search`=TRUE)),
                                numericInput("sub_num","Subcapítulo (1-3)",value=NA,min=1,max=99,step=1),
                                pickerInput("autor_sel","Autor:",choices=NULL,options=list(`live-search`=TRUE)),
                                pickerInput("rol","Rol:",choices=NULL),
                                numericInput("orden","Orden (opcional)",value=NA,min=1,step=1),
                                div(
                                  actionButton("asignar_cap","Asignar a CAPÍTULO",   class="btn btn-primary"),
                                  actionButton("asignar_sub","Asignar a SUBCAPÍTULO",class="btn btn-secondary")
                                ),
                                hr(), strong("Acciones sobre selección (por autor/rol/orden)"), br(),
                                pickerInput("rol_edit","Nuevo rol",choices=NULL),
                                numericInput("orden_edit","Nuevo orden (opcional)",value=NA,min=1,step=1),
                                div(
                                  actionButton("upd_cap","Actualizar en CAPÍTULO",   class="btn btn-warning"),
                                  actionButton("upd_sub","Actualizar en SUBCAPÍTULO",class="btn btn-warning")
                                ),
                                hr(), strong("Quitar asignación"),
                                div(
                                  actionButton("rm_cap","Quitar de CAPÍTULO",    class="btn btn-danger"),
                                  actionButton("rm_sub","Quitar de SUBCAPÍTULO", class="btn btn-danger")
                                ),
                                hr(), strong("Normalizar orden"),
                                div(
                                  actionButton("norm_cap","Normalizar CAPÍTULO",   class="btn btn-success"),
                                  actionButton("norm_sub","Normalizar SUBCAPÍTULO",class="btn btn-success")
                                ),
                                hr(),
                                div(
                                  downloadButton("dl_cap_csv","Equipo CAPÍTULO (CSV)",   class="btn btn-success"),
                                  downloadButton("dl_sub_csv","Equipo SUBCAPÍTULO (CSV)",class="btn btn-success")
                                )
                         ),
                         column(8,
                                tabsetPanel(
                                  tabPanel("Equipo del capítulo",    DTOutput("tabla_cap")),
                                  tabPanel("Equipo del subcapítulo", DTOutput("tabla_sub"))
                                )
                         )
                       )
              ),
              
              tabPanel("Equipo del subcapítulo",
                       fluidRow(column(12, DTOutput("tabla_sub_solo")))
              ),
              
              # -------- Módulos integrados --------
              tabPanel("Alumnos",         alumnos_ui("alum_mod")),
              tabPanel("PMP",             pmp_ui("pmp")),
              tabPanel("Correos (libre)", correo_libre_ui("corr_libre"))
  )
)

# ================== SERVER ==================
server <- function(input, output, session){
  con <- get_con()
  onStop(function() try(DBI::dbDisconnect(con), silent = TRUE))
  try(exec(con, "SET SESSION group_concat_max_len = 32768"), silent = TRUE)
  
  # ---- Data reactiva base ----
  autores_df    <- reactiveVal(q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo"))
  capitulos_df  <- reactiveVal(q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero"))
  roles_df      <- reactiveVal(q(con, "SELECT rol_id, nombre FROM rol ORDER BY rol_id"))
  
  # ---- Poblar pickers globales ----
  observe({
    auts <- autores_df()
    caps <- capitulos_df()
    roles<- roles_df()
    updatePickerInput(session,"autor_pick",
                      choices = setNames(auts$autor_id, paste0(auts$nombre_completo," <",auts$email,">")))
    updatePickerInput(session,"cap_pick",
                      choices = setNames(caps$capitulo_id, paste0("Cap ",caps$numero," — ",caps$titulo)))
    updatePickerInput(session,"cap_num_sel",
                      choices = setNames(caps$numero, paste0("Cap ",caps$numero," — ",caps$titulo)))
    updatePickerInput(session,"autor_sel",
                      choices = setNames(auts$autor_id, paste0(auts$nombre_completo," <",auts$email,">")))
    updatePickerInput(session,"rol",      choices = roles$nombre)
    updatePickerInput(session,"rol_edit", choices = roles$nombre)
  })
  
  # ================== AUTORES (CRUD) ==================
  output$tabla_autores <- renderDT({
    datatable(autores_df(), selection="single", rownames=FALSE,
              options=list(pageLength=12, scrollX=TRUE))
  })
  observeEvent(input$autor_load, {
    req(input$autor_pick)
    a <- q(con, "SELECT * FROM autor WHERE autor_id=?", params=list(as.integer(input$autor_pick)))
    if (nrow(a)) {
      updateTextInput(session,"autor_nombre", value = a$nombre_completo[1] %||% "")
      updateTextInput(session,"autor_email",  value = a$email[1]            %||% "")
    }
  })
  observeEvent(input$add_autor, {
    req(nzchar(trimws(input$autor_nombre)), nzchar(trimws(input$autor_email)))
    exec(con, "INSERT INTO autor(nombre_completo, email) VALUES(?,?)",
         params = list(trimws(input$autor_nombre), tolower(trimws(input$autor_email))))
    autores_df(q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo"))
    showNotification("✅ Autor creado", type="message")
  })
  observeEvent(input$edit_autor, {
    req(input$autor_pick)
    exec(con, "UPDATE autor SET nombre_completo=?, email=? WHERE autor_id=?",
         params=list(trimws(input$autor_nombre), tolower(trimws(input$autor_email)), as.integer(input$autor_pick)))
    autores_df(q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo"))
    showNotification("📝 Cambios guardados", type="warning")
  })
  observeEvent(input$del_autor, {
    req(input$autor_pick)
    exec(con, "DELETE FROM autor WHERE autor_id=?", params=list(as.integer(input$autor_pick)))
    autores_df(q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo"))
    updatePickerInput(session,"autor_pick", selected = character(0))
    updateTextInput(session,"autor_nombre",""); updateTextInput(session,"autor_email","")
    showNotification("🗑️ Autor eliminado", type="message")
  })
  observeEvent(input$clear_autor, {
    updatePickerInput(session,"autor_pick", selected = character(0))
    updateTextInput(session,"autor_nombre",""); updateTextInput(session,"autor_email","")
  })
  
  # ================== CAPÍTULOS (CRUD) ==================
  output$tabla_capitulos <- renderDT({
    datatable(capitulos_df(), selection="single", rownames=FALSE,
              options=list(pageLength=12, scrollX=TRUE))
  })
  observeEvent(input$cap_load, {
    req(input$cap_pick)
    cdf <- q(con, "SELECT * FROM capitulo WHERE capitulo_id=?", params=list(as.integer(input$cap_pick)))
    if (nrow(cdf)) {
      updateNumericInput(session,"cap_num",   value = cdf$numero[1] %||% NA)
      updateTextInput(  session,"cap_titulo", value = cdf$titulo[1] %||% "")
    }
  })
  observeEvent(input$add_cap, {
    req(!is.na(input$cap_num), nzchar(trimws(input$cap_titulo)))
    exec(con, "INSERT INTO capitulo(numero, titulo) VALUES(?,?)",
         params = list(as.integer(input$cap_num), trimws(input$cap_titulo)))
    capitulos_df(q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero"))
    showNotification("✅ Capítulo creado", type="message")
  })
  observeEvent(input$edit_cap, {
    req(input$cap_pick, !is.na(input$cap_num), nzchar(trimws(input$cap_titulo)))
    exec(con, "UPDATE capitulo SET numero=?, titulo=? WHERE capitulo_id=?",
         params = list(as.integer(input$cap_num), trimws(input$cap_titulo), as.integer(input$cap_pick)))
    capitulos_df(q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero"))
    showNotification("📝 Cambios guardados", type="warning")
  })
  observeEvent(input$del_cap, {
    req(input$cap_pick)
    exec(con, "DELETE FROM capitulo WHERE capitulo_id=?", params=list(as.integer(input$cap_pick)))
    capitulos_df(q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero"))
    updatePickerInput(session,"cap_pick", selected = character(0))
    updateNumericInput(session,"cap_num", value = NA); updateTextInput(session,"cap_titulo","")
    showNotification("🗑️ Capítulo eliminado", type="message")
  })
  observeEvent(input$clear_cap, {
    updatePickerInput(session,"cap_pick", selected = character(0))
    updateNumericInput(session,"cap_num", value = NA); updateTextInput(session,"cap_titulo","")
  })
  
  # ================== EQUIPO (asignaciones) ==================
  # Tablas
  cap_df <- reactive({
    req(input$cap_num_sel)
    q(con,"SELECT c.capitulo_id, a.autor_id, r.rol_id,
                  c.numero, c.titulo, a.nombre_completo, a.email, r.nombre AS rol, ca.orden
             FROM capitulo_autor ca
             JOIN capitulo c ON c.capitulo_id=ca.capitulo_id
             JOIN autor a    ON a.autor_id=ca.autor_id
             JOIN rol r      ON r.rol_id=ca.rol_id
            WHERE c.numero=?
            ORDER BY ca.orden, a.nombre_completo", params=list(as.integer(input$cap_num_sel)))
  })
  sub_df <- reactive({
    req(input$cap_num_sel, !is.na(input$sub_num))
    q(con,"SELECT sc.subcapitulo_id, a.autor_id, r.rol_id,
                  c.numero AS cap, sc.numero AS sub, sc.titulo AS sub_titulo,
                  a.nombre_completo, a.email, r.nombre AS rol, sa.orden
             FROM subcapitulo_autor sa
             JOIN subcapitulo sc ON sc.subcapitulo_id=sa.subcapitulo_id
             JOIN capitulo c     ON c.capitulo_id=sc.capitulo_id
             JOIN autor a        ON a.autor_id=sa.autor_id
             JOIN rol r          ON r.rol_id=sa.rol_id
            WHERE c.numero=? AND sc.numero=?
            ORDER BY sa.orden, a.nombre_completo", params=list(as.integer(input$cap_num_sel), as.integer(input$sub_num)))
  })
  output$tabla_cap <- renderDT({
    datatable(cap_df(), selection="single", rownames=FALSE,
              options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2)))))
  })
  output$tabla_sub <- renderDT({
    datatable(sub_df(), selection="single", rownames=FALSE,
              options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2)))))
  })
  output$tabla_sub_solo <- renderDT({
    datatable(sub_df(), selection="single", rownames=FALSE,
              options=list(pageLength=15, columnDefs=list(list(visible=FALSE, targets=c(0,1,2)))))
  })
  
  # Asignar a CAPÍTULO
  observeEvent(input$asignar_cap, {
    req(input$cap_num_sel, input$autor_sel, input$rol)
    cap_id <- get_cap_id_by_num(con, as.integer(input$cap_num_sel))
    rolid  <- get_rol_id(con, input$rol)
    req(!is.na(cap_id), !is.na(rolid))
    exec(con, "INSERT IGNORE INTO capitulo_autor(capitulo_id, autor_id, rol_id, orden) VALUES(?,?,?,?)",
         params=list(cap_id, as.integer(input$autor_sel), rolid, if (is.na(input$orden)) NULL else as.integer(input$orden)))
    showNotification("✅ Asignado al CAPÍTULO", type="message")
  })
  # Asignar a SUBCAPÍTULO
  observeEvent(input$asignar_sub, {
    req(input$cap_num_sel, !is.na(input$sub_num), input$autor_sel, input$rol)
    sub_id <- get_sub_id_by_nums(con, as.integer(input$cap_num_sel), as.integer(input$sub_num))
    rolid  <- get_rol_id(con, input$rol)
    req(!is.na(sub_id), !is.na(rolid))
    exec(con, "INSERT IGNORE INTO subcapitulo_autor(subcapitulo_id, autor_id, rol_id, orden) VALUES(?,?,?,?)",
         params=list(sub_id, as.integer(input$autor_sel), rolid, if (is.na(input$orden)) NULL else as.integer(input$orden)))
    showNotification("✅ Asignado al SUBCAPÍTULO", type="message")
  })
  # Actualizar en CAPÍTULO
  observeEvent(input$upd_cap, {
    req(input$cap_num_sel, input$autor_sel)
    cap_id <- get_cap_id_by_num(con, as.integer(input$cap_num_sel))
    req(!is.na(cap_id))
    sets <- c(); params <- list()
    if (!is.null(input$rol_edit) && nzchar(input$rol_edit)) { sets <- c(sets, "rol_id=?"); params <- c(params, get_rol_id(con, input$rol_edit)) }
    if (!is.na(input$orden_edit))                          { sets <- c(sets, "orden=?");   params <- c(params, as.integer(input$orden_edit)) }
    req(length(sets)>0)
    params <- c(params, cap_id, as.integer(input$autor_sel))
    exec(con, sprintf("UPDATE capitulo_autor SET %s WHERE capitulo_id=? AND autor_id=?", paste(sets, collapse=", ")), params = params)
    showNotification("📝 Actualizado en CAPÍTULO", type="warning")
  })
  # Actualizar en SUBCAPÍTULO
  observeEvent(input$upd_sub, {
    req(input$cap_num_sel, !is.na(input$sub_num), input$autor_sel)
    sub_id <- get_sub_id_by_nums(con, as.integer(input$cap_num_sel), as.integer(input$sub_num))
    req(!is.na(sub_id))
    sets <- c(); params <- list()
    if (!is.null(input$rol_edit) && nzchar(input$rol_edit)) { sets <- c(sets, "rol_id=?"); params <- c(params, get_rol_id(con, input$rol_edit)) }
    if (!is.na(input$orden_edit))                          { sets <- c(sets, "orden=?");   params <- c(params, as.integer(input$orden_edit)) }
    req(length(sets)>0)
    params <- c(params, sub_id, as.integer(input$autor_sel))
    exec(con, sprintf("UPDATE subcapitulo_autor SET %s WHERE subcapitulo_id=? AND autor_id=?", paste(sets, collapse=", ")), params = params)
    showNotification("📝 Actualizado en SUBCAPÍTULO", type="warning")
  })
  # Quitar en CAPÍTULO
  observeEvent(input$rm_cap, {
    req(input$cap_num_sel, input$autor_sel)
    cap_id <- get_cap_id_by_num(con, as.integer(input$cap_num_sel))
    req(!is.na(cap_id))
    exec(con, "DELETE FROM capitulo_autor WHERE capitulo_id=? AND autor_id=?", params=list(cap_id, as.integer(input$autor_sel)))
    showNotification("🗑️ Quitado del CAPÍTULO", type="message")
  })
  # Quitar en SUBCAPÍTULO
  observeEvent(input$rm_sub, {
    req(input$cap_num_sel, !is.na(input$sub_num), input$autor_sel)
    sub_id <- get_sub_id_by_nums(con, as.integer(input$cap_num_sel), as.integer(input$sub_num))
    req(!is.na(sub_id))
    exec(con, "DELETE FROM subcapitulo_autor WHERE subcapitulo_id=? AND autor_id=?", params=list(sub_id, as.integer(input$autor_sel)))
    showNotification("🗑️ Quitado del SUBCAPÍTULO", type="message")
  })
  # Normalizaciones
  observeEvent(input$norm_cap, {
    req(input$cap_num_sel)
    cap_id <- get_cap_id_by_num(con, as.integer(input$cap_num_sel)); req(!is.na(cap_id))
    normalizar_cap(con, cap_id); showNotification("✅ Orden normalizado (CAPÍTULO)", type="message")
  })
  observeEvent(input$norm_sub, {
    req(input$cap_num_sel, !is.na(input$sub_num))
    sub_id <- get_sub_id_by_nums(con, as.integer(input$cap_num_sel), as.integer(input$sub_num)); req(!is.na(sub_id))
    normalizar_sub(con, sub_id); showNotification("✅ Orden normalizado (SUBCAPÍTULO)", type="message")
  })
  # Descargas CSV
  output$dl_cap_csv <- downloadHandler(
    filename=function(){ sprintf("equipo_capitulo_cap%s.csv", input$cap_num_sel %||% "NA") },
    content =function(file){ readr::write_csv(cap_df(), file) }
  )
  output$dl_sub_csv <- downloadHandler(
    filename=function(){ sprintf("equipo_subcap_cap%s_sub%s.csv", input$cap_num_sel %||% "NA", input$sub_num %||% "NA") },
    content =function(file){ readr::write_csv(sub_df(), file) }
  )
  
  # ================== ACTIVACIÓN DE MÓDULOS ==================
  correo_libre_server("corr_libre", con)
  alumnos_server("alum_mod", con)
  pmp_server("pmp", con)
}

# ================== LANZAR ==================
shinyApp(ui, server)
