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

# ================== MODULO PMP (UNIFICADO) ==================
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
               bslib::card(bslib::card_header("Resumen por prioridad"), DTOutput(ns("tbl_prior"))),
               br(),
               bslib::card(bslib::card_header("Acciones (semáforo)"),   DTOutput(ns("tbl_acc")))
      ),
      
      tabPanel("Gantt",
               br(),
               bslib::card(bslib::card_header("Cronograma de fases (si existe)"),
                           plotOutput(ns("plot_gantt_fases"), height=380),
                           DTOutput(ns("tbl_fases"))
               )
      ),
      
      tabPanel("Entregables",
               br(),
               bslib::card(bslib::card_header("Entregables (lectura)"),
                           DTOutput(ns("tbl_ent"))
               )
      ),
      
      tabPanel("Exportar",
               br(),
               bslib::card(
                 bslib::card_header("Descargas / Snapshots"),
                 textInput(ns("rep_name"), "Nombre base", value=paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))),
                 checkboxInput(ns("snap_csv"), "Guardar snapshots CSV (prioridades/acciones/eventos)", TRUE),
                 div(class="d-flex gap-3 flex-wrap",
                     downloadButton(ns("dl_md"),  "Descargar .md"),
                     downloadButton(ns("dl_pdf"), "Descargar PDF")
                 ),
                 br(),
                 helpText("Solo lectura: se basan en las vistas v_riesgo_registro / v_riesgo_acciones / v_riesgo_heatmap y tabla riesgo_evento.")
               )
      )
    )
  )
}

# ================== UI ==================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Libro de Estadística — Autores & Alumnos"),
  tags$small("Variables: MYSQL_HOST, MYSQL_PORT, MYSQL_USER, MYSQL_PASSWORD, MYSQL_DBNAME"),
  br(),
  tags$style(HTML("
    #help-box { position: fixed; right: 18px; bottom: 72px; width: 420px; z-index: 9999; }
    @media (max-width: 992px){ #help-box { width: 90%; left: 5%; right: auto; } }
    .help-fab { position: fixed; right: 18px; bottom: 18px; width: 44px; height: 44px; border-radius: 50%;
                font-weight: 700; font-size: 18px; line-height: 26px; z-index: 10000; }
  ")),
  actionButton("help_toggle", "?", class = "btn-info help-fab", title = "Mostrar/ocultar ayuda"),
  shinyjs::hidden(div(id = "help-box", uiOutput("help_box_ui"))),
  
  tabsetPanel(id = "main_tabs",
              tabPanel("Equipo del capítulo",
                       fluidRow(
                         column(4,
                                h4("Selección"),
                                pickerInput("cap_num","Capítulo:",choices=NULL,options=list(`live-search`=TRUE)),
                                numericInput("sub_num","Subcapítulo (1-3)",value=NA,min=1,max=3,step=1),
                                pickerInput("autor","Autor:",choices=NULL,options=list(`live-search`=TRUE)),
                                pickerInput("rol","Rol:",choices=c("Autor principal","Coautor","Revisor","Coordinador")),
                                numericInput("orden","Orden (opcional)",value=NA,min=1,step=1),
                                actionButton("add_cap","Asignar a CAPÍTULO",class="btn-primary"),
                                actionButton("add_sub","Asignar a SUBCAPÍTULO",class="btn-secondary"),
                                br(), br(),
                                downloadButton("dl_cap_csv","Equipo del CAPÍTULO (CSV)",class="btn-success"),
                                downloadButton("dl_sub_csv","Equipo del SUBCAPÍTULO (CSV)",class="btn-success"),
                                downloadButton("dl_full_csv","Todas las asignaciones (CSV)",class="btn-info"),
                                hr(), strong("Acciones sobre selección"), br(),
                                actionButton("rm_cap","Quitar de CAPÍTULO",class="btn-danger"),
                                actionButton("rm_sub","Quitar de SUBCAPÍTULO",class="btn-danger"), br(), br(),
                                selectInput("rol_edit","Nuevo rol",c("Autor principal","Coautor","Revisor","Coordinador")),
                                numericInput("orden_edit","Nuevo orden (opcional)",value=NA,min=1,step=1),
                                actionButton("upd_cap","Actualizar en CAPÍTULO",class="btn-warning"),
                                actionButton("upd_sub","Actualizar en SUBCAPÍTULO",class="btn-warning"), br(), br(),
                                actionButton("norm_cap","Normalizar ORDEN del CAPÍTULO",class="btn-success"),
                                actionButton("norm_sub","Normalizar ORDEN del SUBCAPÍTULO",class="btn-success")
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
              
              tabPanel("Autores",
                       fluidRow(
                         column(5,
                                h4("Gestionar autor"),
                                pickerInput("autor_pick","Escoger autor (para cargar/editar):",choices=NULL,options=list(`live-search`=TRUE)),
                                actionButton("autor_load","Cargar selección",class="btn-default"),
                                br(), br(),
                                textInput("a_nombre","Nombre completo"),
                                textInput("a_email","Correo electrónico"),
                                textInput("a_pais_ciudad","País y ciudad"),
                                textInput("a_institucion","Institución de afiliación"),
                                textInput("a_cargo","Cargo académico"),
                                textInput("a_orcid","ORCID ID (URL o código)"),
                                textInput("a_telefono","Teléfono / WhatsApp"),
                                fileInput("a_foto","Foto (opcional)",accept=c("image/png","image/jpg","image/jpeg")), br(),
                                fluidRow(
                                  column(3, actionButton("autor_new","Crear",class="btn-primary",width="100%")),
                                  column(3, actionButton("autor_save","Guardar",class="btn-warning",width="100%")),
                                  column(3, actionButton("autor_del","Eliminar",class="btn-danger",width="100%")),
                                  column(3, actionButton("autor_clear","Limpiar",class="btn-default",width="100%"))
                                )
                         ),
                         column(7, h4("Autores"), DTOutput("tabla_autores"))
                       )
              ),
              
              tabPanel("Alumnos", alumnos_ui("alum_mod")),
              
              tabPanel("Reportes",
                       fluidRow(
                         column(3,
                                h4("Filtros"),
                                pickerInput("r_caps","Capítulos",choices=NULL,multiple=TRUE,options=list(`live-search`=TRUE,`actions-box`=TRUE)),
                                pickerInput("r_roles","Roles",choices=NULL,multiple=TRUE,options=list(`actions-box`=TRUE)),
                                dateRangeInput("r_fechas","Rango de fechas (Fases)",start=NA,end=NA,format="yyyy-mm-dd"),
                                br(), strong("Descargas CSV"), br(),
                                downloadButton("r_dl_aut_cap_csv","Autores por capítulo",class="btn-success"),
                                downloadButton("r_dl_aut_caprol_csv","Autores por capítulo/rol",class="btn-success"),
                                downloadButton("r_dl_avance_csv","Avance por capítulo",class="btn-success"),
                                downloadButton("r_dl_sub_csv","Subcapítulos (muestra)",class="btn-success")
                         ),
                         column(9,
                                tabsetPanel(id="report_tabs",
                                            tabPanel("Autores por capítulo", br(),
                                                     plotOutput("r_plot_aut_cap",height=350),
                                                     downloadButton("r_dl_aut_cap_png","PNG",class="btn-primary"),
                                                     DTOutput("r_tbl_aut_cap")
                                            ),
                                            tabPanel("Autores por rol (apilado)", br(),
                                                     plotOutput("r_plot_aut_cap_rol",height=350),
                                                     downloadButton("r_dl_aut_caprol_png","PNG",class="btn-primary"),
                                                     DTOutput("r_tbl_aut_caprol")
                                            ),
                                            tabPanel("Avance de capítulos", br(),
                                                     plotOutput("r_plot_avance",height=350),
                                                     downloadButton("r_dl_avance_png","PNG",class="btn-primary"),
                                                     DTOutput("r_tbl_avance")
                                            ),
                                            tabPanel("Gantt de fases", br(),
                                                     plotOutput("r_plot_gantt",height=350),
                                                     downloadButton("r_dl_gantt_png","PNG",class="btn-primary"),
                                                     DTOutput("r_tbl_fases")
                                            ),
                                            tabPanel("Subcapítulos (muestra)", br(),
                                                     DTOutput("r_tbl_sub_preview")
                                            ),
                                            tabPanel("Capítulo + autores (PDF)", br(),
                                                     div(class="d-flex gap-2 flex-wrap",
                                                         downloadButton("cap_aut_pdf","📄 Descargar PDF",class="btn-danger"),
                                                         downloadButton("cap_aut_csv","⬇️ CSV",class="btn-success")
                                                     ),
                                                     br(), DTOutput("cap_aut_tbl")
                                            )
                                )
                         )
                       )
              ),
              
              # === PESTAÑA INDEPENDIENTE PARA PMP (UNIFICADO) ===
              tabPanel("PMP", pmp_ui("pmp")),
              
              tabPanel("Correos (libre)", correo_libre_ui("corr_libre"))
  )
)

# ================== SERVER ==================
server <- function(input, output, session){
  con <- get_con()
  onStop(function() try(DBI::dbDisconnect(con), silent=TRUE))
  try(exec(con, "SET SESSION group_concat_max_len = 32768"), silent = TRUE)
  
  # --------- Datos base autores/capitulos/roles ---------
  capitulos <- q(con,"SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero")
  autores   <- q(con,"SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
  roles_cat <- q(con,"SELECT rol_id, nombre FROM rol ORDER BY rol_id")
  
  updatePickerInput(session,"cap_num", choices=setNames(capitulos$numero, paste0("Cap ",capitulos$numero," — ",capitulos$titulo)))
  updatePickerInput(session,"autor",   choices=setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
  updatePickerInput(session,"autor_pick", choices=setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
  updatePickerInput(session,"r_caps",  choices=setNames(capitulos$capitulo_id, paste0("Cap ",capitulos$numero," — ",capitulos$titulo)), selected=capitulos$capitulo_id)
  updatePickerInput(session,"r_roles", choices=setNames(roles_cat$rol_id, roles_cat$nombre), selected=roles_cat$rol_id)
  
  # --------- Tablas Equipo (capítulos y subcapítulos) ---------
  cap_df <- reactive({
    req(input$cap_num)
    q(con,"SELECT c.capitulo_id, a.autor_id, r.rol_id,
                  c.numero, c.titulo, a.nombre_completo, a.email, r.nombre AS rol, ca.orden
             FROM capitulo_autor ca
             JOIN capitulo c ON c.capitulo_id=ca.capitulo_id
             JOIN autor a    ON a.autor_id=ca.autor_id
             JOIN rol r      ON r.rol_id=ca.rol_id
            WHERE c.numero=?
            ORDER BY ca.orden, a.nombre_completo", params=list(as.integer(input$cap_num)))
  })
  
  sub_df <- reactive({
    req(input$cap_num, !is.na(input$sub_num))
    q(con,"SELECT sc.subcapitulo_id, a.autor_id, r.rol_id,
                  c.numero AS cap, sc.numero AS sub, sc.titulo AS sub_titulo,
                  a.nombre_completo, a.email, r.nombre AS rol, sa.orden
             FROM subcapitulo_autor sa
             JOIN subcapitulo sc ON sc.subcapitulo_id=sa.subcapitulo_id
             JOIN capitulo c     ON c.capitulo_id=sc.capitulo_id
             JOIN autor a        ON a.autor_id=sa.autor_id
             JOIN rol r          ON r.rol_id=sa.rol_id
            WHERE c.numero=? AND sc.numero=?
            ORDER BY sa.orden, a.nombre_completo", params=list(as.integer(input$cap_num), as.integer(input$sub_num)))
  })
  
  output$tabla_cap      <- renderDT({ datatable(cap_df(), selection="single", rownames=FALSE, options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  output$tabla_sub      <- renderDT({ datatable(sub_df(), selection="single", rownames=FALSE, options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  output$tabla_sub_solo <- renderDT({ datatable(sub_df(), selection="single", rownames=FALSE, options=list(pageLength=15, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  
  # --------- Activación de módulos ---------
  correo_libre_server("corr_libre", con)
  alumnos_server("alum_mod", con)
  pmp_server("pmp", con)   # <<< ACTIVACIÓN DEL MÓDULO PMP >>>
}

# Lanzar aplicación
shinyApp(ui, server)
