# ======================================================
# app.R — Asignador de Autores + Reportes + Correos (libre)
# ======================================================

# --- Credenciales MySQL (edítalas si lo necesitas) ---
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
  library(glue);  library(htmltools); library(blastula)
})

# ---------- Conexión y utilitarios ----------
get_con <- function() {
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
q    <- function(con, sql, ...)  tryCatch(DBI::dbGetQuery(con, sql, ...), error = function(e){ warning(e$message); tibble() })
exec <- function(con, sql, ...) tryCatch(DBI::dbExecute(con, sql, ...),  error = function(e){ warning(e$message); -1 })

rol_id <- function(con, rol_txt){ df <- q(con,"SELECT rol_id FROM rol WHERE nombre=?", params=list(rol_txt)); if (nrow(df)==1) df$rol_id[1] else NA_integer_ }
cap_id <- function(con, num){ df <- q(con,"SELECT capitulo_id FROM capitulo WHERE numero=?", params=list(num)); if (nrow(df)==1) df$capitulo_id[1] else NA_integer_ }
sub_id <- function(con, num_cap, num_sub){
  df <- q(con,"SELECT sc.subcapitulo_id FROM subcapitulo sc JOIN capitulo c ON c.capitulo_id=sc.capitulo_id WHERE c.numero=? AND sc.numero=?", params=list(num_cap,num_sub))
  if (nrow(df)==1) df$subcapitulo_id[1] else NA_integer_
}
tiene_principal_cap <- function(con, cap){ q(con,"SELECT COUNT(*) n FROM capitulo_autor WHERE capitulo_id=? AND rol_id=1", params=list(cap))$n[1] > 0 }
tiene_principal_sub <- function(con, sub){ q(con,"SELECT COUNT(*) n FROM subcapitulo_autor WHERE subcapitulo_id=? AND rol_id=1", params=list(sub))$n[1] > 0 }

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

# ============== MÓDULO: Correos (libre) + Contactos (tabla autor) ==============
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
               pickerInput(
                 ns("contactos"), "Contactos (tabla autor)",
                 choices = NULL, multiple = TRUE,
                 options = list(`live-search`=TRUE, `actions-box`=TRUE)
               ),
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
               fileInput(ns("up_files"), "Subir archivos (adjuntos)", multiple = TRUE,
                         accept = c(
                           "application/pdf","application/msword","application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                           "application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                           "application/vnd.ms-powerpoint","application/vnd.openxmlformats-officedocument.presentationml.presentation",
                           "text/plain","text/csv","image/png","image/jpeg","image/jpg",
                           "application/zip","application/x-zip-compressed","application/x-7z-compressed"
                         )),
               textAreaInput(ns("abs_paths"), "Rutas absolutas a adjuntar (una por línea)", width="100%", height="120px",
                             placeholder="C:\\ruta\\a\\archivo1.pdf\nD:\\otra\\carpeta\\archivo2.xlsx"),
               actionButton(ns("send_btn"), "✉️ Enviar / Generar", class="btn btn-success", width = "100%")
        ),
        column(8,
               textAreaInput(ns("body_md"), "Cuerpo (Markdown)", width="100%", height="360px",
                             "Estimado/a:

Adjunto encontrará la documentación correspondiente.

Saludos cordiales,
Coordinación Libro de Estadística"),
               hr(),
               htmlOutput(ns("preview_html"))
        )
      )
    ),
    bslib::card(
      bslib::card_header("Registro (Log)"),
      DT::DTOutput(ns("tbl_log"))
    )
  )
}

correo_libre_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    app_dir <- normalizePath(getwd())
    outbox  <- file.path(app_dir, "outbox")
    if (!dir.exists(outbox)) dir.create(outbox, recursive = TRUE, showWarnings = FALSE)
    log_file <- file.path(outbox, "sent_log.csv")
    
    `%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a
    parse_emails <- function(x){
      x <- x %||% ""; if (!nzchar(trimws(x))) return(character(0))
      y <- unlist(strsplit(x, "[;\n\r]+")); y <- trimws(y); y <- y[nzchar(y)]
      unique(y)
    }
    paste_emails <- function(v){
      v <- unique(trimws(v)); v <- v[nzchar(v)]
      paste(v, collapse = ";\n")
    }
    is_email <- function(z) grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", z)
    
    append_log <- function(row){
      row$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      row <- row[, c("timestamp","cred_id","from","to","cc","bcc","subject","modo_prueba","resultado","detalle")]
      if (!file.exists(log_file)){
        write.csv(row, log_file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.table(row, log_file, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      }
    }
    
    get_contactos <- reactive({
      tryCatch(
        DBI::dbGetQuery(con, "SELECT nombre_completo, email FROM autor WHERE email IS NOT NULL AND email <> '' ORDER BY nombre_completo"),
        error = function(e){ warning(e$message); data.frame(nombre_completo=character(), email=character()) }
      )
    })
    observe({
      df <- get_contactos()
      ch <- setNames(df$email, paste0(df$nombre_completo, " <", df$email, ">"))
      updatePickerInput(session, "contactos", choices = ch)
    })
    observeEvent(input$add_to_to,  { s <- input$contactos; if(length(s)) updateTextAreaInput(session,"to",  value=paste_emails(c(parse_emails(input$to),  s))) })
    observeEvent(input$add_to_cc,  { s <- input$contactos; if(length(s)) updateTextAreaInput(session,"cc",  value=paste_emails(c(parse_emails(input$cc),  s))) })
    observeEvent(input$add_to_bcc, { s <- input$contactos; if(length(s)) updateTextAreaInput(session,"bcc", value=paste_emails(c(parse_emails(input$bcc), s))) })
    
    cred_user <- reactive({
      id <- input$creds_id %||% ""
      if (!nzchar(id)) return(NA_character_)
      kr <- try(keyring::key_list(id), silent = TRUE)
      if (inherits(kr, "try-error") || nrow(kr)==0) return(NA_character_)
      kr$username[1]
    })
    output$cred_info <- renderText({
      u <- cred_user()
      if (is.na(u)) "✗ Credencial NO encontrada."
      else {
        isolate(if (!nzchar(trimws(input$from_email))) updateTextInput(session,"from_email", value = u))
        paste0("✓ Credencial encontrada: '", input$creds_id, "'\nUsuario: ", u)
      }
    })
    
    output$preview_html <- renderUI({
      subj <- input$subject %||% ""
      body <- input$body_md %||% ""
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
      cred_id <- input$creds_id %||% ""
      cu <- cred_user()
      validate(
        need(nzchar(cred_id), "Indica el ID de credencial."),
        need(!is.na(cu),      "No encuentro esa credencial en el keyring.")
      )
      from_addr <- tolower(trimws(input$from_email %||% ""))
      validate(
        need(nzchar(from_addr), "Debes indicar un remitente."),
        need(identical(from_addr, tolower(cu)), paste0("El remitente debe ser exactamente: ", cu, " (según la credencial)."))
      )
      
      parse_emails <- function(x){
        x <- x %||% ""; if (!nzchar(trimws(x))) return(character(0))
        y <- unlist(strsplit(x, "[;\n\r]+")); y <- trimws(y); y <- y[nzchar(y)]
        unique(y)
      }
      is_email <- function(z) grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", z)
      
      tos  <- parse_emails(input$to);  ccs  <- parse_emails(input$cc);  bccs <- parse_emails(input$bcc)
      validate(need(length(tos) > 0, "Indica al menos un destinatario en 'Para'"))
      bad <- c(tos, ccs, bccs)[!is_email(c(tos, ccs, bccs))]
      validate(need(length(bad)==0, paste("Direcciones inválidas:", paste(bad, collapse=", "))))
      subj <- input$subject %||% ""; validate(need(nzchar(trimws(subj)), "Debes indicar un Asunto"))
      body <- input$body_md %||% ""
      
      em <- blastula::compose_email(
        body   = blastula::md(body),
        footer = blastula::md(glue::glue("_Enviado · {format(Sys.time(), '%Y-%m-%d %H:%M')}._"))
      )
      
      if (!is.null(input$up_files) && nrow(input$up_files) > 0) {
        for (i in seq_len(nrow(input$up_files))) {
          path <- input$up_files$datapath[i]
          if (file.exists(path)) {
            try({ em <- em %>% blastula::add_attachment(file = path, filename = input$up_files$name[i]) }, silent = TRUE)
          }
        }
      }
      abs_lines <- input$abs_paths %||% ""
      if (nzchar(trimws(abs_lines))) {
        ap <- unlist(strsplit(abs_lines, "[\r\n]+"))
        ap <- trimws(ap); ap <- ap[nzchar(ap)]
        for (p in ap) {
          if (file.exists(p)) try({ em <- em %>% blastula::add_attachment(file = p) }, silent = TRUE)
          else showNotification(paste("⚠️ No existe:", p), type = "warning")
        }
      }
      
      log_row <- data.frame(
        cred_id = cred_id,
        from = from_addr,
        to = paste(tos, collapse="; "),
        cc = paste(ccs, collapse="; "),
        bcc = paste(bccs, collapse="; "),
        subject = subj,
        modo_prueba = isTRUE(input$modo_prueba),
        resultado = "", detalle = "", stringsAsFactors = FALSE
      )
      
      if (isTRUE(input$modo_prueba)) {
        fhtml <- file.path(outbox, glue::glue("PREVIEW_{format(Sys.time(), '%Y%m%d_%H%M%S')}.html"))
        ok <- FALSE
        try({
          em %>% blastula::render_email() %>% writeLines(con = fhtml)
          ok <- file.exists(fhtml)
        }, silent = TRUE)
        log_row$resultado <- if (ok) "HTML guardado" else "ERROR"
        log_row$detalle   <- if (ok) basename(fhtml) else "No se pudo escribir HTML"
        append_log(log_row)
        showNotification("📝 Modo prueba: HTML generado en outbox/", type="message")
        return(invisible())
      }
      
      ok <- FALSE; err_msg <- NULL
      tryCatch(
        {
          blastula::smtp_send(
            email       = em,
            from        = from_addr,
            to          = tos,
            cc          = if (length(ccs))  ccs  else NULL,
            bcc         = if (length(bccs)) bccs else NULL,
            subject     = subj,
            credentials = blastula::creds_key(cred_id)
          )
          ok <- TRUE
        },
        error = function(e) { err_msg <<- conditionMessage(e) }
      )
      
      log_row$resultado <- if (ok) "ENVIADO" else "ERROR"
      log_row$detalle   <- err_msg %||% ""
      append_log(log_row)
      
      if (ok) showNotification("✅ Correo enviado", type="message")
      else    showNotification(paste("❌ No se pudo enviar. Detalle:", log_row$detalle), type="error")
    })
  })
}
# ================== FIN MÓDULO ==================

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Asignador de Autores — Libro de Estadística"),
  tags$small("Variables: MYSQL_HOST, MYSQL_PORT, MYSQL_USER, MYSQL_PASSWORD, MYSQL_DBNAME"),
  br(),
  tags$style(HTML("
    #help-box { position: fixed; right: 18px; bottom: 72px; width: 420px; z-index: 9999; }
    @media (max-width: 992px){ #help-box { width: 90%; left: 5%; right: auto; } }
    .help-fab { position: fixed; right: 18px; bottom: 18px; width: 44px; height: 44px; border-radius: 50%;
                font-weight: 700; font-size: 18px; line-height: 26px; z-index: 10000; }
    .author-card { border: 1px solid #e3e3e3; border-radius: 6px; padding: 10px 12px; background: #fafafa; margin-top: 8px; }
    .author-card .name { font-weight: 600; font-size: 14px; } .author-card .meta { color: #666; font-size: 12px; margin-top: 2px; }
    .author-card .line { margin-top: 4px; font-size: 12px; } .author-card code { background: #f1f1f1; }
  ")),
  actionButton("help_toggle", "?", class = "btn-info help-fab", title = "Mostrar/ocultar ayuda"),
  shinyjs::hidden(div(id = "help-box", uiOutput("help_box_ui"))),
  
  tabsetPanel(id = "main_tabs",
              tabPanel("Equipo del capítulo",
                       fluidRow(
                         column(4,
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
                         column(8,
                                tabsetPanel(
                                  tabPanel("Equipo del capítulo",    DTOutput("tabla_cap")),
                                  tabPanel("Equipo del subcapítulo", DTOutput("tabla_sub"))
                                )
                         )
                       )
              ),
              tabPanel("Equipo del subcapítulo", fluidRow(column(12, DTOutput("tabla_sub_solo")))),
              
              # ------------------ AUTORES (CRUD) ------------------
              tabPanel("Autores",
                       fluidRow(
                         column(5,
                                h4("Gestionar autor"),
                                pickerInput("autor_pick", "Escoger autor (para cargar/editar):", choices = NULL, options=list(`live-search`=TRUE)),
                                actionButton("autor_load", "Cargar selección", class="btn-default"),
                                br(), br(),
                                textInput("a_nombre", "Nombre completo"),
                                textInput("a_email", "Correo electrónico"),
                                textInput("a_pais_ciudad", "País y ciudad"),
                                textInput("a_institucion", "Institución de afiliación"),
                                textInput("a_cargo", "Cargo académico"),
                                textInput("a_orcid", "ORCID ID (URL o código)"),
                                textInput("a_telefono", "Teléfono / WhatsApp"),
                                fileInput("a_foto", "Foto (opcional)", accept = c("image/png","image/jpg","image/jpeg")),
                                br(),
                                fluidRow(
                                  column(3, actionButton("autor_new",  "Crear",   class="btn-primary",  width="100%")),
                                  column(3, actionButton("autor_save", "Guardar", class="btn-warning",  width="100%")),
                                  column(3, actionButton("autor_del",  "Eliminar",class="btn-danger",   width="100%")),
                                  column(3, actionButton("autor_clear","Limpiar", class="btn-default",  width="100%"))
                                )
                         ),
                         column(7, h4("Autores"), DTOutput("tabla_autores"))
                       )
              ),
              
              # ------------------ REPORTES ------------------
              tabPanel("Reportes",
                       fluidRow(
                         column(3,
                                h4("Filtros"),
                                pickerInput("r_caps","Capítulos", choices=NULL, multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),
                                pickerInput("r_roles","Roles",     choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE)),
                                dateRangeInput("r_fechas","Rango de fechas (Fases)", start=NA, end=NA, format="yyyy-mm-dd"),
                                br(), strong("Descargas CSV"), br(),
                                downloadButton("r_dl_aut_cap_csv","Autores por capítulo",class="btn-success"),
                                downloadButton("r_dl_aut_caprol_csv","Autores por capítulo/rol",class="btn-success"),
                                downloadButton("r_dl_avance_csv","Avance por capítulo",class="btn-success"),
                                downloadButton("r_dl_sub_csv","Subcapítulos (muestra)",class="btn-success")
                         ),
                         column(9,
                                tabsetPanel(
                                  tabPanel("Autores por capítulo", br(), plotOutput("r_plot_aut_cap", height=350),
                                           downloadButton("r_dl_aut_cap_png","PNG",class="btn-primary"), DTOutput("r_tbl_aut_cap")),
                                  tabPanel("Autores por rol (apilado)", br(), plotOutput("r_plot_aut_cap_rol", height=350),
                                           downloadButton("r_dl_aut_caprol_png","PNG",class="btn-primary"), DTOutput("r_tbl_aut_caprol")),
                                  tabPanel("Avance de capítulos", br(), plotOutput("r_plot_avance", height=350),
                                           downloadButton("r_dl_avance_png","PNG",class="btn-primary"), DTOutput("r_tbl_avance")),
                                  tabPanel("Gantt de fases", br(), plotOutput("r_plot_gantt", height=350),
                                           downloadButton("r_dl_gantt_png","PNG",class="btn-primary"), DTOutput("r_tbl_fases")),
                                  tabPanel("Subcapítulos (muestra)", br(), DTOutput("r_tbl_sub_preview"))
                                )
                         )
                       )
              ),
              
              tabPanel("Correos (libre)", correo_libre_ui("corr_libre"))
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  con <- get_con()
  onStop(function() try(DBI::dbDisconnect(con), silent = TRUE))
  
  `%||%` <- function(x,y) if (is.null(x) || length(x)==0 || is.na(x)) y else x
  
  capitulos <- q(con, "SELECT capitulo_id, numero, titulo FROM capitulo ORDER BY numero")
  autores   <- q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
  roles_cat <- q(con, "SELECT rol_id, nombre FROM rol ORDER BY rol_id")
  
  updatePickerInput(session,"cap_num", choices=setNames(capitulos$numero, paste0("Cap ",capitulos$numero," — ",capitulos$titulo)))
  updatePickerInput(session,"autor",   choices=setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
  updatePickerInput(session,"autor_pick", choices=setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
  updatePickerInput(session,"r_caps",  choices=setNames(capitulos$capitulo_id, paste0("Cap ",capitulos$numero," — ",capitulos$titulo)), selected=capitulos$capitulo_id)
  updatePickerInput(session,"r_roles", choices=setNames(roles_cat$rol_id, roles_cat$nombre), selected=roles_cat$rol_id)
  
  # --------- TABLAS EQUIPO ----------
  cap_df <- reactive({
    req(input$cap_num)
    q(con, "SELECT c.capitulo_id, a.autor_id, r.rol_id,
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
    q(con, "SELECT sc.subcapitulo_id, a.autor_id, r.rol_id,
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
  
  output$tabla_cap <- renderDT({ datatable(cap_df(), selection="single", rownames=FALSE, options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  output$tabla_sub <- renderDT({ datatable(sub_df(), selection="single", rownames=FALSE, options=list(pageLength=10, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  output$tabla_sub_solo <- renderDT({ datatable(sub_df(), selection="single", rownames=FALSE, options=list(pageLength=15, columnDefs=list(list(visible=FALSE, targets=c(0,1,2))))) })
  
  # --------- ALTAS/BAJAS/UPDATES ----------
  observeEvent(input$add_cap, {
    v_cap <- cap_id(con, as.integer(input$cap_num)); v_aut <- as.integer(input$autor); v_rol <- rol_id(con, input$rol)
    v_ord <- if (is.na(input$orden)) NULL else as.integer(input$orden)
    validate(need(!is.na(v_cap),"Capítulo no válido"), need(!is.na(v_aut),"Autor no válido"), need(!is.na(v_rol),"Rol no válido"))
    if (v_rol==1 && tiene_principal_cap(con, v_cap)) { showNotification("Ya existe Autor principal en este capítulo", type="error"); return(NULL) }
    exec(con,"INSERT IGNORE INTO capitulo_autor (capitulo_id, autor_id, rol_id, orden) VALUES (?,?,?,?)", params=list(v_cap, v_aut, v_rol, v_ord))
    normalizar_cap(con, v_cap); showNotification("Asignado al capítulo ✔", type="message")
  })
  observeEvent(input$add_sub, {
    v_sub <- sub_id(con, as.integer(input$cap_num), as.integer(input$sub_num)); v_aut <- as.integer(input$autor); v_rol <- rol_id(con, input$rol)
    v_ord <- if (is.na(input$orden)) NULL else as.integer(input$orden)
    validate(need(!is.na(v_sub),"Subcapítulo no válido"), need(!is.na(v_aut),"Autor no válido"), need(!is.na(v_rol),"Rol no válido"))
    if (v_rol==1 && tiene_principal_sub(con, v_sub)) { showNotification("Ya existe Autor principal en este subcapítulo", type="error"); return(NULL) }
    exec(con,"INSERT IGNORE INTO subcapitulo_autor (subcapitulo_id, autor_id, rol_id, orden) VALUES (?,?,?,?)", params=list(v_sub, v_aut, v_rol, v_ord))
    normalizar_sub(con, v_sub); showNotification("Asignado al subcapítulo ✔", type="message")
  })
  
  observeEvent(input$rm_cap, { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df(); exec(con,"DELETE FROM capitulo_autor WHERE capitulo_id=? AND autor_id=? AND rol_id=?", params=list(df$capitulo_id[s], df$autor_id[s], df$rol_id[s])); normalizar_cap(con, df$capitulo_id[s]); showNotification("Eliminado del capítulo ✔", type="message") })
  observeEvent(input$rm_sub, { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df(); exec(con,"DELETE FROM subcapitulo_autor WHERE subcapitulo_id=? AND autor_id=? AND rol_id=?", params=list(df$subcapitulo_id[s], df$autor_id[s], df$rol_id[s])); normalizar_sub(con, df$subcapitulo_id[s]); showNotification("Eliminado del subcapítulo ✔", type="message") })
  
  observeEvent(input$upd_cap, { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df()
  new_rol <- rol_id(con, input$rol_edit); new_ord <- if (is.na(input$orden_edit)) NULL else as.integer(input$orden_edit)
  if (new_rol==1 && tiene_principal_cap(con, df$capitulo_id[s]) && df$rol_id[s]!=1) { showNotification("Ya existe Autor principal en este capítulo", type="error"); return(NULL) }
  exec(con,"UPDATE capitulo_autor SET rol_id=?, orden=? WHERE capitulo_id=? AND autor_id=?", params=list(new_rol, new_ord, df$capitulo_id[s], df$autor_id[s]))
  normalizar_cap(con, df$capitulo_id[s]); showNotification("Actualizado en capítulo ✔", type="warning")
  })
  observeEvent(input$upd_sub, { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df()
  new_rol <- rol_id(con, input$rol_edit); new_ord <- if (is.na(input$orden_edit)) NULL else as.integer(input$orden_edit)
  if (new_rol==1 && tiene_principal_sub(con, df$subcapitulo_id[s]) && df$rol_id[s]!=1) { showNotification("Ya existe Autor principal en este subcapítulo", type="error"); return(NULL) }
  exec(con,"UPDATE subcapitulo_autor SET rol_id=?, orden=? WHERE subcapitulo_id=? AND autor_id=?", params=list(new_rol, new_ord, df$subcapitulo_id[s], df$autor_id[s]))
  normalizar_sub(con, df$subcapitulo_id[s]); showNotification("Actualizado en subcapítulo ✔", type="warning")
  })
  
  mover_cap <- function(con, cap, autor, dir=c("UP","DOWN")){
    dir <- match.arg(dir)
    df <- q(con, "SELECT ca.capitulo_id, ca.autor_id, ca.rol_id, ca.orden,
                         ROW_NUMBER() OVER(PARTITION BY ca.capitulo_id
                         ORDER BY CASE ca.rol_id WHEN 1 THEN 0 ELSE 1 END,
                         (SELECT nombre_completo FROM autor a WHERE a.autor_id=ca.autor_id)) rn
                  FROM capitulo_autor ca WHERE ca.capitulo_id=?", params=list(cap))
    if (!nrow(df)) return(invisible()); row <- df[df$autor_id==autor,]; if (!nrow(row)) return(invisible())
    rn <- row$rn[1]; rol <- row$rol_id[1]; rn_t <- if (dir=="UP") rn-1 else rn+1
    if (rn_t < 1 || rn_t > nrow(df)) return(invisible()); tgt <- df[df$rn==rn_t,]
    if (dir=="UP" && rol!=1 && tgt$rol_id[1]==1) return(invisible())
    exec(con,"UPDATE capitulo_autor SET orden=IFNULL(orden,10000) WHERE capitulo_id=? AND autor_id IN (?,?)", params=list(cap, autor, tgt$autor_id[1])); normalizar_cap(con, cap)
  }
  mover_sub <- function(con, sub, autor, dir=c("UP","DOWN")){
    dir <- match.arg(dir)
    df <- q(con, "SELECT sa.subcapitulo_id, sa.autor_id, sa.rol_id, sa.orden,
                         ROW_NUMBER() OVER(PARTITION BY sa.subcapitulo_id
                         ORDER BY CASE sa.rol_id WHEN 1 THEN 0 ELSE 1 END,
                         (SELECT nombre_completo FROM autor a WHERE a.autor_id=sa.autor_id)) rn
                  FROM subcapitulo_autor sa WHERE sa.subcapitulo_id=?", params=list(sub))
    if (!nrow(df)) return(invisible()); row <- df[df$autor_id==autor,]; if (!nrow(row)) return(invisible())
    rn <- row$rn[1]; rol <- row$rol_id[1]; rn_t <- if (dir=="UP") rn-1 else rn+1
    if (rn_t < 1 || rn_t > nrow(df)) return(invisible()); tgt <- df[df$rn==rn_t,]
    if (dir=="UP" && rol!=1 && tgt$rol_id[1]==1) return(invisible())
    exec(con,"UPDATE subcapitulo_autor SET orden=IFNULL(orden,10000) WHERE subcapitulo_id=? AND autor_id IN (?,?)", params=list(sub, autor, tgt$autor_id[1])); normalizar_sub(con, sub)
  }
  
  observeEvent(input$up_cap,   { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df(); mover_cap(con, df$capitulo_id[s], df$autor_id[s], "UP");   showNotification("Movido ↑ en capítulo", type="message") })
  observeEvent(input$down_cap, { s<-input$tabla_cap_rows_selected; req(length(s)==1); df<-cap_df(); mover_cap(con, df$capitulo_id[s], df$autor_id[s], "DOWN"); showNotification("Movido ↓ en capítulo", type="message") })
  observeEvent(input$up_sub,   { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df(); mover_sub(con, df$subcapitulo_id[s], df$autor_id[s], "UP");   showNotification("Movido ↑ en subcapítulo", type="message") })
  observeEvent(input$down_sub, { s<-input$tabla_sub_rows_selected; req(length(s)==1); df<-sub_df(); mover_sub(con, df$subcapitulo_id[s], df$autor_id[s], "DOWN"); showNotification("Movido ↓ en subcapítulo", type="message") })
  
  observeEvent(input$norm_cap, { req(input$cap_num);  normalizar_cap(con, cap_id(con, as.integer(input$cap_num))); showNotification("Orden normalizado en capítulo ✔", type="message") })
  observeEvent(input$norm_sub, { req(input$cap_num, input$sub_num); normalizar_sub(con, sub_id(con, as.integer(input$cap_num), as.integer(input$sub_num))); showNotification("Orden normalizado en subcapítulo ✔", type="message") })
  
  output$dl_cap_csv <- downloadHandler(filename=function() sprintf("equipo_cap_%s.csv", input$cap_num),
                                       content=function(file) write.csv(cap_df(), file, row.names=FALSE, fileEncoding="UTF-8"))
  output$dl_sub_csv <- downloadHandler(filename=function() sprintf("equipo_cap_%s_sub_%s.csv", input$cap_num, input$sub_num),
                                       content=function(file) write.csv(sub_df(), file, row.names=FALSE, fileEncoding="UTF-8"))
  output$dl_full_csv <- downloadHandler(filename=function() sprintf("asignaciones_%s.csv", format(Sys.Date(), "%Y%m%d")),
                                        content=function(file){
                                          df <- q(con,"SELECT c.numero, c.titulo, a.nombre_completo, a.email, r.nombre AS rol, ca.orden
                   FROM capitulo_autor ca
                   JOIN capitulo c ON c.capitulo_id=ca.capitulo_id
                   JOIN autor a    ON a.autor_id=ca.autor_id
                   JOIN rol r      ON r.rol_id=ca.rol_id
                   ORDER BY c.numero, ca.orden, a.nombre_completo")
                                          write.csv(df, file, row.names=FALSE, fileEncoding="UTF-8")
                                        })
  
  # =========================
  #       CRUD AUTORES
  # =========================
  
  autores_tbl <- reactiveVal({
    q(con, "SELECT autor_id, nombre_completo, email, pais_ciudad, institucion, cargo, orcid, telefono FROM autor ORDER BY nombre_completo")
  })
  output$tabla_autores <- renderDT({
    datatable(autores_tbl(), selection = "single", rownames = FALSE,
              options = list(pageLength=12, order=list(list(1,"asc")),
                             columnDefs=list(list(visible=FALSE, targets=0))))
  })
  
  # ---- Cargar autor robusto (botón y cambio del picker) ----
  load_autor_by_id <- function(id_sel) {
    id_num <- suppressWarnings(as.integer(id_sel))
    if (is.na(id_num)) { showNotification("Selección inválida (id no numérico).", type="error"); return(invisible()) }
    a <- q(con, "SELECT * FROM autor WHERE autor_id=?", params=list(id_num))
    if (!nrow(a)) { showNotification("No se encontró el autor seleccionado.", type="error"); return(invisible()) }
    updateTextInput(session, "a_nombre",       value = a$nombre_completo[1] %||% "")
    updateTextInput(session, "a_email",        value = a$email[1]            %||% "")
    updateTextInput(session, "a_pais_ciudad",  value = a$pais_ciudad[1]      %||% "")
    updateTextInput(session, "a_institucion",  value = a$institucion[1]      %||% "")
    updateTextInput(session, "a_cargo",        value = a$cargo[1]            %||% "")
    updateTextInput(session, "a_orcid",        value = a$orcid[1]            %||% "")
    updateTextInput(session, "a_telefono",     value = a$telefono[1]         %||% "")
    showNotification("Autor cargado ✔", type="message")
  }
  observeEvent(input$autor_load, { req(input$autor_pick); load_autor_by_id(input$autor_pick) }, ignoreInit = TRUE)
  observeEvent(input$autor_pick, {
    if (!is.null(input$autor_pick) && nzchar(input$autor_pick)) load_autor_by_id(input$autor_pick)
  }, ignoreInit = TRUE)
  
  observeEvent(input$autor_clear, {
    updateTextInput(session, "a_nombre",      value = "")
    updateTextInput(session, "a_email",       value = "")
    updateTextInput(session, "a_pais_ciudad", value = "")
    updateTextInput(session, "a_institucion", value = "")
    updateTextInput(session, "a_cargo",       value = "")
    updateTextInput(session, "a_orcid",       value = "")
    updateTextInput(session, "a_telefono",    value = "")
    updatePickerInput(session, "autor_pick", selected = character(0))
  }, ignoreInit = TRUE)
  
  observeEvent(input$autor_new, {
    validate(
      need(nchar(trimws(input$a_nombre))>0, "Nombre requerido"),
      need(nchar(trimws(input$a_email))>0,  "Email requerido")
    )
    foto_blob <- NULL; mime <- NA_character_; fname <- NA_character_; nbytes <- NA_integer_
    if (!is.null(input$a_foto)) {
      f <- input$a_foto
      bin <- readBin(f$datapath, what="raw", n = file.info(f$datapath)$size)
      foto_blob <- list(bin)
      mime   <- f$type
      fname  <- f$name
      nbytes <- length(bin)
    }
    sql <- "INSERT INTO autor
            (nombre_completo, email, pais_ciudad, institucion, cargo, orcid, telefono,
             foto, foto_mime, foto_nombre, foto_bytes, foto_fecha)
            VALUES (?,?,?,?,?,?,?,?,?,?,?, NOW())"
    exec(con, sql,
         params = list(
           input$a_nombre, input$a_email, input$a_pais_ciudad, input$a_institucion,
           input$a_cargo, input$a_orcid, input$a_telefono,
           foto_blob, mime, fname, nbytes
         ))
    autores_tbl(q(con, "SELECT autor_id, nombre_completo, email, pais_ciudad, institucion, cargo, orcid, telefono FROM autor ORDER BY nombre_completo"))
    autores <- q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
    updatePickerInput(session, "autor",      choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    updatePickerInput(session, "autor_pick", choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    showNotification("Autor creado ✔", type="message")
  })
  
  observeEvent(input$autor_save, {
    req(input$autor_pick)
    validate(need(nchar(trimws(input$a_nombre))>0, "Nombre requerido"),
             need(nchar(trimws(input$a_email))>0,  "Email requerido"))
    foto_blob <- NULL; mime <- NULL; fname <- NULL; nbytes <- NULL
    set_foto  <- ""
    if (!is.null(input$a_foto)) {
      f <- input$a_foto
      bin <- readBin(f$datapath, what="raw", n = file.info(f$datapath)$size)
      foto_blob <- list(bin)
      mime   <- f$type
      fname  <- f$name
      nbytes <- length(bin)
      set_foto <- ", foto=?, foto_mime=?, foto_nombre=?, foto_bytes=?, foto_fecha=NOW()"
    }
    sql <- paste0(
      "UPDATE autor SET
         nombre_completo=?, email=?, pais_ciudad=?, institucion=?,
         cargo=?, orcid=?, telefono=?",
      set_foto,
      " WHERE autor_id=?"
    )
    params <- list(
      input$a_nombre, input$a_email, input$a_pais_ciudad, input$a_institucion,
      input$a_cargo, input$a_orcid, input$a_telefono
    )
    if (set_foto!="") params <- c(params, list(foto_blob, mime, fname, nbytes))
    params <- c(params, list(as.integer(input$autor_pick)))
    exec(con, sql, params = params)
    
    autores_tbl(q(con, "SELECT autor_id, nombre_completo, email, pais_ciudad, institucion, cargo, orcid, telefono FROM autor ORDER BY nombre_completo"))
    autores <- q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
    updatePickerInput(session, "autor",      choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    updatePickerInput(session, "autor_pick", choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    showNotification("Autor guardado ✔", type="warning")
  })
  
  observeEvent(input$autor_del, {
    req(input$autor_pick)
    ref1 <- q(con,"SELECT COUNT(*) n FROM capitulo_autor    WHERE autor_id=?", params=list(as.integer(input$autor_pick)))$n[1]
    ref2 <- q(con,"SELECT COUNT(*) n FROM subcapitulo_autor WHERE autor_id=?", params=list(as.integer(input$autor_pick)))$n[1]
    if ((ref1+ref2)>0) {
      showNotification("No se puede eliminar: el autor tiene asignaciones.", type="error"); return(NULL)
    }
    exec(con,"DELETE FROM autor WHERE autor_id=?", params=list(as.integer(input$autor_pick)))
    autores_tbl(q(con, "SELECT autor_id, nombre_completo, email, pais_ciudad, institucion, cargo, orcid, telefono FROM autor ORDER BY nombre_completo"))
    autores <- q(con, "SELECT autor_id, nombre_completo, email FROM autor ORDER BY nombre_completo")
    updatePickerInput(session, "autor",      choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    updatePickerInput(session, "autor_pick", choices = setNames(autores$autor_id, paste0(autores$nombre_completo," <",autores$email,">")))
    showNotification("Autor eliminado ✔", type="message")
  })
  
  # ------------------ REPORTES ------------------
  get_portada <- reactive({
    q(con,"SELECT c.capitulo_id, c.numero AS cap_numero, c.titulo AS cap_titulo,
                 a.autor_id, a.nombre_completo, r.rol_id, r.nombre AS rol, ca.orden
            FROM capitulo c
            JOIN capitulo_autor ca ON ca.capitulo_id=c.capitulo_id
            JOIN autor a ON a.autor_id=ca.autor_id
            JOIN rol r   ON r.rol_id=ca.rol_id
           ORDER BY c.numero, COALESCE(ca.orden,9999), r.nombre;")
  })
  get_avance <- reactive({
    q(con,"SELECT c.capitulo_id, c.numero, c.titulo,
                  COUNT(sc.subcapitulo_id) AS sub_total,
                  SUM(sc.estado_id=5) AS sub_maquetados,
                  SUM(sc.estado_id=4) AS sub_aprobados
             FROM capitulo c
             LEFT JOIN subcapitulo sc ON sc.capitulo_id=c.capitulo_id
            GROUP BY c.capitulo_id, c.numero, c.titulo
            ORDER BY c.numero;")
  })
  get_fases <- reactive({ q(con,"SELECT fase_id, nombre, fecha_inicio, fecha_fin FROM fase ORDER BY fase_id;") })
  get_sub_prev <- reactive({
    q(con,"SELECT c.capitulo_id, c.numero AS cap_numero, sc.numero AS sub_numero,
                  c.titulo AS cap_titulo, sc.titulo AS sub_titulo,
                  a.autor_id, a.nombre_completo, r.nombre AS rol, sca.orden
             FROM subcapitulo sc
             JOIN capitulo c ON c.capitulo_id=sc.capitulo_id
             LEFT JOIN subcapitulo_autor sca ON sca.subcapitulo_id=sc.subcapitulo_id
             LEFT JOIN autor a ON a.autor_id=sca.autor_id
             LEFT JOIN rol r ON r.rol_id=sca.rol_id
            ORDER BY c.numero, sc.numero, COALESCE(sca.orden,9999)
            LIMIT 100;")
  })
  
  portada_fil <- reactive({ df <- get_portada(); if (!nrow(df)) return(df); df %>% filter(capitulo_id %in% input$r_caps, rol_id %in% input$r_roles) })
  avance_fil  <- reactive({ df <- get_avance();  if (!nrow(df)) return(df); df %>% filter(capitulo_id %in% input$r_caps) })
  fases_fil   <- reactive({
    df <- get_fases(); if (!nrow(df)) return(df)
    if (!any(is.na(input$r_fechas))) df <- df %>% filter(as.Date(fecha_fin) >= as.Date(input$r_fechas[1]) & as.Date(fecha_inicio) <= as.Date(input$r_fechas[2]))
    df
  })
  sub_prev_fil <- reactive({ df <- get_sub_prev(); if (!nrow(df)) return(df); df %>% filter(capitulo_id %in% input$r_caps) })
  
  r_aut_cap <- reactive({ portada_fil() %>% group_by(capitulo_id, cap_numero, cap_titulo) %>% summarise(autores=n_distinct(nombre_completo), .groups="drop") %>% arrange(cap_numero) })
  output$r_plot_aut_cap <- renderPlot({ df<-r_aut_cap(); validate(need(nrow(df)>0,"Sin datos")); ggplot(df, aes(x=factor(cap_numero), y=autores)) + geom_col() + labs(title="Autores por capítulo", x="Capítulo", y="N° autores") + theme_minimal() })
  output$r_tbl_aut_cap <- renderDT({ datatable(r_aut_cap(), rownames=FALSE, options=list(pageLength=10)) })
  output$r_dl_aut_cap_png <- downloadHandler(filename=function() "autores_por_capitulo.png",
                                             content=function(file){ df<-r_aut_cap(); validate(need(nrow(df)>0,"Sin datos")); p<-ggplot(df, aes(x=factor(cap_numero), y=autores))+geom_col()+labs(title="Autores por capítulo",x="Capítulo",y="N° autores")+theme_minimal(); ggsave(file,p,width=9,height=5,dpi=150) })
  output$r_dl_aut_cap_csv <- downloadHandler(filename=function() "autores_por_capitulo.csv", content=function(file) write_csv(r_aut_cap(), file))
  
  r_aut_caprol <- reactive({ portada_fil() %>% group_by(capitulo_id, cap_numero, cap_titulo, rol) %>% summarise(autores=n_distinct(nombre_completo), .groups="drop") %>% arrange(cap_numero, rol) })
  output$r_plot_aut_cap_rol <- renderPlot({ df<-r_aut_caprol(); validate(need(nrow(df)>0,"Sin datos")); ggplot(df, aes(x=factor(cap_numero), y=autores, fill=rol))+geom_col(position="stack")+labs(title="Autores por capítulo (apilado por rol)", x="Capítulo", y="N° autores", fill="Rol")+theme_minimal() })
  output$r_tbl_aut_caprol <- renderDT({ datatable(r_aut_caprol(), rownames=FALSE, options=list(pageLength=10)) })
  output$r_dl_aut_caprol_png <- downloadHandler(filename=function() "autores_por_capitulo_por_rol.png",
                                                content=function(file){ df<-r_aut_caprol(); validate(need(nrow(df)>0,"Sin datos")); p<-ggplot(df, aes(x=factor(cap_numero), y=autores, fill=rol))+geom_col(position="stack")+labs(title="Autores por capítulo (apilado por rol)", x="Capítulo", y="N° autores", fill="Rol")+theme_minimal(); ggsave(file,p,width=9,height=5,dpi=150) })
  output$r_dl_aut_caprol_csv <- downloadHandler(filename=function() "autores_por_capitulo_por_rol.csv", content=function(file) write_csv(r_aut_caprol(), file))
  
  r_avance <- reactive({ avance_fil() %>% mutate(pct_aprob=ifelse(sub_total>0, sub_aprobados/sub_total,0), pct_maquet=ifelse(sub_total>0, sub_maquetados/sub_total,0)) %>% arrange(numero) })
  output$r_plot_avance <- renderPlot({ df<-r_avance(); validate(need(nrow(df)>0,"Sin datos")); df_long<-pivot_longer(df, cols=c(pct_aprob,pct_maquet), names_to="tipo", values_to="valor"); ggplot(df_long, aes(x=factor(numero), y=valor, fill=tipo))+geom_col(position="dodge")+scale_y_continuous(labels=percent_format())+labs(title="Avance por capítulo", x="Capítulo", y="% de subcapítulos", fill=NULL)+theme_minimal() })
  output$r_tbl_avance <- renderDT({ datatable(r_avance(), rownames=FALSE, options=list(pageLength=10)) })
  output$r_dl_avance_png <- downloadHandler(filename=function() "avance_por_capitulo.png",
                                            content=function(file){ df<-r_avance(); validate(need(nrow(df)>0,"Sin datos")); df_long<-pivot_longer(df, cols=c(pct_aprob,pct_maquet), names_to="tipo", values_to="valor"); p<-ggplot(df_long, aes(x=factor(numero), y=valor, fill=tipo))+geom_col(position="dodge")+scale_y_continuous(labels=percent_format())+labs(title="Avance por capítulo", x="Capítulo", y="% de subcapítulos", fill=NULL)+theme_minimal(); ggsave(file,p,width=9,height=5,dpi=150) })
  output$r_dl_avance_csv <- downloadHandler(filename=function() "avance_por_capitulo.csv", content=function(file) write_csv(r_avance(), file))
  
  output$r_plot_gantt <- renderPlot({ df<-fases_fil(); validate(need(nrow(df)>0,"Sin datos")); df$nombre<-factor(df$nombre, levels=rev(df$nombre)); ggplot(df, aes(y=nombre))+geom_segment(aes(x=as.Date(fecha_inicio), xend=as.Date(fecha_fin), yend=nombre), linewidth=3)+scale_x_date(date_breaks="1 month", labels=label_date("%Y-%m"))+labs(title="Cronograma de fases", x="Fecha", y=NULL)+theme_minimal() })
  output$r_tbl_fases <- renderDT({ datatable(fases_fil(), rownames=FALSE, options=list(pageLength=12)) })
  output$r_dl_gantt_png <- downloadHandler(filename=function() "fases_gantt.png",
                                           content=function(file){ df<-fases_fil(); validate(need(nrow(df)>0,"Sin datos")); df$nombre<-factor(df$nombre, levels=rev(df$nombre)); p<-ggplot(df, aes(y=nombre))+geom_segment(aes(x=as.Date(fecha_inicio), xend=as.Date(fecha_fin), yend=nombre), linewidth=3)+scale_x_date(date_breaks="1 month", labels=label_date("%Y-%m"))+labs(title="Cronograma de fases", x="Fecha", y=NULL)+theme_minimal(); ggsave(file,p,width=10,height=4.5,dpi=150) })
  
  output$r_tbl_sub_preview <- renderDT({ datatable(sub_prev_fil(), rownames=FALSE, options=list(pageLength=15)) })
  output$r_dl_sub_csv <- downloadHandler(filename=function() "autores_por_subcapitulo_preview.csv", content=function(file) write_csv(sub_prev_fil(), file))
  
  # ayuda contextual
  output$help_box_ui <- renderUI({
    tab <- input$main_tabs; if (is.null(tab)) tab <- "Equipo del capítulo"
    if (tab == "Autores") {
      div(class="well", h4("Leyenda rápida — Autores"),
          tags$ul(
            tags$li(strong("Escoger autor + Cargar selección:")," llena el formulario."),
            tags$li(strong("Crear/Guardar/Eliminar:")," gestiona el registro del autor."),
            tags$li(strong("Foto:")," se almacena como BLOB con su metadata.")
          ))
    } else if (tab == "Reportes") {
      div(class="well", h4("Leyenda rápida — Reportes"),
          tags$ul(
            tags$li(strong("Filtros:")," capítulos, roles y rango de fechas."),
            tags$li(strong("Descargas:")," CSV y PNG por cada subpestaña.")
          ))
    } else {
      div(class="well", h4("Leyenda rápida — Asignaciones"),
          tags$ul(
            tags$li(strong("Asignar/Quitar/Actualizar:")," gestiona el equipo de cada capítulo/subcapítulo."),
            tags$li(strong("Subir/Bajar/Normalizar:")," ordena respetando Autor principal primero.")
          ))
    }
  })
  observeEvent(input$help_toggle, { shinyjs::toggle(id = "help-box", anim = TRUE, time = 0.2) })
  observeEvent(input$main_tabs,   { shinyjs::hide("help-box") }, ignoreInit = TRUE)
  
  # activar módulo de correos
  correo_libre_server("corr_libre", con)
}

shinyApp(ui, server)
