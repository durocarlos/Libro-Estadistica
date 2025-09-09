# -------------------------------------------
# Asignador de Autores — Libro de Estadística
# (Shiny completo con Reportes + Riesgos PMP)
# -------------------------------------------
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(ggplot2)
  library(DBI);  library(RMariaDB); library(pool); library(dplyr)
  library(readr); library(knitr)
})

# ========= Conexión segura a MySQL =========
safe_pool <- function(){
  tryCatch({
    pool::dbPool(
      drv      = RMariaDB::MariaDB(),
      host     = Sys.getenv("MYSQL_HOST", "127.0.0.1"),
      user     = Sys.getenv("MYSQL_USER", "root"),
      password = Sys.getenv("MYSQL_PASSWORD", ""),
      dbname   = Sys.getenv("MYSQL_DBNAME", "libro"),
      port     = as.integer(Sys.getenv("MYSQL_PORT", 3306)),
      bigint   = "numeric"
    )
  }, error = function(e){
    message("POOL ERROR: ", e$message)
    NULL
  })
}

# ============= Helpers reutilizables =============
`%||%` <- function(a,b) if (is.null(a)) b else a
nz_or <- function(x, fallback) if (is.null(x) || !nzchar(x)) fallback else x

q_or_empty <- function(pool, sql){
  if (is.null(pool)) return(data.frame())
  out <- tryCatch(DBI::dbGetQuery(pool, sql),
                  error = function(e){ message("SQL ERROR: ", e$message); data.frame() })
  if (!is.data.frame(out)) data.frame() else out
}

kpi_box <- function(lbl, n, color){
  bslib::value_box(value = format(ifelse(is.na(n), 0, n)),
                   title = lbl, theme_color = color)
}

build_md <- function(prior, acc, evt){
  c(
    paste0("# Reporte de Riesgos - ", format(Sys.Date(), "%Y-%m-%d")),
    "",
    "## Resumen por prioridad",
    "| Prioridad | Total |",
    "|---|---|",
    if (nrow(prior)) apply(prior, 1, function(r) paste0("| ", r[['prioridad']], " | ", r[['total']], " |"))
    else "| (sin datos) | 0 |",
    "",
    "## Acciones (con semáforo)",
    if (nrow(acc)) knitr::kable(acc, format = "pipe") else "_Sin acciones disponibles_",
    "",
    "## Eventos recientes",
    if (nrow(evt)) knitr::kable(evt, format = "pipe") else "_Sin eventos recientes_",
    "",
    "_Generado desde Shiny_"
  )
}

write_report_bundle <- function(dir_path, base_name, prior, acc, evt, snap_csv = TRUE){
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  md_path <- file.path(dir_path, paste0(base_name, ".md"))
  writeLines(build_md(prior, acc, evt), md_path, useBytes = TRUE)
  if (isTRUE(snap_csv)) {
    readr::write_csv(prior, file.path(dir_path, paste0(base_name, "_prioridades.csv")))
    readr::write_csv(acc,   file.path(dir_path, paste0(base_name, "_acciones.csv")))
    readr::write_csv(evt,   file.path(dir_path, paste0(base_name, "_eventos.csv")))
  }
  md_path
}

# ====================== UI ======================
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Asignador de Autores — Libro de Estadística",
  
  navset_card_tab(
    nav_panel("Reportes",
              navset_tab(id = "report_tabs",
                         nav_panel("Autores por capítulo",           p("Placeholder de tu reporte existente.")),
                         nav_panel("Autores por rol (apilado)",      p("Placeholder de tu reporte existente.")),
                         nav_panel("Avance de capítulos",            p("Placeholder de tu reporte existente.")),
                         nav_panel("Gantt de fases",                 p("Placeholder de tu reporte existente.")),
                         nav_panel("Subcapítulos (muestra)",         p("Placeholder de tu reporte existente.")),
                         
                         # =============== NUEVO: RIESGOS PMP ===============
                         nav_panel("Riesgos PMP",
                                   uiOutput("status_conn"),
                                   layout_columns(col_widths = c(4,4,4),
                                                  uiOutput("kpi_extreme"),
                                                  uiOutput("kpi_high"),
                                                  uiOutput("kpi_medium"),
                                                  uiOutput("kpi_low")
                                   ),
                                   hr(),
                                   card(card_header("Acciones con semáforo"), DTOutput("tbl_acciones")),
                                   card(card_header("Heatmap Prob × Impacto"), plotOutput("plot_heatmap", height = 420)),
                                   card(card_header("Eventos recientes"), DTOutput("tbl_eventos")),
                                   card(
                                     card_header("Exportar / Guardar reporte"),
                                     textInput("rep_nombre", "Nombre base",
                                               value = paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d"))),
                                     checkboxInput("snap_csv", "Guardar snapshots CSV (prioridades, acciones, eventos)", TRUE),
                                     checkboxInput("auto_replace", "Reemplazar si ya existe al autogenerar", FALSE),
                                     div(class="d-flex gap-3 flex-wrap",
                                         downloadButton("dl_md", "Descargar .md"),
                                         actionButton("save_md", "Guardar en /PMP/Riesgos/reportes", class = "btn-primary"),
                                         actionButton("auto_today", "Generar hoy (auto)", class = "btn-outline-primary")
                                     ),
                                     helpText("Si faltan vistas/tablas, verás mensajes y la app no se cae.")
                                   )
                         )
              )
    ),
    
    nav_panel("Correos (libre)",
              p("Placeholder de tu módulo de correos.")
    )
  ),
  
  div(class="small text-muted mt-2",
      paste0("Variables: MYSQL_HOST, MYSQL_PORT, MYSQL_USER, MYSQL_PASSWORD, MYSQL_DBNAME"))
)

# ==================== SERVER ====================
server <- function(input, output, session){
  pool <- safe_pool()
  onStop(function(){ if (!is.null(pool)) poolClose(pool) })
  
  # Carpeta de salida
  report_dir <- normalizePath(file.path(getwd(), "PMP", "Riesgos", "reportes"), mustWork = FALSE)
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  
  # Estado de conexión
  output$status_conn <- renderUI({
    if (is.null(pool)) {
      div(class="alert alert-danger",
          "No hay conexión a MySQL. Verifica MYSQL_HOST/USER/PASSWORD/DBNAME/PORT.")
    } else {
      div(class="alert alert-success", "Conexión OK.")
    }
  })
  
  # ---- Consultas (deben existir tus vistas/tablas) ----
  q_prioridad <- "
    SELECT prioridad, COUNT(*) AS total
    FROM v_riesgo_registro
    GROUP BY prioridad"
  q_acciones <- "
    SELECT accion_id, riesgo_id, titulo, responsable, estado, fecha_compromiso, semaforo
    FROM v_riesgo_acciones
    ORDER BY FIELD(semaforo,'Vencida','Por vencer (≤7 días)','En curso','Cerrada'), fecha_compromiso"
  q_heatmap <- "SELECT prob_nivel, imp_nivel, prioridad, color_hex FROM v_riesgo_heatmap"
  q_eventos <- "
    SELECT evento_id, riesgo_id, tipo, detalle, fecha_evento
    FROM riesgo_evento
    ORDER BY evento_id DESC
    LIMIT 20"
  
  # Carga diferida: solo al entrar a "Riesgos PMP"
  active_riesgos <- reactiveVal(FALSE)
  observeEvent(input$report_tabs, {
    active_riesgos(identical(input$report_tabs, "Riesgos PMP"))
  }, ignoreInit = FALSE)
  
  df_prior <- reactive({ req(active_riesgos()); q_or_empty(pool, q_prioridad) })
  df_acc   <- reactive({ req(active_riesgos()); q_or_empty(pool, q_acciones) })
  df_heat  <- reactive({ req(active_riesgos()); q_or_empty(pool, q_heatmap) })
  df_evt   <- reactive({ req(active_riesgos()); q_or_empty(pool, q_eventos) })
  
  # KPIs (con fallback)
  output$kpi_extreme <- renderUI({
    req(active_riesgos())
    n <- df_prior() |> filter(prioridad %in% c("Extreme","Crítica","Critica")) |>
      summarise(n=sum(total, na.rm=TRUE)) |> pull(n)
    kpi_box("Extreme/Crítica", n %||% 0, "#b71c1c")
  })
  output$kpi_high <- renderUI({
    req(active_riesgos())
    n <- df_prior() |> filter(prioridad %in% c("High","Alta")) |>
      summarise(n=sum(total, na.rm=TRUE)) |> pull(n)
    kpi_box("Alta", n %||% 0, "#e65100")
  })
  output$kpi_medium <- renderUI({
    req(active_riesgos())
    n <- df_prior() |> filter(prioridad %in% c("Medium","Media")) |>
      summarise(n=sum(total, na.rm=TRUE)) |> pull(n)
    kpi_box("Media", n %||% 0, "#fdd835")
  })
  output$kpi_low <- renderUI({
    req(active_riesgos())
    n <- df_prior() |> filter(prioridad %in% c("Low","Baja")) |>
      summarise(n=sum(total, na.rm=TRUE)) |> pull(n)
    kpi_box("Baja", n %||% 0, "#4a3047")
  })
  
  # Tablas / Gráfico con mensajes claros
  output$tbl_acciones <- renderDT({
    req(active_riesgos())
    dat <- df_acc()
    validate(need(ncol(dat) > 0, "Sin datos (¿existe la vista v_riesgo_acciones?)."))
    datatable(dat |> mutate(fecha_compromiso = as.Date(fecha_compromiso)),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$plot_heatmap <- renderPlot({
    req(active_riesgos())
    h <- df_heat()
    validate(need(ncol(h) > 0, "Sin datos (¿existe la vista v_riesgo_heatmap?)."))
    ggplot(h, aes(x=factor(prob_nivel), y=factor(imp_nivel), fill=color_hex)) +
      geom_tile(color="grey90") +
      scale_fill_identity() +
      labs(x="Probabilidad", y="Impacto") +
      theme_minimal(base_size = 14)
  })
  
  output$tbl_eventos <- renderDT({
    req(active_riesgos())
    dat <- df_evt()
    validate(need(ncol(dat) > 0, "Sin datos (¿tabla riesgo_evento con registros?)."))
    datatable(dat |> mutate(fecha_evento = as.Date(fecha_evento)),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Descargar .md
  output$dl_md <- downloadHandler(
    filename = function(){ paste0(nz_or(input$rep_nombre, "reporte_riesgos"), ".md") },
    content = function(file){
      writeLines(build_md(df_prior(), df_acc(), df_evt()), file, useBytes = TRUE)
    }
  )
  
  # Guardar manualmente
  observeEvent(input$save_md, {
    base <- nz_or(input$rep_nombre, paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")))
    path <- write_report_bundle(report_dir, base, df_prior(), df_acc(), df_evt(), snap_csv = input$snap_csv)
    showNotification(paste("Reporte guardado en:", path), type = "message", duration = 6)
  })
  
  # Autogeneración (hoy)
  make_today_base <- reactiveVal(paste0("reporte_riesgos_", format(Sys.Date(), "%Y-%m-%d")))
  generate_today <- function(force_replace = FALSE){
    base <- make_today_base()
    md_path <- file.path(report_dir, paste0(base, ".md"))
    if (!file.exists(md_path) || isTRUE(force_replace)) {
      path <- write_report_bundle(report_dir, base, df_prior(), df_acc(), df_evt(), snap_csv = input$snap_csv)
      showNotification(paste("Reporte diario generado en:", path), type = "message", duration = 6)
    } else {
      showNotification("Ya existe el reporte de hoy (no se reemplazó).", type = "message", duration = 5)
    }
  }
  observeEvent(input$auto_today, { generate_today(force_replace = isTRUE(input$auto_replace)) })
  observeEvent(input$report_tabs, {
    if (identical(input$report_tabs, "Riesgos PMP")) {
      invalidateLater(400, session)
      isolate(generate_today(force_replace = isTRUE(input$auto_replace)))
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
