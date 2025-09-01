# app.R — AvanceLibro (lee desde data/)
library(shiny); library(bslib); library(shinyWidgets); library(DT)
library(readxl); library(writexl); library(janitor)
library(dplyr); library(tidyr); library(lubridate); library(stringr)
library(ggplot2); library(plotly); library(scales); library(timevis)
options(shiny.maxRequestSize = 100*1024^2)

`%||%` <- function(a, b) if (is.null(a) || (is.atomic(a) && length(a)==1 && is.na(a))) b else a
coerce_date <- function(x){ out <- suppressWarnings(as.Date(x, origin="1899-12-30")); if (all(is.na(out))) out <- suppressWarnings(as.Date(x)); out }
classify_traffic <- function(p) case_when(is.na(p)~"Sin dato", p<0.34~"Rojo", p<0.67~"Amarillo", p<1~"Verde (en curso)", TRUE~"Verde (completo)")
badge <- function(s){ col <- c("Rojo"="#e74c3c","Amarillo"="#f1c40f","Verde (en curso)"="#2ecc71","Verde (completo)"="#27ae60","Sin dato"="#95a5a6")[s]
sprintf('<span style="background:%s;color:white;padding:2px 8px;border-radius:12px;font-weight:600;">%s</span>', col %||% "#95a5a6", s) }

theme <- bs_theme(bootswatch="flatly", base_font = font_google("Inter"))

# ------- UI -------
ui <- page_navbar(
  title = "Libro de Estadística – Avance (desde /data)",
  theme = theme,
  nav("Carga y filtros",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("1) Fuente de datos"),
          helpText(HTML("Se cargan automáticamente estos archivos:<br>
          <code>data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx</code> (Fases/Capitulos/Subcapitulos)<br>
          <code>data/Indice_Autores.xlsx</code> (opcional).")),
          actionButton("reload", "🔄 Recargar desde /data", class="btn btn-primary"),
          br(), br(),
          pickerInput("fil_autor", "Autor (principal o coautor)", choices=NULL, multiple=TRUE,
                      options=list(`actions-box`=TRUE, title="Filtrar autor")),
          pickerInput("fil_cap", "Capítulo", choices=NULL, multiple=TRUE,
                      options=list(`actions-box`=TRUE, title="Filtrar capítulo")),
          prettySwitch("edit_mode", "Modo edición en tablas", value=FALSE, status="info")
        ),
        card(card_header("2) Resumen de fases"), tableOutput("tbl_fases"))
      )
  ),
  nav("Capítulos",
      card(full_screen=TRUE, card_header("Vista por capítulos"),
           DTOutput("tbl_caps"), br(), downloadButton("dl_caps","Descargar Capitulos (xlsx)")),
      card(card_header("Avance promedio por autor (principal)"), plotlyOutput("plot_caps_autor", height="360px"))
  ),
  nav("Subcapítulos",
      card(full_screen=TRUE, card_header("Detalle de subcapítulos"),
           DTOutput("tbl_sub"), br(), downloadButton("dl_sub","Descargar Subcapitulos (xlsx)")),
      card(card_header("Gantt por capítulo"), timevisOutput("gantt_sub", height="380px"))
  )
)

# ------- SERVER -------
server <- function(input, output, session){
  rv <- reactiveValues(fases=NULL, caps=NULL, sub=NULL, indice=NULL)
  
  load_from_data <- function(){
    cron <- "data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx"
    ind  <- "data/Indice_Autores.xlsx"
    validate(need(file.exists(cron), HTML("No se encontró <code>data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx</code>")))
    fases <- read_excel(cron, sheet="Fases")   %>% clean_names()
    caps  <- read_excel(cron, sheet="Capitulos") %>% clean_names()
    subs  <- read_excel(cron, sheet="Subcapitulos") %>% clean_names()
    if (file.exists(ind)) indice <- tryCatch(read_excel(ind, sheet=1) %>% clean_names(), error=function(e) NULL) else indice <- NULL
    
    for (nm in intersect(names(fases), c("fecha_inicio","fecha_fin"))) fases[[nm]] <- coerce_date(fases[[nm]])
    for (nm in intersect(names(caps),  c("fecha_inicio","fecha_fin")))  caps[[nm]]  <- coerce_date(caps[[nm]])
    for (nm in intersect(names(subs),  c("fecha_inicio","fecha_fin")))  subs[[nm]]  <- coerce_date(subs[[nm]])
    
    if (!"autor_principal" %in% names(caps) && "autor" %in% names(caps)) caps <- rename(caps, autor_principal = autor)
    if (!"coautor" %in% names(caps)) caps$coautor <- NA_character_
    if (!"avance_global" %in% names(caps)) caps$avance_global <- NA_real_
    caps$avance_global <- suppressWarnings(as.numeric(caps$avance_global)); if (max(caps$avance_global, na.rm=TRUE) > 1.5) caps$avance_global <- caps$avance_global/100
    
    if (!"avance" %in% names(subs)) subs$avance <- NA_real_
    subs$avance <- suppressWarnings(as.numeric(subs$avance)); if (max(subs$avance, na.rm=TRUE) > 1.5) subs$avance <- subs$avance/100
    
    rv$fases <- fases; rv$caps <- caps; rv$sub <- subs; rv$indice <- indice
    
    autores <- sort(unique(c(caps$autor_principal, caps$coautor)))
    updatePickerInput(session, "fil_autor", choices = autores)
    updatePickerInput(session, "fil_cap",   choices = sort(unique(caps$capitulo)))
  }
  
  observeEvent(TRUE, { load_from_data() }, once = TRUE)
  observeEvent(input$reload, { load_from_data(); showNotification("Datos recargados desde /data", type="message") })
  
  caps_fil <- reactive({
    df <- rv$caps; req(df)
    if (length(input$fil_autor)) df <- filter(df, autor_principal %in% input$fil_autor | coautor %in% input$fil_autor)
    if (length(input$fil_cap))   df <- filter(df, capitulo %in% input$fil_cap)
    df
  })
  sub_fil <- reactive({
    df <- rv$sub; req(df)
    if (length(input$fil_cap))   df <- filter(df, capitulo %in% input$fil_cap)
    caps <- rv$caps %>% select(capitulo, autor_principal, coautor)
    df <- left_join(df, caps, by="capitulo")
    if (length(input$fil_autor)) df <- filter(df, autor %in% input$fil_autor | autor_principal %in% input$fil_autor | coautor %in% input$fil_autor)
    df
  })
  
  output$tbl_fases <- renderTable({
    req(rv$fases)
    rv$fases %>% transmute(Fase = fase %||% NA, `Fecha inicio` = as.character(fecha_inicio), `Fecha fin` = as.character(fecha_fin))
  }, striped=TRUE, bordered=TRUE, hover=TRUE)
  
  output$tbl_caps <- renderDT({
    df <- caps_fil(); req(df)
    show <- df %>% mutate(`Avance global` = ifelse(!is.na(avance_global), percent(avance_global,1), "—"),
                          `Semáforo` = vapply(classify_traffic(avance_global), badge, FUN.VALUE=character(1))) %>%
      select(capitulo, titulo_capitulo, autor_principal, coautor, fecha_inicio, fecha_fin, `Avance global`, `Semáforo`, comentarios_global)
    datatable(show, escape=FALSE, rownames=FALSE, editable=input$edit_mode,
              options=list(pageLength=12, scrollX=TRUE))
  }, server=TRUE)
  
  output$dl_caps <- downloadHandler(
    filename = function(){ paste0("Capitulos_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx") },
    content  = function(file){ writexl::write_xlsx(caps_fil(), path=file) }
  )
  
  output$tbl_sub <- renderDT({
    df <- sub_fil(); req(df)
    show <- df %>% mutate(`Avance` = ifelse(!is.na(avance), percent(avance,1), "—"),
                          `Semáforo` = vapply(classify_traffic(avance), badge, FUN.VALUE=character(1))) %>%
      select(capitulo, subcapitulo, titulo_subcapitulo, autor, autor_principal, coautor, fecha_inicio, fecha_fin, `Avance`, `Semáforo`, comentarios)
    datatable(show, escape=FALSE, rownames=FALSE, editable=input$edit_mode,
              options=list(pageLength=15, scrollX=TRUE))
  }, server=TRUE)
  
  output$dl_sub <- downloadHandler(
    filename = function(){ paste0("Subcapitulos_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx") },
    content  = function(file){ writexl::write_xlsx(sub_fil(), path=file) }
  )
  
  output$plot_caps_autor <- renderPlotly({
    df <- caps_fil(); req(df)
    agg <- df %>% group_by(autor_principal) %>% summarise(avance_prom = mean(avance_global, na.rm=TRUE), .groups="drop") %>%
      mutate(avance_prom = ifelse(is.nan(avance_prom), NA, avance_prom))
    p <- ggplot(agg, aes(x=reorder(autor_principal, avance_prom), y=avance_prom)) +
      geom_col() + geom_hline(yintercept=c(0.34,0.67,1), linetype=c("dashed","dashed","solid")) +
      scale_y_continuous(labels = percent_format(accuracy=1), limits=c(0,1)) +
      coord_flip() + labs(x="Autor principal", y="Avance promedio", title="Avance promedio por autor principal") +
      theme_minimal(base_size=12)
    ggplotly(p)
  })
  
  output$gantt_sub <- renderTimevis({
    df <- sub_fil(); req(df)
    tv <- df %>% mutate(start = as.Date(fecha_inicio), end = as.Date(fecha_fin),
                        content = paste0(capitulo, " – ", ifelse(is.na(titulo_subcapitulo), subcapitulo, titulo_subcapitulo))) %>%
      filter(!is.na(start)) %>% mutate(id=row_number()) %>% select(id, content, start, end)
    timevis(tv, options=list(stack=TRUE, editable=FALSE, multiselect=TRUE, zoomKey="ctrlKey"))
  })
}

shinyApp(ui, server)
