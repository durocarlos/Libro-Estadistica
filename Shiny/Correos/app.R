# app.R — Shiny mínimo: carga dinámica de subcapítulos y vista previa por capítulo

library(shiny)
library(bslib)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(DT)
library(here)

# ---------- HELPERS ----------
`%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a

normalize_subs <- function(x){
  req(nrow(x) > 0)
  for(nm in c("fecha_inicio","fecha_fin","avance","comentarios")){
    if (!nm %in% names(x)) x[[nm]] <- NA
  }
  x %>%
    clean_names() %>%
    mutate(
      capitulo_num = coalesce(
        suppressWarnings(as.integer(capitulo_num)),
        suppressWarnings(as.integer(str_extract(subcapitulo, "^[0-9]+")))
      ),
      subcapitulo  = as.character(subcapitulo),
      fecha_inicio = suppressWarnings(as.Date(fecha_inicio)),
      fecha_fin    = suppressWarnings(as.Date(fecha_fin)),
      avance       = suppressWarnings(as.numeric(avance))
    ) %>%
    select(capitulo_num, subcapitulo, titulo_subcapitulo,
           fecha_inicio, fecha_fin, avance, comentarios)
}

default_subs <- function(cap_nums){
  tibble(
    capitulo_num = rep(cap_nums, each = 3),
    subcapitulo = unlist(lapply(cap_nums, function(k) paste0(k, ".", 1:3))),
    titulo_subcapitulo = rep(c("Introducción y contexto teórico",
                               "Desarrollo del contenido principal",
                               "Conclusiones y referencias"),
                             times = length(cap_nums)),
    fecha_inicio = as.Date(NA), fecha_fin = as.Date(NA),
    avance = NA_real_, comentarios = NA_character_
  )
}

fmt_date <- function(x){
  y <- suppressWarnings(as.Date(x))
  ifelse(is.na(y), "", format(y, "%Y-%m-%d"))
}
fmt_avance <- function(x){
  if (is.null(x) || all(is.na(x))) return("")
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", ifelse(x <= 1, paste0(round(x*100),"%"), paste0(round(x), "%")))
}

# ---------- UI ----------
theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Inter"))

ui <- page_navbar(
  theme = theme, title = "Correos (demo) – Subcapítulos dinámicos",
  
  nav("Cargar & revisar",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("1) Fuente de datos"),
          HTML("
<p><b>Se buscan subcapítulos con esta prioridad:</b></p>
<ol style='margin-left:16px'>
<li><code>data/Subcapitulos_Custom.xlsx</code> (hoja 1)</li>
<li>Hoja <code>Subcapitulos_Custom</code> de <code>data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx</code></li>
<li>Hoja <code>Subcapitulos</code> del mismo archivo</li>
<li>Si no existen, se generan 3 estándar</li>
</ol>
<p>El índice se lee de <code>data/Indice_Autores.xlsx</code></p>
"),
          actionButton("reload", "🔄 Recargar", class = "btn btn-primary"),
          br(), br(),
          strong("Origen de subcapítulos detectado:"),
          verbatimTextOutput("origen_txt", placeholder = TRUE)
        ),
        card(
          card_header("2) Índice (vista previa)"),
          DTOutput("tbl_idx")
        )
      )
  ),
  
  nav("Previsualizar",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("3) Selección"),
          uiOutput("pick_row")
        ),
        card(
          card_header("4) Vista previa del correo"),
          htmlOutput("preview_html")
        )
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  idx <- reactiveVal(NULL)
  subs_all <- reactiveVal(NULL)
  origen <- reactiveVal("")
  
  load_data <- function(){
    
    # 1) Índice
    ind_path <- here::here("data","Indice_Autores.xlsx")
    validate(need(file.exists(ind_path),
                  "Falta data/Indice_Autores.xlsx"))
    df <- read_excel(ind_path, 1) %>% clean_names()
    
    if (!"capitulo_num" %in% names(df)) {
      df$capitulo_num <- suppressWarnings(as.integer(str_extract(df$capitulo, "[0-9]+")))
    }
    # normalizar nombres típicos
    if ("principal"        %in% names(df)) df$autor_principal  <- df$principal
    if ("principal_correo" %in% names(df)) df$correo_principal <- df$principal_correo
    if ("coautor_correo"   %in% names(df)) df$correo_coautor   <- df$coautor_correo
    
    # 2) Subcapítulos por prioridad
    src <- "DEFAULT (3 estándar por capítulo)"
    subs <- NULL
    
    custom_xlsx <- here::here("data","Subcapitulos_Custom.xlsx")
    if (file.exists(custom_xlsx)) {
      subs <- tryCatch(read_excel(custom_xlsx, 1), error = function(e) NULL)
      if (!is.null(subs)) src <- "EXCEL: data/Subcapitulos_Custom.xlsx"
    }
    
    if (is.null(subs)) {
      cron_xlsx <- here::here("data","Cronograma_Libro_Estadistica_CON_INDICE.xlsx")
      if (file.exists(cron_xlsx)) {
        subs <- tryCatch(read_excel(cron_xlsx, "Subcapitulos_Custom"), error = function(e) NULL)
        if (!is.null(subs)) src <- "CRONOGRAMA: hoja Subcapitulos_Custom"
      }
    }
    
    if (is.null(subs)) {
      cron_xlsx <- here::here("data","Cronograma_Libro_Estadistica_CON_INDICE.xlsx")
      if (file.exists(cron_xlsx)) {
        subs <- tryCatch(read_excel(cron_xlsx, "Subcapitulos"), error = function(e) NULL)
        if (!is.null(subs)) src <- "CRONOGRAMA: hoja Subcapitulos"
      }
    }
    
    if (!is.null(subs)) {
      subs <- normalize_subs(subs)
    } else {
      caps <- unique(na.omit(df$capitulo_num))
      subs <- default_subs(caps)
    }
    
    origen(src)
    subs_all(subs)
    
    # Resumen a la tabla
    resumen <- subs %>%
      group_by(capitulo_num) %>%
      summarise(subcapitulos = paste(titulo_subcapitulo, collapse = "; "),
                .groups = "drop")
    
    df <- df %>% left_join(resumen, by = "capitulo_num")
    idx(df)
    
    # actualizar selector
    labs <- ifelse(!is.na(df$capitulo) & nzchar(df$capitulo),
                   paste0(df$capitulo, " — ", df$titulo_capitulo),
                   paste0("Cap ", df$capitulo_num, " — ", df$titulo_capitulo))
    updateSelectInput(session, "row_pick", choices = setNames(seq_len(nrow(df)), labs))
  }
  
  observeEvent(TRUE, load_data(), once = TRUE)
  observeEvent(input$reload, load_data())
  
  output$origen_txt <- renderText(origen())
  
  output$tbl_idx <- renderDT({
    req(idx()); datatable(idx(), options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
  })
  
  output$pick_row <- renderUI({
    req(idx())
    selectInput("row_pick", "Capítulo", choices = NULL, width = "100%")
  })
  
  output$preview_html <- renderUI({
    req(idx(), subs_all(), input$row_pick)
    r <- as.integer(input$row_pick)
    row <- idx()[r, , drop = FALSE]
    
    cap_txt <- row$capitulo %||% paste("Cap", row$capitulo_num)
    cap_num <- suppressWarnings(as.integer(str_extract(cap_txt, "[0-9]+")))
    if (is.na(cap_num)) cap_num <- suppressWarnings(as.integer(row$capitulo_num))
    
    titulo <- row$titulo_capitulo %||% ""
    autor  <- row$autor_principal %||% ""
    coaut  <- row$coautor %||% ""
    
    f1 <- row$fase_1_fin %||% ""; f2 <- row$fase_2_fin %||% ""
    f3 <- row$fase_3_fin %||% ""; f4 <- row$fase_4_fin %||% ""; f5 <- row$fase_5_fin %||% ""
    
    sub_tbl <- subs_all() %>% filter(capitulo_num == cap_num) %>% arrange(subcapitulo)
    
    if (nrow(sub_tbl) == 0) {
      subs_md <- "—"
    } else {
      subs_md <- paste(apply(sub_tbl, 1, function(rr){
        parts <- c(rr[["titulo_subcapitulo"]] %||% "")
        fi <- fmt_date(rr[["fecha_inicio"]]); ff <- fmt_date(rr[["fecha_fin"]])
        if (nzchar(fi) || nzchar(ff)) parts <- c(parts, sprintf("(%s — %s)",
                                                                ifelse(nzchar(fi), fi, "?"),
                                                                ifelse(nzchar(ff), ff, "?")))
        av <- fmt_avance(rr[["avance"]]); if (nzchar(av)) parts <- c(parts, paste0("[avance: ", av, "]"))
        com <- rr[["comentarios"]] %||% ""; if (nzchar(trimws(com))) parts <- c(parts, paste0("— ", com))
        paste0("- ", paste(parts, collapse = " "))
      }), collapse = "\n")
    }
    
    subject <- glue("Libro de Estadística – {cap_txt}: {titulo}")
    body <- glue("
**Asunto:** Libro de Estadística – Encargo de {cap_txt}: {titulo}

Estimado/a **{autor}**{ifelse(nchar(coaut)>0, glue(' (con {coaut})'), '')},

- **{cap_txt} – {titulo}**
- **Subcapítulos sugeridos:**
{subs_md}

**Hitos por fases (referenciales):**
- F1: {f1}
- F2: {f2}
- F3: {f3}
- F4: {f4}
- F5: {f5}
")
    
    HTML(glue("<h4>Asunto</h4><p>{subject}</p><h4>Cuerpo</h4><pre style='white-space:pre-wrap'>{body}</pre>"))
  })
}

shinyApp(ui, server)
