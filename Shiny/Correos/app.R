# app.R — Envío de correos con subcapítulos (prioriza Subcapitulos_Custom)

# ---- Paquetes ----
library(shiny); library(bslib); library(shinyWidgets); library(DT)
library(readxl); library(janitor); library(dplyr); library(stringr)
library(glue); library(blastula); library(here); library(tidyr)

# ---- Tema/UI base ----
theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Inter"))

`%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a
safe_col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_character_

fmt_date <- function(x){
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  y <- suppressWarnings(as.Date(x))
  if (is.na(y)) "" else format(y, "%Y-%m-%d")
}
fmt_avance <- function(x){
  if (is.null(x) || all(is.na(x))) return(NA_character_)
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(NA_character_)
  if (x <= 1) x <- x * 100
  paste0(round(x), "%")
}
build_sub_lines <- function(tbl){
  if (nrow(tbl) == 0) return("—")
  paste(apply(tbl, 1, function(r){
    partes <- c(r[["titulo_subcapitulo"]] %||% "")
    fi <- fmt_date(r[["fecha_inicio"]]); ff <- fmt_date(r[["fecha_fin"]])
    if (nzchar(fi) || nzchar(ff)) partes <- c(partes, sprintf("(%s — %s)",
                                                              ifelse(nzchar(fi), fi, "?"),
                                                              ifelse(nzchar(ff), ff, "?")))
    av <- fmt_avance(r[["avance"]]); if (!is.na(av)) partes <- c(partes, sprintf("[avance: %s]", av))
    com <- r[["comentarios"]] %||% ""; if (nzchar(trimws(com))) partes <- c(partes, paste0("— ", com))
    paste0("- ", paste(partes, collapse = " "))
  }), collapse = "\n")
}

# ---- Plantilla de correo ----
email_template <- function(row, subs_long) {
  cap    <- row$capitulo %||% row$capitulo_num %||% ""
  titulo <- row$titulo_capitulo %||% ""
  autor  <- row$autor_principal %||% row$principal %||% ""
  coaut  <- row$coautor %||% ""
  
  f1 <- row$fase_1_fin %||% ""; f2 <- row$fase_2_fin %||% ""
  f3 <- row$fase_3_fin %||% ""; f4 <- row$fase_4_fin %||% ""; f5 <- row$fase_5_fin %||% ""
  
  cap_num <- suppressWarnings(as.integer(str_extract(cap, "^[0-9]+")))
  if (is.na(cap_num)) cap_num <- suppressWarnings(as.integer(row$capitulo_num))
  
  sub_tbl <- subs_long %>% filter(capitulo_num == cap_num) %>% arrange(subcapitulo)
  subs_md <- build_sub_lines(sub_tbl)
  
  body_md <- glue("
**Asunto:** Libro de Estadística – Encargo de {cap}: {titulo}

Estimado/a **{autor}**{ifelse(nchar(coaut)>0, glue(' (con {coaut})'), '')},

Gracias por participar en el *Libro de Estadística*. Según el índice maestro, usted está a cargo de:

- **{cap} – {titulo}**
- **Subcapítulos sugeridos:**
{subs_md}

**Hitos por fases (referenciales):**
- Fase 1: Preparación y esquema – *{f1}*
- Fase 2: Redacción del borrador – *{f2}*
- Fase 3: Revisión/estilo – *{f3}*
- Fase 4: Entrega a editorial – *{f4}*
- Fase 5: Diagramación/versión final – *{f5}*

> **Importante:** En esta fase **no se requieren ejercicios**.  
> Solo necesitamos el **desarrollo completo del capítulo**.

Adjunto la **rúbrica** con los criterios de calidad.  
Quedo atento/a a cualquier consulta.

Saludos cordiales,  
**Coordinación editorial**
")
  list(subject = glue("Libro de Estadística – {cap}: {titulo}"), body_md = body_md)
}

# ---- UI ----
ui <- page_navbar(
  theme = theme, title = "Envío de correos – Libro de Estadística",
  
  nav("Cargar & revisar",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("1) Fuente"),
          HTML("Se leerán <code>data/Indice_Autores.xlsx</code> y 
               <code>data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx</code> 
               (hojas <b>Subcapitulos</b> y opcional <b>Subcapitulos_Custom</b>)."),
          actionButton("reload", "🔄 Recargar datos", class="btn btn-primary"),
          br(), br(),
          prettySwitch("adj_rubrica", "Adjuntar rúbrica (docs/Rubrica_Capitulo.pdf)", TRUE, status="info"),
          prettySwitch("cc_coautor",  "CC al coautor si hay correo", TRUE, status="info"),
          prettySwitch("modo_prueba", "Modo prueba (no envía; guarda HTML)", TRUE, status="warning")
        ),
        card(
          card_header("2) Índice – vista previa (con subcapítulos)"),
          DTOutput("tbl_idx")
        )
      )
  ),
  
  nav("Redactar & enviar",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("3) Selección"),
          pickerInput("sel_rows", "Filas a enviar", choices = NULL, multiple = TRUE,
                      options = list(`actions-box`=TRUE))
        ),
        card(
          card_header("4) Previsualización / Log"),
          htmlOutput("preview_info"),
          verbatimTextOutput("send_log")
        )
      ),
      footer = div(
        actionButton("btn_preview", "👁️ Previsualizar correo", class="btn btn-secondary"),
        actionButton("btn_send",    "✉️ Enviar correos", class="btn btn-success ms-2")
      )
  )
)

# ---- SERVER ----
server <- function(input, output, session){
  
  idx <- reactiveVal(NULL)        # índice unido
  subs_all <- reactiveVal(NULL)   # subcapítulos (largo)
  
  load_index <- function(){
    
    # 1) Índice de autores
    ind_path <- here::here("data","Indice_Autores.xlsx")
    validate(need(file.exists(ind_path), "No se encontró data/Indice_Autores.xlsx"))
    df <- read_excel(ind_path, 1) %>% clean_names()
    
    # Homologar nombres posibles
    if ("principal"        %in% names(df)) df$autor_principal  <- df$principal
    if ("principal_correo" %in% names(df)) df$correo_principal <- df$principal_correo
    if ("coautor_correo"   %in% names(df)) df$correo_coautor   <- df$coautor_correo
    
    # 2) Subcapítulos — base + overrides
    cron_path <- here::here("data","Cronograma_Libro_Estadistica_CON_INDICE.xlsx")
    validate(need(file.exists(cron_path),
                  "No se encontró data/Cronograma_Libro_Estadistica_CON_INDICE.xlsx"))
    
    subs_raw <- readxl::read_excel(cron_path, sheet = "Subcapitulos") %>% clean_names()
    subs_custom <- tryCatch(
      readxl::read_excel(cron_path, sheet = "Subcapitulos_Custom") %>% clean_names(),
      error = function(e) NULL
    )
    
    norm_subs <- function(x){
      for(nm in c("fecha_inicio","fecha_fin","avance","comentarios")){
        if (!nm %in% names(x)) x[[nm]] <- NA
      }
      x %>%
        mutate(
          capitulo_num = if ("capitulo_num" %in% names(.)) as.integer(capitulo_num)
          else as.integer(str_extract(subcapitulo, "^[0-9]+")),
          fecha_inicio = suppressWarnings(as.Date(fecha_inicio)),
          fecha_fin    = suppressWarnings(as.Date(fecha_fin)),
          avance       = suppressWarnings(as.numeric(avance))
        ) %>%
        select(capitulo_num, subcapitulo, titulo_subcapitulo,
               fecha_inicio, fecha_fin, avance, comentarios)
    }
    
    subs_raw    <- norm_subs(subs_raw)
    subs_custom <- if (is.null(subs_custom)) NULL else norm_subs(subs_custom)
    
    if (!is.null(subs_custom)) {
      caps_custom  <- unique(subs_custom$capitulo_num)
      subs_long_df <- bind_rows(
        subs_raw    %>% filter(!capitulo_num %in% caps_custom),
        subs_custom %>% arrange(capitulo_num, subcapitulo)
      )
    } else {
      subs_long_df <- subs_raw
    }
    
    subs_all(subs_long_df)
    
    # Resumen por capítulo para la tabla
    subs_summary <- subs_long_df %>%
      group_by(capitulo_num) %>%
      summarise(subcapitulos = paste(titulo_subcapitulo, collapse = "; "),
                .groups = "drop")
    
    # Si falta capitulo_num en índice, derivarlo del texto "Cap X"
    if (!"capitulo_num" %in% names(df)) {
      df$capitulo_num <- suppressWarnings(as.integer(str_extract(df$capitulo, "^[0-9]+")))
    }
    
    df <- df %>% left_join(subs_summary, by = "capitulo_num")
    
    # Asegurar columnas que usaremos
    needed <- c("capitulo","capitulo_num","titulo_capitulo","autor_principal","correo_principal",
                "coautor","correo_coautor","subcapitulos",
                "fase_1_fin","fase_2_fin","fase_3_fin","fase_4_fin","fase_5_fin")
    for(nm in setdiff(needed, names(df))) df[[nm]] <- NA_character_
    
    # Etiquetas selector
    labs <- ifelse(!is.na(df$capitulo) & nzchar(df$capitulo),
                   paste0(df$capitulo, " — ", df$titulo_capitulo),
                   paste0("Cap ", df$capitulo_num, " — ", df$titulo_capitulo))
    
    idx(df)
    updatePickerInput(session, "sel_rows", choices = setNames(seq_len(nrow(df)), labs))
  }
  
  observeEvent(TRUE, load_index(), once = TRUE)
  observeEvent(input$reload, load_index())
  
  output$tbl_idx <- renderDT({
    req(idx()); datatable(idx(), options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
  })
  
  output$preview_info <- renderUI({
    req(idx(), input$sel_rows)
    r  <- as.integer(input$sel_rows)[1]
    df <- idx()[r, , drop = FALSE]
    tpl <- email_template(df, subs_all())
    HTML(glue("<h4>Asunto</h4><p>{tpl$subject}</p><h4>Cuerpo</h4><pre style='white-space:pre-wrap'>{tpl$body_md}</pre>"))
  })
  
  observeEvent(input$btn_send, {
    req(idx(), input$sel_rows)
    n <- length(input$sel_rows)
    showModal(modalDialog(
      title = "Confirmar envío",
      HTML(glue("Se enviarán <b>{n}</b> correo(s).<br>
      Modo prueba: <b>{ifelse(isTRUE(input$modo_prueba),'ACTIVADO (no envía)','DESACTIVADO (envío real)')}</b>.")),
      footer = tagList(modalButton("Cancelar"),
                       actionButton("confirm_send", "Sí, continuar", class = "btn btn-danger"))
    ))
  })
  
  observeEvent(input$confirm_send, {
    removeModal()
    req(idx(), input$sel_rows)
    
    df_all <- idx()
    rows   <- as.integer(input$sel_rows)
    rubrica_pdf <- here::here("docs","Rubrica_Capitulo.pdf")
    log_lines <- c()
    
    for(r in rows){
      row <- df_all[r, , drop = FALSE]
      tpl <- email_template(row, subs_all())
      
      to_principal <- safe_col(row, "correo_principal") %||% ""
      to_coautor   <- if (isTRUE(input$cc_coautor)) (safe_col(row, "correo_coautor") %||% "") else ""
      
      if (nchar(to_principal) == 0){
        log_lines <- c(log_lines, glue("[Fila {r}] SIN correo principal — {row$capitulo %||% row$capitulo_num %||% ''}"))
        next
      }
      
      em <- compose_email(body = md(tpl$body_md))
      if (isTRUE(input$adj_rubrica) && file.exists(rubrica_pdf)) {
        em <- add_attachment(em, file = rubrica_pdf)
      }
      
      if (isTRUE(input$modo_prueba)){
        outdir <- here::here("Shiny","Correos","outbox")
        dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
        outfile <- file.path(outdir, glue("preview_{sprintf('%03d', r)}.html"))
        export_email(em, file = outfile)
        log_lines <- c(log_lines, glue("[PREVIEW] Guardado {outfile}"))
      } else {
        tryCatch({
          smtp_send(
            email   = em,
            from    = "cbsarmiento@utmachala.edu.ec",
            to      = to_principal,
            cc      = if (nchar(to_coautor) > 0) to_coautor else NULL,
            subject = tpl$subject,
            credentials = creds_key("office365")
          )
          log_lines <- c(log_lines, glue("[OK] Enviado a {to_principal} (cc: {to_coautor}) — {row$capitulo %||% row$capitulo_num %||% ''}"))
        }, error = function(e){
          log_lines <- c(log_lines, glue("[ERROR] {row$capitulo %||% row$capitulo_num %||% ''}: {conditionMessage(e)}"))
        })
      }
    }
    
    output$send_log <- renderText(paste(log_lines, collapse = "\n"))
    showNotification("Proceso finalizado. Revisa el log.", type = "message")
  })
}

shinyApp(ui, server)
