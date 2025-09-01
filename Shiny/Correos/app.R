# Shiny de Correos — Libro de Estadística
# Envia correos personalizados a autores usando data/Indice_Autores.xlsx
# Adjunta docs/Rubrica_Capitulo.pdf (opcional)
library(shiny); library(bslib); library(shinyWidgets); library(DT)
library(readxl); library(janitor); library(dplyr); library(stringr)
library(glue); library(blastula); library(here)

theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Inter"))

`%||%` <- function(a,b) if (is.null(a) || (is.atomic(a)&&length(a)==1&&is.na(a))) b else a
safe_col <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_character_

email_template <- function(row) {
  cap    <- row$capitulo
  titulo <- row$titulo_capitulo %||% ""
  autor  <- row$autor_principal %||% ""
  coaut  <- row$coautor %||% ""
  
  f1 <- row$fase_1_fin %||% ""; f2 <- row$fase_2_fin %||% ""
  f3 <- row$fase_3_fin %||% ""; f4 <- row$fase_4_fin %||% ""; f5 <- row$fase_5_fin %||% ""
  
  subs <- paste(na.omit(c(row$subcapitulo_1, row$subcapitulo_2, row$subcapitulo_3)), collapse="; ")
  body_md <- glue("
**Asunto:** Libro de Estadística – Encargo de {cap}: {titulo}

Estimado/a **{autor}**{ifelse(nchar(coaut)>0, glue(' (con {coaut})'), '')},

Gracias por participar en el *Libro de Estadística*. Según el índice maestro, usted está a cargo de:

- **{cap} – {titulo}**
- **Subcapítulos sugeridos:** {subs}

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
  list(
    subject = glue("Libro de Estadística – {cap}: {titulo}"),
    body_md = body_md
  )
}

ui <- page_navbar(
  theme = theme, title = "Envío de correos – Libro de Estadística",
  nav("Cargar & revisar",
      layout_columns(
        col_widths = c(4,8),
        card(
          card_header("1) Fuente"),
          HTML("Se leerá <code>data/Indice_Autores.xlsx</code> (nombres flexibles; se limpian con <code>clean_names()</code>)."),
          actionButton("reload", "🔄 Recargar índice", class="btn btn-primary"),
          br(), br(),
          prettySwitch("adj_rubrica", "Adjuntar rúbrica (docs/Rubrica_Capitulo.pdf)", TRUE, status="info"),
          prettySwitch("cc_coautor",  "CC al coautor si hay correo", TRUE, status="info"),
          prettySwitch("modo_prueba", "Modo prueba (no envía, guarda HTML)", TRUE, status="warning")
        ),
        card(
          card_header("2) Índice – vista previa"),
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
                      options = list(`actions-box`=TRUE)),
          helpText("Las filas corresponden a autores/capítulos del índice.")
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

server <- function(input, output, session){
  
  idx <- reactiveVal(NULL)
  
  load_index <- function(){
    path <- here::here("data","Indice_Autores.xlsx")
    validate(need(file.exists(path), "No se encontró data/Indice_Autores.xlsx"))
    df <- read_excel(path, sheet = 1) %>% clean_names()
    
    needed <- c("capitulo","titulo_capitulo","autor_principal","correo_principal",
                "coautor","correo_coautor","subcapitulo_1","subcapitulo_2","subcapitulo_3",
                "fase_1_fin","fase_2_fin","fase_3_fin","fase_4_fin","fase_5_fin")
    for(nm in setdiff(needed, names(df))) df[[nm]] <- NA_character_
    
    idx(df)
    updatePickerInput(session, "sel_rows",
                      choices = setNames(seq_len(nrow(df)), paste0(df$capitulo, " — ", df$titulo_capitulo)))
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
    tpl <- email_template(df)
    HTML(glue("<h4>Asunto</h4><p>{tpl$subject}</p><h4>Cuerpo</h4><pre style='white-space:pre-wrap'>{tpl$body_md}</pre>"))
  })
  
  observeEvent(input$btn_send, {
    req(idx(), input$sel_rows)
    df_all <- idx()
    rows   <- as.integer(input$sel_rows)
    rubrica_pdf <- here::here("docs","Rubrica_Capitulo.pdf")
    log_lines <- c()
    
    for(r in rows){
      row <- df_all[r, , drop = FALSE]
      tpl <- email_template(row)
      
      to_principal <- safe_col(row, "correo_principal") %||% ""
      to_coautor   <- if (isTRUE(input$cc_coautor)) (safe_col(row, "correo_coautor") %||% "") else ""
      
      if (nchar(to_principal) == 0){
        log_lines <- c(log_lines, glue("[Fila {r}] SIN correo principal — {row$capitulo}"))
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
        # Envío real usando la credencial guardada "office365"
        tryCatch({
          smtp_send(
            email   = em,
            from    = "cbsarmiento@utmachala.edu.ec",
            to      = to_principal,
            cc      = if (nchar(to_coautor) > 0) to_coautor else NULL,
            subject = tpl$subject,
            credentials = creds_key("office365")   # << aquí usamos tu credencial
          )
          log_lines <- c(log_lines, glue("[OK] Enviado a {to_principal} (cc: {to_coautor}) — {row$capitulo}"))
        }, error = function(e){
          log_lines <- c(log_lines, glue("[ERROR] {row$capitulo}: {conditionMessage(e)}"))
        })
      }
    }
    
    output$send_log <- renderText(paste(log_lines, collapse = "\n"))
    showNotification("Proceso finalizado. Revisa el log.", type = "message")
  })
}

shinyApp(ui, server)
